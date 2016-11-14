library(dplyr)
library(ggplot2)
library(readxl)
library(data.table)
library(glmnet)
library(tidyr)
library(parallel)
library(foreach)
library(doParallel)

source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/auc.R")
source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/assign_clusters.R")
source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/winpercentages.R")
source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/predict_game.R")
source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/get_surplus_variables.R")
source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/reporting.R")
source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/sim_playoffs.R")
source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/attach_win_perc.R")
source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/manipulate_and_save_output.R")
source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/save_results.R")



## Read the box scores
box_scores <- readRDS("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/cleandata/box_scores.RDA") 

## Get the conferences
conferences <- read.csv("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/rawdata/Conferences.csv", stringsAsFactors = FALSE)


### Create a date-index
datemap <- select(box_scores, DATE, future_game, season) %>%
  distinct(DATE, .keep_all=TRUE) %>%
  arrange(DATE) %>%
  mutate(DATE_INDEX=row_number())

box_scores <- inner_join(box_scores, select(datemap, DATE, DATE_INDEX), by="DATE")

## Get model variables
model_variables <- read.csv("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/modeldetails/model_variables.csv", stringsAsFactors = FALSE)


### Global settings
cutoff <- 8 # minutes per game. if a player plays less than this amount, he is excluded
estimation_window <- 360 # number of days used to estimate the model
winstreak_window <- 91 # number of days used to calculate the weighted win %, for the short term effect
winstreak_window_s <- 31 # number of days used to calculate the weighted win %
playing_time_window <- 91 # number of days used to estimate average playing time
cluster_window <- 91 # number of days used for cluster assignment
alpha <- 0 # for elastic net
sims <- 0 # number of random normal draws used when playing games
ignore_winstreaks <- 0 # if equal to 1, win % are ignored in the model
save_results <- 1 # set to 1 if you want to save the results

### When to start and end the forecasts
start_date <- min(subset(box_scores, season==2015)$DATE)
end_date <- max(subset(box_scores, season==2015 & playoffs==0)$DATE)

### Cut off the box scores
box_scores <- subset(box_scores, DATE<=end_date)

### If we want to trick the model to backcast, edit the future_game indicator by filling in the xs
#box_scores <- mutate(box_scores, future_game = ifelse(DATE>as.Date("xxxxxxx"), 1, future_game))

### specify start and end points
ignore_season_prior_to <- 2014
start_index <- subset(datemap, DATE==start_date)$DATE_INDEX
end_index <- subset(datemap, DATE==end_date)$DATE_INDEX


### Assign clusters to the historical data and calculate rolling win percentages
centroids <- readRDS("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/centroids/centroids.RDA")
s <- min(subset(datemap, season==ignore_season_prior_to)$DATE_INDEX)
e <-max(subset(datemap, future_game==0)$DATE_INDEX) 
ncore <- detectCores()-1
registerDoParallel(ncore)
loop_result <- foreach(i=s:e) %dopar% {

  ### Get the data inside the window  
  thisseason <- datemap[i, "season"]
  inwindow <- filter(box_scores, DATE_INDEX<datemap[i, "DATE_INDEX"] & DATE_INDEX>datemap[i-cluster_window, "DATE_INDEX"])
  thisdate <- filter(box_scores, DATE_INDEX==datemap[i, "DATE_INDEX"])
  thisseason <- thisdate[1,"season"]

  ## Get the win percentages
  win_perc1 <- winpercentages(filter(inwindow, DATE_INDEX>datemap[i-winstreak_window, "DATE_INDEX"]), thisseason)
  win_perc2 <- winpercentages(filter(inwindow, DATE_INDEX>datemap[i-winstreak_window_s, "DATE_INDEX"]), thisseason)
  
  ## Assign clusters
  clusters <- assign_clusters(centroids, inwindow, cutoff, thisseason)

  ### Join
  t <- inner_join(thisdate, select(clusters, PLAYER_FULL_NAME, Cluster), by="PLAYER_FULL_NAME")
  f <- attach_win_perc(t, win_perc1, win_perc2)

  if (ignore_winstreaks==1){
    f$winrate_season_opposing_team <- 0
    f$winrate_season_opposing_team_s <- 0
    f$winrate_early_season_selected_team <- 0
    f$winrate_early_season_opposing_team <- 0
    f$winrate_season_selected_team <- 0
    f$winrate_season_selected_team_s <- 0
  }
  
  rm(win_perc1)
  rm(win_perc2)
  
  return(f)
}

box_scores_plus <- data.frame(rbindlist(loop_result))

## Save clusters
clusters_and_players <- 
  select(box_scores_plus, DATE, PLAYER_FULL_NAME, Cluster, points, assists, offensive_rebounds, defensive_rebounds, turnovers, threepointers_made, threepoint_attempts, steals, minutes, fieldgoal_attempts, fieldgoals_made, freethrow_attempts, freethrows_made, fouls, blocks, season) %>%
  ungroup() %>%
  filter(season==max(season)) %>%
  distinct(PLAYER_FULL_NAME, .keep_all=TRUE) %>%
  arrange(Cluster, PLAYER_FULL_NAME, DATE)

### Number of clusters
nclus <- max(box_scores_plus$Cluster)

### Predict the past and the future
counter <- 1
modelupdates <- 1
index <- 1
scores <- list()
model_details <- list()
model_parts <- list()
max_real_date <- max(subset(box_scores_plus, future_game==0)$DATE_INDEX)
for (i in start_index:end_index){
  
  ### Make sure we only use real data
  j <- min(max_real_date, i)

  ### Check the dates
  print(subset(datemap, DATE_INDEX==i)$DATE)
  print(subset(datemap, DATE_INDEX==j)$DATE)

  ### Data inside the window  
  inwindow <- filter(box_scores_plus, DATE_INDEX<j & DATE_INDEX>j-estimation_window) 
  
  ### Estimate the model unless we have run out of historical data
  if (counter==1 | i <= j){
  
     ### Get game_id level data
     game_data <- distinct(inwindow, game_id, .keep_all=TRUE)
    
     ### Combine the data
     x <- get_surplus_variables(inwindow, nclus)  %>%
       inner_join(game_data, by="game_id")

     ## Estimate the model
     Y <- x$selected_team_win
     x <- x[,names(x) %in% unique(model_variables$Variable)]
     X <- model.matrix(as.formula(Y ~ .), x)
     model <- cv.glmnet(y=Y, x=X, family="binomial", alpha=alpha, parallel=FALSE, nfolds=10)
     c <- as.matrix(coef(model, s=model$lambda.1se))
     p <- prob_win <- 1/(1+exp(-X%*%c[-1]))

     ## Save model details
     details <- cbind.data.frame(sapply(row.names(c), as.character), sapply(c, as.numeric), stringsAsFactors = FALSE)
     names(details) <- c("Variable", "Coeff")
     details$DATE <- datemap[i, "DATE"] 
     details$AUROC <- AUC(Y, p)[[1]]
     details$N <- nrow(X)
     model_details[[modelupdates]] <- subset(details, Variable != "(Intercept)")
     modelupdates <- modelupdates+1
     
     ## Get the latest win percentages
     thisseason <- filter(inwindow, DATE==max(DATE))[1,"season"]
     win_perc1 <- winpercentages(filter(inwindow, DATE_INDEX>j-winstreak_window), thisseason)
     win_perc2 <- winpercentages(filter(inwindow, DATE_INDEX>j-winstreak_window_s), thisseason)

  }
  
  ### Predict game outcomes
  thisday <- filter(box_scores, DATE==datemap[i, "DATE"]) 
  games <- unique(thisday$game_id)
  
  for (d in 1:length(games)){
    pred <- predict_game(c, filter(inwindow, DATE_INDEX>datemap[j-playing_time_window, "DATE_INDEX"]), win_perc1, win_perc2, games[d], sims, subset(thisday, game_id==games[d]), nclus, 0.50, 0.50, "/Users/kimlarsen/Documents/Code/NBA_RANKINGS/rawdata/", model_variables)
    scores[[counter]] <- pred[[1]]
    model_parts[[counter]] <- pred[[2]] 
    counter <- counter + 1
  }
}

### Manipulate the output
results <- manipulate_and_save_output(clusters_and_players, scores, game_level, model_parts, model_details, "/Users/kimlarsen/Documents/Code/NBA_RANKINGS/", 1, 0)



##### Run the playoffs
inwindow <- filter(box_scores_plus, DATE_INDEX<=max_real_date) 
thisseason <- filter(inwindow, DATE==max(DATE))[1,"season"]
win_perc1 <- winpercentages(filter(inwindow, DATE_INDEX>datemap[max_real_date-winstreak_window, "DATE_INDEX"]), thisseason)
win_perc2 <- winpercentages(filter(inwindow, DATE_INDEX>datemap[max_real_date-winstreak_window_s, "DATE_INDEX"]), thisseason)


ncore <- detectCores()-1
registerDoParallel(ncore)
loop_result <- foreach(p=1:100) %dopar% {
   playoffs <- sim_playoff(ranks, inwindow, playing_time_window, win_perc1, win_perc2, datemap, 1, "/Users/kimlarsen/Documents/Code/NBA_RANKINGS", c, max_real_date, thisseason, end_date)
   winner <- subset(playoffs[[1]], status=="W")$team
   return(data.frame(p, winner))
}

title_chances <- data.frame(rbindlist(loop_result)) %>% group_by(winner) %>%
  summarise(n=n()) %>%
  mutate(prob_win_title=n/100) %>%
  select(-n)

  
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
  ungroup() %>%
  distinct(DATE, .keep_all=TRUE) %>%
  arrange(DATE) %>%
  mutate(DATE_INDEX=row_number()) %>%
  group_by(season) %>%
  mutate(season_day=row_number(),
         season_day_std=ifelse(season_day>91, 0, 1-(season_day-1)/90)) %>%
  ungroup()

box_scores <- inner_join(box_scores, select(datemap, DATE, DATE_INDEX, season_day, season_day_std), by="DATE")

## Get model variables
model_variables <- read.csv("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/modeldetails/model_variables.csv", stringsAsFactors = FALSE)


### Global settings
cutoff <- 8 # minutes per game. if a player plays less than this amount, he is excluded
estimation_window <- 1000 # number of days used to estimate the model
winstreak_window <- 91 # number of days used to calculate the weighted win %, for the short term effect
winstreak_window_s <- 31 # number of days used to calculate the weighted win %
playing_time_window <- 91 # number of days used to estimate average playing time
cluster_window <- 91 # number of days used for cluster assignment
alpha <- 0 # for elastic net
sims <- 0 # number of random normal draws used when playing games
save_results <- 1 # set to 1 if you want to save the results
weighted_win_rates <- 1
use_current_rosters <- 0
current_season <- max(box_scores$season)

### When to start and end the forecasts
start_date <- min(subset(box_scores, season==2015)$DATE)
#start_date <- as.Date('2015-11-20')
end_date <- max(subset(box_scores, season==2015 & playoffs==0)$DATE)

### Cut off the box scores
box_scores <- subset(box_scores, DATE<=end_date) %>%
  ungroup() %>%
  mutate(fb=ifelse(season==max(season), 1, 0))


### If we want to trick the model to backcast, edit the future_game indicator by filling in the xs
box_scores <- mutate(box_scores, future_game = ifelse(DATE>=as.Date('2015-11-20'), 1, 0))

### specify start and end points
ignore_season_prior_to <- 2013
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
  inwindow <- filter(box_scores, DATE_INDEX<i & DATE_INDEX>i-cluster_window)
  thisdate <- filter(box_scores, DATE_INDEX==i)
  thisseason <- thisdate[1,"season"]
  print(nrow(inwindow))

  ## Get the win percentages
  win_perc1 <- winpercentages(filter(inwindow, DATE_INDEX>i-winstreak_window), thisseason, 0)
  win_perc2 <- winpercentages(filter(inwindow, DATE_INDEX>i-winstreak_window_s), thisseason, 0)
  
  ## Assign clusters
  clusters <- assign_clusters(centroids, inwindow, cutoff, thisseason)

  ### Join
  t <- inner_join(thisdate, select(clusters, PLAYER_FULL_NAME, Cluster), by="PLAYER_FULL_NAME")
  f <- attach_win_perc(t, win_perc1, win_perc2)
  
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
  
  ## Are current rosters used to pick teams
  cr <- use_current_rosters
  
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
     set.seed(2015)
     model <- cv.glmnet(y=Y, x=X, family="binomial", alpha=alpha, parallel=FALSE, nfolds=10)
     c <- as.matrix(coef(model, s=model$lambda.1se))
     p <- prob_win <- 1/(1+exp(-X%*%c[-1]))

     ## Save model details
     details <- cbind.data.frame(sapply(row.names(c), as.character), sapply(c, as.numeric), stringsAsFactors = FALSE)
     names(details) <- c("Variable", "Coeff")
     details$DATE <- subset(datemap, DATE_INDEX==i)$DATE
     details$AUROC <- AUC(Y, p)[[1]]
     details$N <- nrow(X)
     model_details[[modelupdates]] <- subset(details, Variable != "(Intercept)")
     modelupdates <- modelupdates+1
     
     ## Get the latest win percentages
     thisseason <- filter(inwindow, DATE==max(DATE))[1,"season"]
     w <- weighted_win_rates 
     if (thisseason != current_season){
       w <-0 
       cr <- 0
     }
     if (w==1){
       print("Using weights")
     } else{
       print("Not using weights")
     }
     win_perc1 <- winpercentages(filter(inwindow, DATE_INDEX>j-winstreak_window), thisseason, w)
     win_perc2 <- winpercentages(filter(inwindow, DATE_INDEX>j-winstreak_window_s), thisseason, w)

  }
  
  posterior <- 0.5
  prior <- 0.5
  if (i==max_real_date){
    ytd_scores <- data.frame(rbindlist(scores)) %>% 
      filter(current_season_data_used==1 & is.na(prob_selected_team_win_d)==FALSE & is.na(selected_team_win)==FALSE)
    posterior=mean(ytd_scores$prob_selected_team_win_d)
    prior=mean(ytd_scores$selected_team_win)
    print(posterior)
    print(prior)
    rm(ytd_scores)
  }
  
  
  ### Predict game outcomes
  thisday <- filter(box_scores, DATE_INDEX==i) 
  games <- unique(thisday$game_id)

  for (d in 1:length(games)){
    pred <- predict_game(c, filter(inwindow, DATE_INDEX>j-playing_time_window), win_perc1, win_perc2, games[d], sims, subset(thisday, game_id==games[d]), nclus, prior, posterior, "/Users/kimlarsen/Documents/Code/NBA_RANKINGS/rawdata/", model_variables, cr)
    scores[[counter]] <- pred[[1]]
    model_parts[[counter]] <- pred[[2]] 
    counter <- counter + 1
  }
}

### Manipulate the output
results <- manipulate_and_save_output(clusters_and_players, scores, model_parts, model_details, "/Users/kimlarsen/Documents/Code/NBA_RANKINGS/", 1, 0)

write.csv(select(filter(results[[1]], future_game==1), -current_season_data_used), "/Users/kimlarsen/Documents/Code/NBA_RANKINGS/rankings/game_level_validation_2015.csv")
write.csv(results[[2]], "/Users/kimlarsen/Documents/Code/NBA_RANKINGS/rankings/ranking_validation_2015.csv")

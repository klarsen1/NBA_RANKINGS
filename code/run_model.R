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
source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/get_team_offsets.R")


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

box_scores <- mutate(box_scores, future_game = ifelse(DATE>=as.Date('2017-02-11'), 1, 0))

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
use_current_rosters <- 1
current_season <- max(box_scores$season)
adjust_intercept_by_team <- 0

### When to start and end the forecasts
start_date <- min(subset(box_scores, season==2016)$DATE)
end_date <- max(subset(box_scores, season==2016 & playoffs==0)$DATE)

### Cut off the box scores
box_scores <- subset(box_scores, DATE<=end_date) %>%
  ungroup() %>%
  mutate(fb=ifelse(season==max(season), 1, 0))

### specify start and end points
ignore_season_prior_to <- 2013
start_index <- subset(datemap, DATE==start_date)$DATE_INDEX
end_index <- subset(datemap, DATE==end_date)$DATE_INDEX


### Assign clusters to the historical data and calculate rolling win percentages
centroids <- readRDS("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/centroids/centroids.RDA")
s <- min(subset(datemap, season==ignore_season_prior_to)$DATE_INDEX)
e <-max(subset(datemap, future_game==0)$DATE_INDEX) 
ncore <- detectCores()-2
registerDoParallel(ncore)
loop_result <- foreach(i=s:e) %dopar% {
  #for (i in s:e){
  ### Get the data inside the window  
  thisseason <- datemap[i, "season"]
  inwindow <- filter(box_scores, DATE_INDEX<i & DATE_INDEX>i-cluster_window)
  thisdate <- filter(box_scores, DATE_INDEX==i)
  thisseason <- thisdate[1,"season"]

  ## Get the win percentages
  w <- weighted_win_rates 
  if (thisseason != current_season){
    w <-0 
  }
  win_perc1 <- winpercentages(filter(inwindow, DATE_INDEX>i-winstreak_window), thisseason, w)
  win_perc2 <- winpercentages(filter(inwindow, DATE_INDEX>i-winstreak_window_s), thisseason, w)
  
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
  select(box_scores_plus, DATE, PLAYER_FULL_NAME, Cluster, season) %>%
  ungroup() %>%
  filter(season==max(season)) %>%
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
posterior <- 0.5 ## average probability of winning a game if all is perfect
prior <- 0.5 ## expected average probability of winning a game
cr <- 0 ## current rosters
for (i in start_index:end_index){
  
  ### ELO weight indicator
  w <- weighted_win_rates ## ELO weights
  
  ### Make sure we only use real data
  j <- min(max_real_date, i)
  
  ### Check the dates
  print(subset(datemap, DATE_INDEX==i)$DATE)
  print(subset(datemap, DATE_INDEX==j)$DATE)

  ### Data inside the window  
  inwindow <- filter(box_scores_plus, DATE_INDEX<j & DATE_INDEX>j-estimation_window) 
  
  ### Estimate the model unless we have run out of historical data
  if (counter==1 | i <= max_real_date){
  
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
     p <- 1/(1+exp(-X%*%c[-1]))

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
     if (thisseason != current_season){
       w <-0 
     }
     inwindow <- filter(inwindow, DATE_INDEX>j-max(winstreak_window, playing_time_window))
     win_perc1 <- winpercentages(inwindow, thisseason, w)
     win_perc2 <- winpercentages(inwindow, thisseason, w)
  }
  
  ### Special case for the last observed day
  offsets_by_team <- NULL
  if (i==max_real_date){
    if (thisseason==current_season){
       cr <- use_current_rosters
    }
    ytd_scores <- data.frame(rbindlist(scores)) %>% 
      filter(current_season_data_used==1 & is.na(prob_selected_team_win_d)==FALSE & is.na(selected_team_win)==FALSE)
    posterior=mean(ytd_scores$prob_selected_team_win_d)
    prior=mean(ytd_scores$selected_team_win)
    
    if (adjust_intercept_by_team==1){
      offsets_by_team <- get_team_offsets(ytd_scores)
    }
    rm(ytd_scores)
    
    ### Get the latest data for forecasting
    inwindow <- filter(box_scores_plus, DATE_INDEX<=max_real_date & DATE_INDEX>max_real_date-playing_time_window+1)
    win_perc1 <- winpercentages(inwindow, thisseason, w)
    win_perc2 <- winpercentages(inwindow, thisseason, w)
    
  }

  ### Predict game outcomes
  thisday <- filter(box_scores, DATE_INDEX==i) 
  games <- unique(thisday$game_id)
  thisdate <- max(thisday$DATE)
  
  inwindow_active <- mutate(inwindow,
    today=as.Date(thisdate),                        
    injured=ifelse(is.na(injury_status), 0, ifelse(today>=injury_scrape_date & today<return_date, 1, 0))
  )
  injured_players <- unique(subset(inwindow_active, injured==1)$PLAYER_FULL_NAME)
  if (length(injured_players)>0){
    print(paste0("Injuries: ", injured_players))
    inwindow_active <- filter(inwindow_active, injured==0)
  }
  
  if (w==1){
    print("Using CARM-ELO weights")
  }      
  if (cr==1){
    print("Using current scraped rosters")
  }
  
  for (d in 1:length(games)){
    pred <- predict_game(c, filter(inwindow_active, DATE_INDEX>j-playing_time_window), win_perc1, win_perc2, games[d], sims, subset(thisday, game_id==games[d]), nclus, prior, posterior, "/Users/kimlarsen/Documents/Code/NBA_RANKINGS/rawdata/", model_variables, cr, offsets_by_team)
    scores[[counter]] <- pred[[1]]
    model_parts[[counter]] <- pred[[2]] 
    counter <- counter + 1
  }
  rm(inwindow_active)
  rm(inwindow)
}

### Manipulate and save the output
#results <- manipulate_and_save_output(clusters_and_players, scores, model_parts, model_details, "/Users/kimlarsen/Documents/Code/NBA_RANKINGS/", 0, 1, as.Date("2016-11-20"))
#results <- manipulate_and_save_output(clusters_and_players, scores, model_parts, model_details, "/Users/kimlarsen/Documents/Code/NBA_RANKINGS/", 0, 0)
results <- manipulate_and_save_output(clusters_and_players, scores, model_parts, model_details, "/Users/kimlarsen/Documents/Code/NBA_RANKINGS/", 0, 1, NA)

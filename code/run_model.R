library(dplyr)
library(ggplot2)
library(readxl)
library(data.table)
library(glmnet)
library(tidyr)


box_scores <- readRDS("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/cleandata/box_scores.RDA")

source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/auc.R")
source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/assign_clusters.R")
source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/weighted_winpercentages.R")
source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/predict_game.R")
source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/get_surplus_variables.R")

### Global settings
cutoff <- 8 # minutes per game. if a player plays less than this amount, he is excluded
estimation_window <- 180
winstreak_window <- 91
playing_time_window <- 91
cluster_window <- 91
alpha <- 0 # for elastic net

### Start the forecast date and end dates
start_date <- min(subset(box_scores, season==2015)$DATE)
end_date <- max(subset(box_scores, season==2015)$DATE)

### Cut off the box scores
box_scores <- subset(box_scores, DATE<=end_date)

### Create a date-index
datemap <- select(box_scores, DATE, DATE_INDEX, future_game, season) %>% distinct(DATE, .keep_all=TRUE)

### specify start and end points
ignore_season_prior_to <- 2014
start_index <- subset(datemap, DATE==start_date)$DATE_INDEX
end_index <- nrow(datemap)
end_index <- start_index+5

scores <- list()
counter <- 1

centroids <- readRDS("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/centroids/centroids.RDA")


### Assign clusters to the historical data and calculate rolling win percentages
box_scores_plus_list <- list()
counter <- 1
for (i in min(subset(datemap, season==ignore_season_prior_to)$DATE_INDEX):max(subset(datemap, future_game==0)$DATE_INDEX)){
  
  print(paste0("DATE = ", datemap[i, "DATE"]))

  ### Data inside the window  
  inwindow <- filter(box_scores, DATE<datemap[i, "DATE"] & DATE>datemap[i-cluster_window, "DATE"])
  thisdate <- filter(box_scores, DATE==datemap[i, "DATE"])

  win_perc <- weighted_winpercentages(filter(inwindow, DATE>datemap[i-winstreak_window, "DATE"]))
  
  clusters <- assign_clusters(centroids, inwindow, cutoff)

  ### Join against win percentages and clusters  
  f <- inner_join(thisdate, select(clusters, PLAYER_FULL_NAME, Cluster), by="PLAYER_FULL_NAME") %>%
       inner_join(win_perc, by="selected_team")
  
  box_scores_plus_list[[counter]] <- f
  counter <- counter + 1
}
box_scores_plus <- data.frame(rbindlist(box_scores_plus_list))

### Number of clusters
nclus <- max(box_scores_plus$Cluster)

### Predict the past and the future
counter <- 1
for (i in start_index:end_index){
  
  
  ### Make sure we only use real data
  max_real_date <- max(subset(datemap, future_game==0)$DATE_INDEX)
  j <- min(max_real_date, i)

  ### Check the dates
  print(i)
  print(j)
  print(datemap[i, "DATE"])
  print(datemap[j, "DATE"])
  
  ### Data inside the window  
  inwindow <- filter(box_scores_plus, DATE<datemap[j, "DATE"] & DATE>datemap[j-estimation_window, "DATE"]) 
  
  ### Get rest and travel by game_id
  rest_and_travel <- select(inwindow, game_id, travel, rest_differential) %>% 
    distinct(game_id, .keep_all=TRUE)

  ### Estimate the model
  x <- get_surplus_variables(inwindow, nclus)  %>%
    inner_join(win_perc, by="selected_team") %>%
    inner_join(rest_and_travel, by="game_id") %>%
    select(-game_id, -selected_team)

  Y <- x$selected_team_win
  x$selected_team_win <- NULL

  X <- model.matrix(as.formula(Y ~ .), x)
  model <- cv.glmnet(y=Y, x=X, family="binomial", alpha=alpha, parallel=FALSE, nfolds=10)
  c <- as.matrix(coef(model, s=model$lambda.1se))
  p <- prob_win <- 1/(1+exp(-X%*%c[-1]))
  print(paste0("C: ", AUC(Y, p)[1]))
  
  ### Score the games
  thisday <- filter(box_scores, DATE==datemap[i, "DATE"]) 
  games <- unique(thisday$game_id)

  for (d in 1:length(games)){
    scores[[counter]] <- predict_game(c, filter(inwindow, DATE>datemap[j-playing_time_window, "DATE"]), win_perc, games[d], datemap[i, "DATE"], 100, subset(thisday, game_id==games[d]), nclus)
    counter <- counter + 1
  }
}

output <- data.frame(rbindlist(scores))
  

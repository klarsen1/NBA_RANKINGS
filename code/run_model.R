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
window <- 91 # number of days used to estimate models and playing time
streak <- 31
alpha <- 0 # for elastic net

### Start the forecast date and end dates
start_date <- min(subset(box_scores, season==2015)$DATE)
end_date <- max(subset(box_scores, season==2015)$DATE)

### Cut off the box scores
box_scores <- subset(box_scores, DATE<=end_date)

### Create a date-index
dates <- sort(unique(box_scores$DATE))
datemap <- data.frame(cbind(dates, row_number(dates), box_scores$future_game))
names(datemap) <- c("DATE", "ROW", "future_game")
start_index <- subset(datemap, DATE==start_date)$ROW[1]

scores <- list()
counter <- 1
  
for (i in start_index:nrow(datemap)){
  
  ### Make sure we only use real data
  max_real_date <- max(subset(datemap, future_game==0)$ROW)
  j <- min(max_real_date, i)

  ### Data inside the window  
  inwindow <- dplyr::filter(box_scores, DATE<dates[j] & DATE>dates[j-window]) 
  
  centroids <- readRDS("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/centroids/centroids.RDA")
  clusters <- assign_clusters(centroids, inwindow, cutoff)
  
  ### Get weighted win percentages for the selected team
  win_perc <- weighted_winpercentages(filter(inwindow, DATE>dates[j-streak]))
  
  ### Join againt win percentages and clusters  
  inwindow <- inner_join(inwindow, select(clusters, PLAYER_FULL_NAME, Cluster), by="PLAYER_FULL_NAME")

  ### Get rest and travel by game_id
  rest_and_travel <- select(inwindow, game_id, travel, rest_differential) %>% 
    distinct(game_id, .keep_all=TRUE)

  ### Estimate the model
  x <- get_surplus_variables(inwindow)  %>%
    inner_join(win_perc, by="selected_team") %>%
    inner_join(rest_and_travel, by="game_id") %>%
    select(-game_id, -selected_team)

  Y <- x$selected_team_win
  x$selected_team_win <- NULL

  X <- model.matrix(as.formula(Y ~ .), x)
  model <- cv.glmnet(y=Y, x=X, family="binomial", alpha=alpha, parallel=FALSE, nfolds=10)
  c <- as.matrix(coef(model, s=model$lambda.1se))
  
  ### Score the games
  thisday <- dplyr::filter(box_scores, DATE==dates[i]) 
  games <- unique(thisday$game_id)

  for (d in 1:length(games)){
    scores[[counter]] <- predict_game(c, inwindow, win_perc, games[d], dates[i], 100, thisday)
    counter <- counter + 1
  }
}


  

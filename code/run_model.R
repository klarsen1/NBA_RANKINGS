library(dplyr)
library(ggplot2)
library(readxl)
library(data.table)
library(glmnet)
library(tidyr)
library(parallel)
library(foreach)
library(doParallel)


box_scores <- readRDS("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/cleandata/box_scores.RDA")

source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/auc.R")
source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/assign_clusters.R")
source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/weighted_winpercentages.R")
source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/predict_game.R")
source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/get_surplus_variables.R")
source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/reporting.R")

### Global settings
cutoff <- 8 # minutes per game. if a player plays less than this amount, he is excluded
estimation_window <- 180
winstreak_window <- 91
playing_time_window <- 91
cluster_window <- 91
alpha <- 0 # for elastic net
sims <- 1
ignore_winstreaks <- 0

### Start the forecast date and end dates
start_date <- min(subset(box_scores, season==2015)$DATE)
end_date <- max(subset(box_scores, season==2015)$DATE)

### Cut off the box scores
box_scores <- subset(box_scores, DATE<=end_date)


### If we want to trick the model to backcast, edit the future_game indicator
#box_scores <- mutate(box_scores, future_game = ifelse(DATE>as.Date(), 1, future_game))


### Create a date-index
datemap <- select(box_scores, DATE, DATE_INDEX, future_game, season) %>% distinct(DATE, .keep_all=TRUE)

### specify start and end points
ignore_season_prior_to <- 2014
start_index <- subset(datemap, DATE==start_date)$DATE_INDEX
end_index <- subset(datemap, DATE==end_date)$DATE_INDEX
#end_index <- start_index+5


### Assign clusters to the historical data and calculate rolling win percentages
centroids <- readRDS("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/centroids/centroids.RDA")
loop_result <- foreach(i=min(subset(datemap, season==ignore_season_prior_to)$DATE_INDEX):max(subset(datemap, future_game==0)$DATE_INDEX)) %dopar% {
  
  ### Data inside the window  
  inwindow <- filter(box_scores, DATE_INDEX<datemap[i, "DATE_INDEX"] & DATE_INDEX>datemap[i-cluster_window, "DATE_INDEX"])
  thisdate <- filter(box_scores, DATE_INDEX==datemap[i, "DATE_INDEX"])
  thisseason <- thisdate[1,"season"]

  win_perc <- weighted_winpercentages(filter(inwindow, DATE_INDEX>datemap[i-winstreak_window, "DATE_INDEX"]), thisseason)

  clusters <- assign_clusters(centroids, inwindow, cutoff)
  
  ### Join against win percentages and clusters  
  f <- inner_join(thisdate, select(clusters, PLAYER_FULL_NAME, Cluster), by="PLAYER_FULL_NAME") %>%
       left_join(win_perc, by="selected_team") %>%
        replace(is.na(.), 0)
  
  if (ignore_winstreaks==1){
    f$win_perc_delta <- 0
  }
  
  return(f)
}
box_scores_plus <- data.frame(rbindlist(loop_result))

### Number of clusters
nclus <- max(box_scores_plus$Cluster)

### Predict the past and the future
counter <- 1
modelupdates <- 1
scores <- list()
model_details <- list()
for (i in start_index:end_index){
  
  ### Make sure we only use real data
  max_real_date <- max(subset(datemap, future_game==0)$DATE_INDEX)
  j <- min(max_real_date, i)

  ### Check the dates
  print(datemap[i, "DATE"])
  print(datemap[j, "DATE"])
  
  ### Data inside the window  
  inwindow <- filter(box_scores_plus, DATE_INDEX<datemap[j, "DATE_INDEX"] & DATE_INDEX>datemap[j-estimation_window, "DATE_INDEX"]) 
  
  ### Estimate the model unless we have run out of historical data
  if (counter==1 | i <= j){
  
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
     
     details <- cbind.data.frame(sapply(row.names(c), as.character), sapply(c, as.numeric))
     names(details) <- c("Variable", "Coeff")
     details$DATE <- datemap[i, "DATE"] 
     details$AUROC <- AUC(Y, p)[1]
     details$N <- nrow(X)
     model_details[[modelupdates]] <- subset(details, Variable != "(Intercept)")
     modelupdates <- modelupdates + 1
  }
  
  ### Predict game outcomes
  thisday <- filter(box_scores, DATE==datemap[i, "DATE"]) 
  games <- unique(thisday$game_id)

  for (d in 1:length(games)){
    scores[[counter]] <- predict_game(c, filter(inwindow, DATE_INDEX>datemap[j-playing_time_window, "DATE_INDEX"]), win_perc, games[d], datemap[i, "DATE"], sims, subset(thisday, game_id==games[d]), nclus, prior=0.50, posterior=0.55)
    counter <- counter + 1
  }
}

output <- data.frame(rbindlist(scores))
models <- data.frame(rbindlist(model_details))

print(paste0("C: ", AUC(output$selected_team_win, output$prob_selected_team_win_d)[1]))
print(paste0("C: ", AUC(output$selected_team_win, output$prob_selected_team_win_b)[1]))
cor(output$prob_selected_team_win_b, output$prob_selected_team_win_d)

mean(output$prob_selected_team_win_b)
mean(output$prob_selected_team_win_d)
mean(output$selected_team_win)

report(output, 2)

output <- readRDS("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/rankings/first_successful_sim.RDA")
#saveRDS(output, "/Users/kimlarsen/Documents/Code/NBA_RANKINGS/rankings/first_successful_sim.RDA")
# 0.687457329904138
# 0.6739852700491

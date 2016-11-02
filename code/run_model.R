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
conferences <- read.csv("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/rawdata/Conferences.csv", stringsAsFactors = FALSE)


source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/auc.R")
source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/assign_clusters.R")
source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/winpercentages.R")
source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/predict_game.R")
source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/get_surplus_variables.R")
source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/reporting.R")
source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/sim_playoffs.R")
source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/attach_win_perc.R")


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
start_date <- min(subset(box_scores, season==2015)$DATE)+7
end_date <- max(subset(box_scores, season==2015)$DATE)

### Cut off the box scores
box_scores <- subset(box_scores, DATE<=end_date)

### If we want to trick the model to backcast, edit the future_game indicator by filling in the xs
#box_scores <- mutate(box_scores, future_game = ifelse(DATE>as.Date("xxxxxxx"), 1, future_game))

### Create a date-index
datemap <- select(box_scores, DATE, DATE_INDEX, future_game, season) %>% distinct(DATE, .keep_all=TRUE)

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
#for (i in s:e){

  #print(datemap[i, "DATE"])
  ### Data inside the window  
  inwindow <- filter(box_scores, DATE_INDEX<datemap[i, "DATE_INDEX"] & DATE_INDEX>datemap[i-cluster_window, "DATE_INDEX"])
  thisdate <- filter(box_scores, DATE_INDEX==datemap[i, "DATE_INDEX"])
  thisseason <- thisdate[1,"season"]

  win_perc1 <- winpercentages(filter(inwindow, DATE_INDEX>datemap[i-winstreak_window, "DATE_INDEX"]), thisseason)
  win_perc2 <- winpercentages(filter(inwindow, DATE_INDEX>datemap[i-winstreak_window_s, "DATE_INDEX"]), thisseason)
  
  clusters <- assign_clusters(centroids, inwindow, cutoff)

  
  ### Join against win percentages and clusters  
  t <- inner_join(thisdate, select(clusters, PLAYER_FULL_NAME, Cluster), by="PLAYER_FULL_NAME")
  print(names(t))
  f <- attach_win_perc(t, win_perc1, win_perc2)

  if (ignore_winstreaks==1){
    f$winrate_early_season_opposing_team <- 0
    f$winrate_season_opposing_team <- 0
    f$winrate_season_opposing_team_s <- 0
    f$winrate_early_season_selected_team <- 0
    f$winrate_season_selected_team <- 0
    f$winrate_season_selected_team_s <- 0
    #f$win_rate_trend_opposing_team <- 0
    #f$win_rate_trend_selected_team <- 0
  }
  
  return(f)
}

box_scores_plus <- data.frame(rbindlist(loop_result))

## Save clusters
clusters_and_players <- 
  select(box_scores_plus, DATE, PLAYER_FULL_NAME, Cluster, points, assists, offensive_rebounds, defensive_rebounds, turnovers, threepointers_made, threepoint_attempts, steals, minutes, fieldgoal_attempts, fieldgoals_made, freethrow_attempts, freethrows_made, fouls, blocks) %>%
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
     rest_and_travel <- select(inwindow, game_id, opposing_team_travel, selected_team_travel, opposing_team_rest, selected_team_rest) %>% 
       distinct(game_id, .keep_all=TRUE)

     ### Estimate the model
     x <- get_surplus_variables(inwindow, nclus)  %>%
       inner_join(rest_and_travel, by="game_id") %>%
       select(-game_id, -selected_team)

     Y <- x$selected_team_win
     x$selected_team_win <- NULL

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
     thisseason <- filter(inwindow, DATE==max(DATE))
     win_perc1 <- winpercentages(filter(inwindow, DATE_INDEX>datemap[i-winstreak_window, "DATE_INDEX"]), thisseason[1,"season"])
     win_perc2 <- winpercentages(filter(inwindow, DATE_INDEX>datemap[i-winstreak_window_s, "DATE_INDEX"]), thisseason[1,"season"])
     
  }
  
  ### Predict game outcomes
  thisday <- filter(box_scores, DATE==datemap[i, "DATE"]) 
  games <- unique(thisday$game_id)
  
  for (d in 1:length(games)){
    pred <- predict_game(c, filter(inwindow, DATE_INDEX>datemap[j-playing_time_window, "DATE_INDEX"]), win_perc1, win_perc2, games[d], sims, subset(thisday, game_id==games[d]), nclus, 0.50, 0.55, "/Users/kimlarsen/Documents/Code/NBA_RANKINGS/rawdata/")
    scores[[counter]] <- pred[[1]]
    model_parts[[counter]] <- pred[[2]] 
    counter <- counter + 1
  }
}

### Manipulate the output
game_level <- data.frame(rbindlist(scores), stringsAsFactors = FALSE) %>% 
  mutate(d=ifelse(is.na(selected_team_win), as.numeric(prob_selected_team_win_d>0.5), selected_team_win)) 
ranks <- report(game_level, "d") %>%
  left_join(conferences, by="team") %>%
  select(team, games, pred_win_rate, conference)
models <- data.frame(rbindlist(model_details), stringsAsFactors = FALSE)
parts <- data.frame(rbindlist(model_parts), stringsAsFactors = FALSE)
details <- mutate(game_level, d_road_team_predicted_win=ifelse(selected_team==road_team_name, d, 1-d), 
                  d_home_team_predicted_win=1-d_road_team_predicted_win, 
                  predicted_winner=ifelse(d_road_team_predicted_win==1, road_team_name, home_team_name),
                  actual_winner=ifelse(is.na(selected_team_win), "NA", ifelse(selected_team_win==1, selected_team, opposing_team)),
                  home_team_prob_win=ifelse(selected_team==home_team_name, prob_selected_team_win_d, 1-prob_selected_team_win_d), 
                  road_team_prob_win=1-home_team_prob_win) %>%
  select(DATE, home_team_name, road_team_name, d_road_team_predicted_win, road_team_prob_win, d_home_team_predicted_win, home_team_prob_win, predicted_winner, actual_winner)








##### Run the playoffs
inwindow <- filter(box_scores_plus, DATE_INDEX<datemap[end_index, "DATE_INDEX"] & DATE_INDEX>datemap[end_index-estimation_window, "DATE_INDEX"]) 
thisseason <- filter(inwindow, DATE==max(DATE))
win_perc <- winpercentages(filter(inwindow, DATE_INDEX>datemap[end_index-winstreak_window, "DATE_INDEX"]), thisseason[1,"season"])


ncore <- detectCores()-1
registerDoParallel(ncore)
loop_result <- foreach(p=1:100) %dopar% {
   playoffs <- sim_playoff(ranks, inwindow, playing_time_window, win_perc, datemap, 1, "/Users/kimlarsen/Documents/Code/NBA_RANKINGS", c, end_index, thisseason[1,"season"])
   winner <- subset(playoffs[[1]], status=="W")$team
   return(data.frame(p, winner))
}

title_chances <- data.frame(rbindlist(loop_result)) %>% group_by(winner) %>%
  summarise(n=n()) %>%
  mutate(prob_win_title=n/100) %>%
  select(-n)

### Save results
#save_results("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/")
  
  
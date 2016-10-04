library(dplyr)
library(ggplot2)
library(readxl)
library(data.table)
library(scales)
library(randomForestSRC)
library(doMC)
library(glmnet)
library(gridExtra)
library(grid)
library(tidyr)

setwd("/Users/kimlarsen/Code/analytics/analysis/nba/predict")

source("/Users/kimlarsen/Code/analytics/analysis/nba_rankings/functions/auc.R")

### Global settings
cutoff <- 8 # minutes per game. if a player plays less than this amount, he is excluded
nclus <- 25 # number of archetypes
window <- 91 # number of days used to cluster
alpha <- 0 # for elastic net

### Get the moving forecasts
dates <- sort(unique(box_scores$DATE))
datemap <- cbind.data.frame(dates, row_number(dates))
names(datemap) <- c("DATE", "ROW")

start_date <- min(subset(box_scores, season==2015)$DATE)
start_index <- subset(datemap, DATE==start_date)$ROW
  
scores <- list()
importances <- list()
team_stats <- list()
cluster_movement <- list()
surpluses <- list()
counter <- 1
  
for (i in start_index:length(dates)){
  
  inwindow <- dplyr::filter(box_scores, DATE<dates[i] & DATE>dates[i-window]) 
  
  assign_clusters()
    
  ### Get weighted win percentages for the selected team
  win_perc <- weighted_winpercentages(inwindow)
  
  ### Join againt win percentages and clusters  
  inwindow <- inner_join(inwindow, select(clusters, PLAYER_FULL_NAME, Cluster), by="PLAYER_FULL_NAME")
  inwindow <- inner_join(inwindow, win_perc, by="PLAYER_FULL_NAME")
  
  
  
  
  
  
  thisday <- inner_join(filter(box_scores, DATE==dates[i]), select(clusters, PLAYER_FULL_NAME, Cluster), by="PLAYER_FULL_NAME")
    

    for (j in 0:nclus){
      rfdata[,paste0("share_minutes_cluster_", j)] <- rfdata$share_of_minutes_signed * as.numeric(rfdata$Cluster==j)
      thisday[,paste0("share_minutes_cluster_", j)] <- thisday$share_of_minutes_signed * as.numeric(thisday$Cluster==j)
    }


    thisday <- dplyr::select(thisday, selected_team, selected_team_win, starts_with("share_minutes_cluster_"), home_team_selected, travel, days_since_last_game, game_id) %>%
      group_by(game_id, selected_team) %>%
      summarise_each(funs(sum)) %>%
      mutate(selected_team_win=as.numeric(selected_team_win>0), 
             home_team_selected=as.numeric(home_team_selected>0)) %>%
      ungroup() %>%
      left_join(win_perc, by="selected_team") %>%
      replace(is.na(.), 0)
    
      
    surpluses[[counter]] <- for_bar_plot


    thisday <- left_join(thisday, a, by="selected_team") %>%
               replace(is.na(.), 0)
    rm(X)
      
    Y <- thisday$selected_team_win
    X <- model.matrix(f, data.frame(dplyr::select(thisday, -selected_team_win, -game_id, -selected_team, -adj)))[, -1]
    prob_win <- 1/(1+exp(-X%*%c[-1] + thisday$adj))
    importances[[counter]] <- arrange(selected, Coeff)
    rm(selected)
    result <-  data.frame(cbind(select(thisday, game_id, selected_team_win), prob_win), stringsAsFactors = FALSE)

    names(result) <- c("game_id", "selected_team_win", "prob_win_selected_team")
    result$pred_win <- as.numeric(result$prob_win_selected_team>0.5)
    result <- inner_join(result, select(game_scores, -selected_team_win), by="game_id")
    scores[[counter]] <- result
    
    print(paste0("Clusters= ", nclus))
    print(paste0("N = ", nrow(rfdata)))
    print(paste0("Date = ", dates[i]))
    print(paste0("Players = ", nrow(means_no_scrubs)))

    rm(thisday)
    rm(scaled)
    rm(clusters)
    rm(avg_minutes)
    rm(result)
    rm(team_mix)
    rm(means_no_scrubs)
    rm(scrubs)
    rm(rfdata)
    rm(thisdata)
    rm(win_perc_opposing)
    rm(win_perc)
    rm(win_perc1)
    rm(win_perc2)
    rm(team_members)
    rm(active)
    rm(for_bar_plot)
    
    counter <- counter + 1
    
}
### End of scoring code  
    
scores <- data.frame(rbindlist(scores), stringsAsFactors = FALSE)
importances <- data.frame(rbindlist(importances), stringsAsFactors = FALSE) %>% mutate(playoffs=as.numeric(DATE>playoff_date))
team_stats <- data.frame(rbindlist(team_stats), stringsAsFactors = FALSE) %>% 
              arrange(team, DATE) %>% mutate(playoffs=as.numeric(DATE>playoff_date))
cluster_movement <- data.frame(rbindlist(cluster_movement), stringsAsFactors = FALSE) %>% 
                    arrange(PLAYER_FULL_NAME, DATE) %>% mutate(playoffs=as.numeric(DATE>playoff_date))
surpluses <- data.frame(rbindlist(surpluses), stringsAsFactors = FALSE)

### Get the C stat

paste0("C: ", AUC(scores$selected_team_win, scores$prob_win_selected_team)[1])

d <- subset(scores, DATE>=as.Date("2015-10-27"))
paste0("Accuracy: ", sum((d$selected_team_win==d$pred_win))/nrow(d))

### Summarize team performances -- have to do this twice because the data was collapsed into matchups
summary1 <- scores %>%
            group_by(playoffs, selected_team) %>%
  mutate(match=as.numeric(selected_team_win==pred_win)) %>%
  summarise(pred_win=sum(pred_win),
            prob_win_selected_team=sum(prob_win_selected_team),
            win=sum(selected_team_win),
            match=sum(match),
            games=n()) %>%
            rename(team=selected_team) %>%
  ungroup()

summary2 <- scores %>%
  group_by(playoffs, opposing_team) %>%
  mutate(match = as.numeric((selected_team_win==0)==(pred_win==0))) %>%
  summarise(pred_win=sum(as.numeric(pred_win==0)),
            prob_win_selected_team=sum(1-prob_win_selected_team),
            win=sum(selected_team_win==0),
            match=sum(match),
            games=n()) %>%
  rename(team=opposing_team) %>%
  ungroup()

summary <- rbind.data.frame(summary1, summary2) %>% group_by(playoffs, team) %>%
           summarise(win_rate=sum(win), 
                     prob_win_selected_team=sum(prob_win_selected_team),
                     games=sum(games), 
                     pred_win=sum(pred_win), 
                     match=sum(match)) %>%
           mutate(actual_win_rate = win_rate/games, 
                 prob_win_selected_team = prob_win_selected_team/games, 
                 pred_win_rate = pred_win / games,
                 accuracy = match / games, 
                 lift_over_coin_flip = accuracy/0.5-1, 
                 actual_wins=win_rate, 
                 predicted_wins=pred_win) %>%
           arrange(playoffs, pred_win_rate) %>%
           select(-match, -prob_win_selected_team, -win_rate, -pred_win) %>%
           mutate(pred_rank=rank(-pred_win_rate), 
                  actual_rank=rank(-actual_win_rate)) %>%
           ungroup()

View(summary)

setwd("/Users/kimlarsen/Documents/Code/nbapost/results")

saveRDS(importances, "importance_history.RDA")
saveRDS(cluster_movement, "cluster_history.RDA")
saveRDS(scores, "scores_and_predictions.RDA")
saveRDS(team_stats, "historical_team_cluster_deltas.RDA")
saveRDS(summary, "high_level_summary.RDA")
saveRDS(production_centroids, "centroids_from_2013.RDA")

importances <- readRDS("importance_history.RDA")
cluster_movement <- readRDS("cluster_history.RDA")
scores <- readRDS("scores_and_predictions.RDA")
team_stats <- readRDS("historical_team_cluster_deltas.RDA")
summary <- readRDS("high_level_summary.RDA")
production_centroids <- readRDS("centroids_from_2013.RDA")

df <- filter(summary, playoffs==0) %>% 
      dplyr::select(-playoffs, -games, -lift_over_coin_flip, -actual_wins, -predicted_wins) %>%
      mutate(actual = paste0(round(actual_win_rate*100), "%"),
             predicted = paste0(round(pred_win_rate*100), "%"), 
             rank = pred_rank, 
             rank_diff = pred_rank - actual_rank) %>%
      arrange(-pred_win_rate) %>%
      select(-actual_win_rate, -pred_win_rate, -actual_rank, -pred_rank, -accuracy) %>% arrange()
View(df)

###### Deal with the playoffs
### Summarize team performances -- have to do this twice because the data was collapsed into matchups
playoff1 <- scores %>%
  filter(playoffs==1) %>%
  group_by(selected_team, opposing_team) %>%
  mutate(match=as.numeric(selected_team_win==pred_win)) %>%
  summarise(pred_win=sum(pred_win),
            prob_win_selected_team=sum(prob_win_selected_team),
            win=sum(selected_team_win),
            match=sum(match),
            games=n()) %>%
  rename(team=selected_team) %>%
  ungroup()

playoff2 <- scores %>%
  filter(playoffs==1) %>%
  group_by(opposing_team, selected_team) %>%
  mutate(match = as.numeric((selected_team_win==0)==(pred_win==0))) %>%
  summarise(pred_win=sum(as.numeric(pred_win==0)),
            prob_win_selected_team=sum(1-prob_win_selected_team),
            win=sum(selected_team_win==0),
            match=sum(match),
            games=n()) %>%
  rename(team=opposing_team,
         opposing_team=selected_team) %>%
  ungroup()

playoff_summary <- rbind.data.frame(playoff1, playoff2) %>% group_by(team, opposing_team) %>%
  summarise(win_rate=sum(win), 
            prob_win_selected_team=sum(prob_win_selected_team),
            games=sum(games), 
            pred_win=sum(pred_win), 
            match=sum(match)) %>%
  mutate(prob_win_selected_team = prob_win_selected_team/games, 
         accuracy = match / games, 
         actual_wins=win_rate, 
         predicted_wins=pred_win) %>%
  arrange(-actual_wins) %>%
  select(-match, -prob_win_selected_team, -win_rate, -pred_win, -accuracy, -actual_wins) %>%
  filter(team=="Golden State") %>%
  ungroup()

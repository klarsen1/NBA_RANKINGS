library(dplyr)
library(ggplot2)
library(readxl)
library(data.table)
library(glmnet)
library(tidyr)
library(parallel)
library(foreach)
library(doParallel)
root <- "/Users/kim.larsen/Documents/Code/NBA_RANKINGS"
source(paste0(root, "/functions/auc.R"))
source(paste0(root, "/functions/assign_clusters.R"))
source(paste0(root, "/functions/winpercentages.R"))
source(paste0(root, "/functions/predict_game.R"))
source(paste0(root, "/functions/get_surplus_variables.R"))
source(paste0(root, "/functions/reporting.R"))
source(paste0(root, "/functions/sim_playoffs.R"))
source(paste0(root, "/functions/attach_win_perc.R"))
source(paste0(root, "/functions/manipulate_and_save_output.R"))
source(paste0(root, "/functions/save_results.R"))
source(paste0(root, "/functions/get_team_offsets.R"))
source(paste0(root, "/functions/assign_clusters_and_win_rates.R"))
source(paste0(root, "/functions/combine.R"))

## Read the box scores
box_scores <- readRDS(paste0(root, "/cleandata/box_scores.RDA")) %>%
  filter(playoffs==0)

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
# current_season <- 2019
adjust_intercept_by_team <- 0
buffer_days <- 10


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
model_variables <- read.csv(paste0(root, "/modeldetails/model_variables.csv"), stringsAsFactors = FALSE)

### When to start and end the forecasts
start_date <- min(subset(box_scores, season==current_season)$DATE)
end_date <- max(subset(box_scores, season==current_season & playoffs==0)$DATE)

### Cut off the box scores
box_scores <- subset(box_scores, DATE<=end_date) %>%
  ungroup() %>%
  mutate(fb=ifelse(season==max(season), 1, 0))

### specify start and end points
ignore_season_prior_to <- 2016
start_index <- subset(datemap, DATE==start_date)$DATE_INDEX
end_index <- subset(datemap, DATE==end_date)$DATE_INDEX

### Assign clusters to the historical data and calculate rolling win percentages
box_scores_plus <- assign_clusters_and_win_rates(root, datemap, box_scores, weighted_win_rates, cluster_window)

## Save clusters
clusters_and_players <- 
  select(box_scores_plus, DATE, PLAYER_FULL_NAME, Cluster, season) %>%
  ungroup() %>%
  filter(season==max(season)) %>%
  arrange(Cluster, PLAYER_FULL_NAME, DATE)

### Number of clusters
nclus <- max(box_scores_plus$Cluster)

### Predict the past and the future -- season only
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
     win_perc2 <- win_perc1
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
    print("Using 538 wins as weights")
  }      
  if (cr==1){
    print("Using current scraped rosters")
  }
  
  for (d in 1:length(games)){
    pred <- predict_game(c, filter(inwindow_active, DATE_INDEX>j-playing_time_window), win_perc1, win_perc2, games[d], sims, subset(thisday, game_id==games[d]), nclus, prior, posterior, paste0(root, "/rawdata/"), model_variables, cr, offsets_by_team)
    scores[[counter]] <- pred[[1]]
    model_parts[[counter]] <- pred[[2]] 
    counter <- counter + 1
  }
  rm(inwindow_active)
  rm(inwindow)
}

### Manipulate and save the output
results <- manipulate_and_save_output(clusters_and_players, scores, model_parts, model_details, root, 0, 1, NA, "prob_selected_team_win")


### Run the playoffs

playoff_start_date <- max(box_scores$DATE)+1 ## faking it a bit here

runs <- 0

rankings <- results[[2]] %>% filter(seed<9)

inwindow <- filter(box_scores_plus, DATE_INDEX<=max_real_date & DATE_INDEX>max_real_date-playing_time_window+1)
thisseason <- filter(inwindow, DATE==max(DATE))[1,"season"]
win_perc1 <- winpercentages(inwindow, thisseason, w)
win_perc2 <- win_perc1

inwindow_active <- mutate(inwindow,
                          today=as.Date(end_date),                        
                          injured=ifelse(is.na(injury_status), 0, ifelse(playoff_start_date>=injury_scrape_date & playoff_start_date<=return_date, 1, 0)))
injured_players <- unique(subset(inwindow_active, injured==1)$PLAYER_FULL_NAME)
if (length(injured_players)>0){
  print(sort(injured_players))
  inwindow_active <- filter(inwindow_active, injured==0)
}

ncore <- detectCores()-1
registerDoParallel(ncore)
sims <- 1
loopResult <- foreach(i=1:sims, .combine='combine', .multicombine=TRUE,
                      .init=list(list(), list(), list())) %dopar% {
                        playoffs <- sim_playoff(rankings, inwindow_active, playing_time_window, win_perc1, win_perc2, datemap, runs, root, c, max_real_date, thisseason, end_date, seed=1000*i + runif(1)*1000)
                        playoffs[[2]]$sim <- i
                        return(list(playoffs[[2]], playoffs[[3]], data.frame(playoffs[[1]])))
                      }

full_results <- data.frame(rbindlist(loopResult[[1]]))
r <- max(full_results$round)
m <- max(full_results$matchup)


coin_flips <- list()
f <- 1
for (i in 1:r){
  for (j in 1:m){
    d <- data.frame(filter(full_results, round==i & matchup==j))
    if (nrow(d)>0){
      dd <- filter(d, winner != "NONE")
      t1 <- dd$winner
      t2 <- dd$loser
      d <- filter(d, game<5)
      p <- d$prob_selected_team_win
      s <- d$selected_team
      n1 <- 0
      n2 <- 0
      p <- c(p, p[1], p[3], p[1])
      s <- c(s, s[1], s[3], s[1])
      for (k in 1:20000){
        nn1 <- 0
        nn2 <- 0
        for (g in 1:length(p)){
          binomial <- as.numeric(rbinom(n=1, size=1, prob=p[g]))  
          if (t1==s[g]){
            if (binomial==1){nn1 <- nn1+1}
            else {nn2 <- nn2+1}
          } else{
            if (binomial==1){nn2 <- nn2+1}
            else {nn1<- nn1+1}
          }
        }
        if (nn1>nn2){
          n1 <- n1+1
        } else{
          n2 <- n2+1
        }
      }
      prob_win <- n1/(n1+n2)
      simresult <- data.frame(matrix(nrow=1, ncol=7))
      names(simresult) <- c("round", "matchup", "selected_team", "winner", "loser", "prob_win_series", "sims")
      simresult$round <- i
      simresult$matchup <- j
      simresult$selected_team <- s[1]
      simresult$winner <- t1
      simresult$loser <- t2
      simresult$prob_win_series <- prob_win
      simresult$sims <- n1+n2
      coin_flips[[f]] <- simresult
      f <- f+1
    }
  }
}

seeds <- mutate(rankings, winner=team, loser=team) %>%
  select(winner, loser, seed)

playoff_results <- 
  data.frame(rbindlist(coin_flips)) %>% 
  arrange(round, winner, matchup) %>% 
  inner_join(select(seeds, -loser), by="winner") %>%
  rename(winner_seed=seed) %>%
  inner_join(select(seeds, -winner), by="loser") %>%
  rename(loser_seed=seed)

write.csv(playoff_results, paste0(root, "/rankings/playoff_prediction_", Sys.Date(), ".csv"))

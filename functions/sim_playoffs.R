striHelper <- function(x) stri_c(x[stri_order(x)], collapse = "")

create_fake_entry <- function(game, date, selected_team, opposing_team, thisseason, matchup1, matchup2, order_preserved){
  thisday <- data.frame(matrix(nrow=1, ncol=18))
  names(thisday) <- c("DATE", "opposing_team_travel", "selected_team_travel", "opposing_team_rest", "selected_team_rest", "home_team_name", "road_team_name", "opposing_team", "selected_team", "selected_team_win", "season", "selected_team_matchup_wins", "opposing_team_matchup_wins", "days_on_road_selected_team", 
                      "days_on_road_opposing_team", "fb", "game_id", "future_game")
  thisday$DATE <- date
  thisday$opposing_team_travel <- 0
  thisday$selected_team_travel <- 0
  thisday$opposing_team_rest <- 0
  thisday$selected_team_rest <- 0
  thisday$home_team_selected <- 0
  thisday$selected_team_win <- NA
  thisday$season <- thisseason
  thisday$opposing_team <- opposing_team
  thisday$selected_team <- selected_team
  thisday$home_team_selected <- order_preserved
  if (order_preserved==1){
     thisday$home_team_name <- selected_team
     thisday$road_team_name <- opposing_team
     thisday$selected_team_matchup_wins <- matchup1
     thisday$opposing_team_matchup_wins <- matchup2
  } else{
    thisday$home_team_name <- opposing_team
    thisday$road_team_name <- selected_team
    thisday$selected_team_matchup_wins <- matchup2
    thisday$opposing_team_matchup_wins <- matchup1
  }
  thisday$days_on_road_selected_team <- 0
  thisday$days_on_road_opposing_team <- 0
  thisday$fb <- 0
  thisday$future_game <- 1
  cat <- paste0(selected_team, opposing_team)
  thisday$game_id <- paste0(date, vapply(stri_split_boundaries(cat, type = "character"), striHelper, ""))
  if (game %in% c(3,4,6)) {
    if (order_preserved==1){
       thisday$home_team_selected <- 0
       thisday$road_team_name <- selected_team
       thisday$home_team_name <- opposing_team
    }
    else{
      thisday$home_team_selected <- 1
      thisday$home_team_name <- selected_team
      thisday$road_team_name <- opposing_team
    }
  }  
  return(thisday)
}


sim_playoff <- function(ranks, inwindow, playing_time_window, win_perc1, win_perc2, datemap, runs=0, root, c, end_index, thisseason, last_date_in_season, seed){

  set.seed(seed)
  
  ### Read the playoff tree
  tree <- data.frame(read.csv(paste0(root, "/rawdata/playofftree.csv"), stringsAsFactors = FALSE))
  
  ### Get the qualifying teams
  qualifiers <- group_by(ranks, conference) %>% arrange(-season_win_rate) %>%
    mutate(rank=row_number(), exclude=0, status="W", round=4, ngames=7) %>%
    #filter(team != "Miami") %>%
    filter(rank<9) %>%
    select(conference, team, season_win_rate, rank, exclude, status, round, ngames) %>%
    ungroup()
  
  rounds <- max(tree$round)
  
  game_results <- list()
  decomps <- list()
  counter <- 1
  
  for (i in 1:rounds){
    #print(paste0("Round = ", i))
    matchups <- subset(tree, round==i)
    n <- max(matchups$series)
    DATE <- last_date_in_season + i
    for (j in 1:n){
      s <- subset(matchups, series==j)
      if (i<4){
         home_team <- subset(qualifiers, rank==s$rank_home & exclude==0 & conference==s$conference)$team
         road_team <- subset(qualifiers, rank==s$rank_road & exclude==0 & conference==s$conference)$team
      } else{
        h <- filter(qualifiers, exclude==0) %>% filter(row_number()==1) %>% select(team)
        r <- filter(qualifiers, exclude==0) %>% filter(row_number()==2) %>% select(team)
        home_team <- h$team
        road_team <- r$team
      }
      order_preserved <-1
      selected <- home_team
      opposing <- road_team
      last_matchup <- filter(inwindow, selected_team %in% c(home_team, road_team) & opposing_team %in% c(home_team, road_team) & season==thisseason) %>%
         ungroup() %>%
         filter(DATE==max(DATE)) %>%
         select(selected_team, opposing_team, selected_team_matchup_wins, opposing_team_matchup_wins, selected_team_win) %>%
         distinct(selected_team, .keep_all=TRUE)

      if (nrow(last_matchup)>0){
        if (last_matchup$selected_team == selected){
           matchup1 <- last_matchup$selected_team_matchup_wins + last_matchup$selected_team_win
           matchup2 <- last_matchup$opposing_team_matchup_wins + as.numeric(last_matchup$selected_team_win==0)
        } else{
           matchup2 <- last_matchup$selected_team_matchup_wins + last_matchup$selected_team_win
           matchup1 <- last_matchup$opposing_team_matchup_wins + as.numeric(last_matchup$selected_team_win==0)
        }
        total_matchups <- matchup1+matchup2
        matchup1 <- matchup1/total_matchups
        matchup2 <- matchup2/total_matchups
      } else{
        matchup1 <- 0
        matchup2 <- 0
      }
      
      w1 <- 0
      w2 <- 0
      winner_declared=FALSE
      winner <- "NONE"
      loser <- "NONE"
      games=0
      while(games<7 & winner_declared==FALSE){
         thisday <- create_fake_entry(games+1, DATE, selected, opposing, thisseason, matchup1, matchup2, order_preserved)
         pred <- predict_game(c, filter(inwindow, DATE_INDEX>end_index-playing_time_window), win_perc1, win_perc2, thisday[1,"game_id"], runs, thisday, nclus, prior, posterior, paste0(root, "/rawdata/"), model_variables, 1, NULL, seed=seed)
         decomps[[counter]] <- pred[[2]]
         if (pred[[1]]$prob_selected_team_win_d>0.5){ 
           if (order_preserved==1){
              w1 <- w1 + 1
           } else{
             w2 <- w2 + 1
           }
         }
         else{ 
           if (order_preserved==1){
             w2 <- w2 + 1
           } else{
             w1 <- w1 + 1
           }
         }
         if (w1==4){
           winner_declared <- TRUE
           winner <- home_team
           loser <- road_team
         } else if (w2==4){
           winner_declared <- TRUE
           winner <- road_team
           loser <- home_team
         }
         games <- games+1
         if (runif(1)>0.5){
           order_preserved <- 1
           #print("Kept Order")
           selected <- home_team
           opposing <- road_team
         } else{
           #print("Flipped Order")
           order_preserved <- 0
           selected <- road_team
           opposing <- home_team
         }
         #print(paste0("game ", games))
         result <- data.frame(matrix(nrow=1, ncol=6))
         names(result) <- c("round", "game", "home_team", "road_team", "prob_home_team_win", "prob_road_team_win")
         result$round <- i
         result$game <- games
         result$road_team <- road_team
         result$home_team <- home_team
         result$prob_home_team_win <- pred[[1]]$prob_selected_team_win_d
         result$prob_road_team_win <- 1-pred[[1]]$prob_selected_team_win_d
         thisday <- mutate(thisday, prob_home_team_win=ifelse(selected_team==home_team_name, pred[[1]]$prob_selected_team_win_d, 1-pred[[1]]$prob_selected_team_win_d), 
                                    prob_road_team_win=1-prob_home_team_win, 
                                    round=i, 
                                    matchup=j,
                                    prob_selected_team_win=pred[[1]]$prob_selected_team_win_d,
                                    game=games, 
                                    loser=loser, 
                                    winner=winner)
         game_results[[counter]] <- thisday
         counter <- counter + 1
      }
      qualifiers <- mutate(qualifiers, 
                           status=ifelse(team==loser, "L", status),
                           round=ifelse(exclude==1, round, i),
                           ngames=ifelse(exclude==1, ngames, games),
                           opponent=ifelse(exclude==1, opponent, ifelse(team==home_team, road_team, home_team)),
                           exclude=ifelse(team==loser, 1, exclude))
    }
    qualifiers <- arrange(qualifiers, -season_win_rate) %>% 
      group_by(conference, exclude) %>%
      mutate(rank=row_number()) %>%
      ungroup()
  }
  final_results <- rbindlist(game_results) %>%
    dplyr::select(round, matchup, winner, loser, game, prob_home_team_win, prob_selected_team_win, selected_team)
    
  return(list(qualifiers, final_results, data.frame(rbindlist(decomps)), qualifiers))
}
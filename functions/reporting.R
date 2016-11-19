report <- function(scores, predvar){

  scores$pred <- scores[,predvar]
  
  scores <- mutate(scores,
                   game_played=ifelse(is.na(selected_team_win), 0, 1),
                   future_game=ifelse(is.na(selected_team_win), 1, 0),
                   selected_team_win=ifelse(is.na(selected_team_win), 0, selected_team_win)) %>%
    replace(is.na(.), 0)
  
  ### Summarize team performances -- have to do this twice because the data was collapsed into matchups
  summary1 <- group_by(scores, selected_team) %>%
    mutate(pred_future=future_game*pred) %>%
    summarise(win=sum(selected_team_win),
              games_played=sum(game_played),
              future_games=sum(future_game),
              pred_win=sum(pred),
              future_pred_win=sum(pred_future),
              games_season=n()) %>%
    rename(team=selected_team) %>%
  ungroup()

  summary2 <- group_by(scores, opposing_team) %>%
    mutate(pred_future=future_game*(1-pred)) %>%
    summarise(win=sum(as.numeric(selected_team_win==0 & game_played==1)),
              games_played=sum(game_played),
              future_games=sum(future_game),
              pred_win=sum(1-pred),
              future_pred_win=sum(pred_future),
              games_season=n()) %>%
    rename(team=opposing_team) %>%
  ungroup()
  
  summary <- bind_rows(summary1, summary2) %>% 
     group_by(team) %>%
     summarise_each(funs(sum)) %>%
     mutate(ytd_win_rate = win/games_played, 
            pred_season_win_rate = pred_win/games_season, 
            pred_win_rate=future_pred_win/future_games) %>%
     arrange(pred_season_win_rate) %>%
     select(team, games_season, games_played, pred_season_win_rate, ytd_win_rate, pred_win_rate) %>%
     mutate(rank_actual=min_rank(-ytd_win_rate), 
            rank_pred=min_rank(-pred_season_win_rate)) %>%
  ungroup()
     
  return(data.frame(summary))   
}
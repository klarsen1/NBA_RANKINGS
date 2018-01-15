report <- function(scores, predvar){

  scores$pred <- scores[,predvar]
  
  scores <- mutate(scores,
                   game_played=ifelse(future_game, 0, 1)) %>%
    replace(is.na(.), 0)
  
  ### Summarize team performances -- have to do this twice because the data was collapsed into matchups
  summary1 <- group_by(scores, selected_team) %>%
    mutate(future=future_game*pred,
           season=ifelse(game_played==1, selected_team_win, pred),
           past=ifelse(future_game==1, 0, selected_team_win)) %>%
    summarise(future=sum(future),
              season=sum(season),
              past=sum(past),
              games_played=sum(game_played),
              games_future=sum(future_game),
              games_season=n()) %>%
    rename(team=selected_team) %>%
  ungroup()

  summary2 <- group_by(scores, opposing_team) %>%
    mutate(future=future_game*(1-pred),
           season=ifelse(game_played==1, 1-selected_team_win, 1-pred),
           past=ifelse(future_game==1, 0, 1-selected_team_win)) %>%
    summarise(future=sum(future),
              season=sum(season),
              past=sum(past),
              games_played=sum(game_played),
              games_future=sum(future_game),
              games_season=n()) %>%
    rename(team=opposing_team) %>%
  ungroup()
  
  summary <- bind_rows(summary1, summary2) %>% 
     group_by(team) %>%
     summarise_all(funs(sum)) %>%
     mutate(ytd_win_rate = past/games_played, 
            season_win_rate = season/games_season, 
            future_win_rate=future/games_future) %>%
     arrange(season_win_rate) %>%
     select(team, games_season, games_played, games_future, 
            season_win_rate, ytd_win_rate, future_win_rate) %>%
     mutate(rank_actual=min_rank(-ytd_win_rate), 
            rank_pred=min_rank(-season_win_rate)) %>%
  ungroup()
     
  return(data.frame(summary))   
}
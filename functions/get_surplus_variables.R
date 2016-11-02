get_surplus_variables <- function(data, nclus){
  for (j in 0:nclus){
    data[,paste0("share_minutes_cluster_", j)] <- data$share_of_minutes_signed * as.numeric(data$Cluster==j)
  }
  
  df <- select(data, selected_team, starts_with("share_minutes_cluster_"), home_team_selected, game_id, selected_team_win, first_game, winrate_early_season_selected_team, winrate_season_selected_team, winrate_early_season_opposing_team, winrate_season_opposing_team, winrate_season_selected_team_s, winrate_season_opposing_team_s) %>%
    group_by(game_id, selected_team) %>%
    mutate(n=1) %>%
    summarise_each(funs(sum)) %>%
    mutate(home_team_selected=as.numeric(home_team_selected>0), 
           selected_team_win=as.numeric(selected_team_win>0),
           winrate_early_season_selected_team=winrate_early_season_selected_team/n, 
           winrate_season_selected_team=winrate_season_selected_team/n, 
           winrate_early_season_opposing_team=winrate_early_season_opposing_team/n, 
           winrate_season_opposing_team=winrate_season_opposing_team/n, 
           #winrate_early_season_selected_team_s=winrate_early_season_selected_team_s/n, 
           winrate_season_selected_team_s=winrate_season_selected_team_s/n, 
           #winrate_early_season_opposing_team_s=winrate_early_season_opposing_team_s/n, 
           winrate_season_opposing_team_s=winrate_season_opposing_team_s/n, 
           #win_rate_trend_opposing_team=win_rate_trend_opposing_team/n,
           #win_rate_trend_selected_team=win_rate_trend_selected_team/n,
           first_game=as.numeric(first_game>0)) %>%
    select(-n) %>%
    ungroup()
           
  return(df)
}
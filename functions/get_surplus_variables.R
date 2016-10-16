get_surplus_variables <- function(data, nclus){
  for (j in 0:nclus){
    data[,paste0("share_minutes_cluster_", j)] <- data$share_of_minutes_signed * as.numeric(data$Cluster==j)
  }
  
  df <- select(data, selected_team, starts_with("share_minutes_cluster_"), home_team_selected, game_id, selected_team_win, early_season, winrate_selected_team, winrate_opposing_team) %>%
    group_by(game_id, selected_team) %>%
    mutate(n=1) %>%
    summarise_each(funs(sum)) %>%
    mutate(home_team_selected=as.numeric(home_team_selected>0), 
           selected_team_win=as.numeric(selected_team_win>0),
           winrate_selected_team=winrate_selected_team/n, 
           winrate_opposing_team=winrate_opposing_team/n, 
           early_season=as.numeric(early_season>0)) %>%
    select(-n) %>%
    ungroup()
           
  return(df)
}
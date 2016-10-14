get_surplus_variables <- function(data, nclus){
  for (j in 0:nclus){
    data[,paste0("share_minutes_cluster_", j)] <- data$share_of_minutes_signed * as.numeric(data$Cluster==j)
  }
  
  df <- select(data, selected_team, starts_with("share_minutes_cluster_"), home_team_selected, game_id, selected_team_win) %>%
    group_by(game_id, selected_team) %>%
    summarise_each(funs(sum)) %>%
    mutate(home_team_selected=as.numeric(home_team_selected>0), 
           selected_team_win=as.numeric(selected_team_win>0),
           win_perc_delta=mean(win_perc_delta)) %>%
    ungroup()
           
  return(df)
}
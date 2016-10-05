get_surplus_variables <- function(data){
  nclus <- length(unique(data$Cluster))
  for (j in min(data$Cluster):max(data$Cluster)){
    data[,paste0("share_minutes_cluster_", j)] <- data$share_of_minutes_signed * as.numeric(rfdata$Cluster==j)
  }
  
  df <- dplyr::select(data, selected_team, starts_with("share_minutes_cluster_"), home_team_selected) %>%
    group_by(game_id, selected_team) %>%
    summarise_each(funs(sum)) %>%
    mutate(home_team_selected=as.numeric(home_team_selected>0))
           
  return(df)
}
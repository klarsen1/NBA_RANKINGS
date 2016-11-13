get_surplus_variables <- function(data, nclus){
  df <- group_by(data, game_id, OWN_TEAM) %>%
        mutate(share_of_minutes=share_of_minutes/sum(share_of_minutes),
             share_of_minutes_signed = ifelse(OWN_TEAM==selected_team, share_of_minutes, -share_of_minutes)) %>%
  ungroup()
  
  for (j in 0:nclus){
    df[,paste0("share_minutes_cluster_", j)] <- df$share_of_minutes_signed * as.numeric(df$Cluster==j)
  }
  
  df <- select(df, game_id, starts_with("share_minutes_cluster_")) %>%
    group_by(game_id) %>%
    summarise_each(funs(sum)) %>%
    ungroup()
           
  return(df)
}
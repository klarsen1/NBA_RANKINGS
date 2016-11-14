winpercentages <- function(data, s){

  df <- subset(data, season==s) %>% distinct(game_id, .keep_all=TRUE) %>%
    select(selected_team, opposing_team, selected_team_win, selected_team_points, opposing_team_points)
  
  MM <- month(max(subset(data, season==s)$DATE))
  
  if (nrow(df)>0){
    all_teams <- distinct(data, OWN_TEAM)
    winrates <- list()
    for (k in 1:nrow(all_teams)){
      t <- all_teams[k,"OWN_TEAM"]
      winrate <- filter(df, selected_team==t | opposing_team==t) %>%
        ungroup() %>%
        mutate(win=ifelse(selected_team==t, selected_team_win, 1-selected_team_win),
               points_scored=ifelse(selected_team==t, selected_team_points, opposing_team_points),
               points_allowed=ifelse(opposing_team==t, selected_team_points, opposing_team_points)) %>%
        summarise(win_rate=mean(win), 
                  n=n(), 
                  points_scored=sum(points_scored), 
                  points_allowed=sum(points_allowed)) %>%
        mutate(opposing_team=t, 
               selected_team=t)
      winrates[[k]] <- winrate
    }
    winratesdf <- data.frame(rbindlist(winrates), stringsAsFactors=FALSE) %>%
        mutate(month=MM,
               early_season=as.numeric(month %in% c(10, 11)),
               win_rate_early_season=win_rate*as.numeric(early_season==1),
               win_rate_season=win_rate*as.numeric(early_season==0),
               point_diff=ifelse(points_allowed>0, points_scored/points_allowed, 1),
               point_diff_season=point_diff*as.numeric(early_season==0),
               point_diff_early_season=point_diff*as.numeric(early_season==1),
               first_game=0) %>%
      select(selected_team, opposing_team, win_rate_early_season, win_rate_season, first_game, point_diff_season, point_diff_early_season)
    return(winratesdf)
  } else{
    t <- distinct(data, selected_team) %>%
      mutate(win_rate_early_season=0, win_rate_season=0, opposing_team=selected_team, first_game=1, point_diff_season=0, point_diff_early_season=0)
    return(data.frame(t))
  }
}
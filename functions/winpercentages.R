winpercentages <- function(data, s, use_weights){

  df <- subset(data, season==s) %>% distinct(game_id, .keep_all=TRUE) %>%
    select(selected_team, opposing_team, selected_team_win, selected_team_points, opposing_team_points, season_day_std, carm_elo_selected_team, carm_elo_opposing_team)

  MM <- month(max(subset(data, season==s)$DATE))
  SS <- filter(data, DATE==max(DATE))[1,"season_day_std"]

  if (nrow(df)>0){
    all_teams <- distinct(data, OWN_TEAM)
    winrates <- list()
    for (k in 1:nrow(all_teams)){
      t <- all_teams[k,"OWN_TEAM"]
      if (use_weights==1){
        df <- mutate(df, weight=ifelse(selected_team==t, carm_elo_opposing_team, carm_elo_selected_team))
        #print("--- Using CARM-ELO weights")
      }  else{
        df$weight <- 1
      }
      winrate <- filter(df, selected_team==t | opposing_team==t) %>%
        ungroup() %>%
        mutate(win=ifelse(selected_team==t, selected_team_win, 1-selected_team_win),
               points_scored=ifelse(selected_team==t, selected_team_points, opposing_team_points),
               points_allowed=ifelse(opposing_team==t, selected_team_points, opposing_team_points)) %>%
        summarise(win_rate=weighted.mean(win, weight), 
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
               win_rate_season=win_rate,
               point_diff=ifelse(points_allowed>0, points_scored/points_allowed, 1),
               point_diff_season=point_diff,
               point_diff_early_season=point_diff*as.numeric(early_season==1),
               point_diff_adj=point_diff*as.numeric(SS), 
               win_rate_season_adj=win_rate_season*as.numeric(SS),
               first_game=0) %>%
      select(selected_team, opposing_team, win_rate_early_season, win_rate_season, first_game, point_diff_season, point_diff_early_season, win_rate_season_adj, point_diff_adj)
    return(winratesdf)
  } else{
    t <- distinct(data, selected_team) %>%
      mutate(win_rate_early_season=0, win_rate_season=0, opposing_team=selected_team, first_game=1, point_diff_season=0, point_diff_early_season=0, win_rate_season_adj=0, point_diff_adj=0)
    return(data.frame(t))
  }
}
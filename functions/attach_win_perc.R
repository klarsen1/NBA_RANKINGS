attach_win_perc <- function(data, w1, w2){
  
  ## the full dataframe
  dd <- mutate(data, rowid=row_number())
  
  d <- left_join(select(dd, selected_team, opposing_team, rowid), select(w1, -opposing_team), by="selected_team") %>%
  rename(winrate_early_season_selected_team=win_rate_early_season, 
         winrate_season_selected_team=win_rate_season,
         point_diff_selected_team_season=point_diff_season, 
         point_diff_selected_team_early_season=point_diff_early_season, 
         point_diff_selected_team_adj=point_diff_adj, 
         winrate_season_selected_team_adj=win_rate_season_adj) %>%
  left_join(select(w1, -first_game, -selected_team), by="opposing_team") %>%
  rename(winrate_early_season_opposing_team=win_rate_early_season, 
         winrate_season_opposing_team=win_rate_season, 
         point_diff_opposing_team_season=point_diff_season, 
         point_diff_opposing_team_early_season=point_diff_early_season, 
         point_diff_opposing_team_adj=point_diff_adj,
         winrate_season_opposing_team_adj=win_rate_season_adj) %>%
  left_join(select(w2, -first_game, -opposing_team, -point_diff_season, -point_diff_early_season, -win_rate_season_adj, -point_diff_adj), by="selected_team") %>%
  rename(winrate_early_season_selected_team_s=win_rate_early_season, 
         winrate_season_selected_team_s=win_rate_season) %>%
  left_join(select(w2, -first_game, -selected_team, -point_diff_season, -point_diff_early_season, -win_rate_season_adj, -point_diff_adj), by="opposing_team") %>%
  rename(winrate_early_season_opposing_team_s=win_rate_early_season, 
         winrate_season_opposing_team_s=win_rate_season) %>%
  replace(is.na(.), 0) %>%
  mutate(win_trend_opposing=ifelse(winrate_season_opposing_team>0, winrate_season_opposing_team_s/winrate_season_opposing_team,1), 
         win_trend_selected=ifelse(winrate_season_selected_team>0, winrate_season_selected_team_s/winrate_season_selected_team, 1)) %>%  
  select(-winrate_early_season_opposing_team_s, -winrate_early_season_selected_team_s, -selected_team, -opposing_team) %>%
  ungroup()
  
  dd <- inner_join(dd, d, by="rowid") %>% select(-rowid)
  
  return(dd)
}
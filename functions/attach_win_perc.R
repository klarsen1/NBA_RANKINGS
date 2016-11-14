attach_win_perc <- function(data, w1, w2){
  d <- left_join(data, select(w1, -opposing_team), by="selected_team") %>%
  rename(winrate_early_season_selected_team=win_rate_early_season, 
         winrate_season_selected_team=win_rate_season,
         point_diff_selected_season=point_diff_season, 
         point_diff_selected_early_season=point_diff_early_season) %>%
  left_join(select(w1, -first_game, -selected_team), by="opposing_team") %>%
  rename(winrate_early_season_opposing_team=win_rate_early_season, 
         winrate_season_opposing_team=win_rate_season, 
         point_diff_opposing_season=point_diff_season, 
         point_diff_opposing_early_season=point_diff_early_season) %>%
  left_join(select(w2, -first_game, -opposing_team, -point_diff_season, -point_diff_early_season), by="selected_team") %>%
  rename(winrate_early_season_selected_team_s=win_rate_early_season, 
         winrate_season_selected_team_s=win_rate_season) %>%
  left_join(select(w2, -first_game, -selected_team, -point_diff_season, -point_diff_early_season), by="opposing_team") %>%
  rename(winrate_early_season_opposing_team_s=win_rate_early_season, 
         winrate_season_opposing_team_s=win_rate_season) %>%
  replace(is.na(.), 0) %>%
  select(-winrate_early_season_opposing_team_s, -winrate_early_season_selected_team_s) %>%
  ungroup()
  return(d)
}
save_results <- function(root){
   details <- mutate(game_level, d_road_team_predicted_win=ifelse(selected_team==road_team_name, d, 1-d), 
                     d_home_team_predicted_win=1-d_road_team_predicted_win, 
                     predicted_winner=ifelse(d_road_team_predicted_win==1, road_team_name, home_team_name),
                     actual_winner=ifelse(is.na(selected_team_win), "NA", ifelse(selected_team_win==1, selected_team, opposing_team)),
                     home_team_prob_win=ifelse(selected_team==home_team_name, prob_selected_team_win_d, 1-prob_selected_team_win_d), 
                     road_team_prob_win=1-home_team_prob_win) %>%
             select(DATE, home_team_name, road_team_name, d_road_team_predicted_win, road_team_prob_win, d_home_team_predicted_win, home_team_prob_win, predicted_winner, actual_winner)
  write.csv(ranks, paste0(root, "/rankings/rankings_",Sys.Date(), ".csv"))
  write.csv(details, paste0(root,"/rankings/game_level_predictions_",Sys.Date(), ".csv"))
  write.csv(clusters_and_players, paste0(root, "/modeldetails/cluster_details_",Sys.Date(), ".csv"))
  write.csv(models, paste0(root, "/modeldetails/coefficients_", Sys.Date(), ".csv"))
  write.csv(parts, paste0(root, "/modeldetails/score_decomp_", Sys.Date(), ".csv"))
}
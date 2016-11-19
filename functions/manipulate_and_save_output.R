manipulate_and_save_output <- function(clusters_and_players, scores, game_level, model_parts, model_details, root, back_test, save){
  if (back_test==0){
     game_level <- data.frame(rbindlist(scores), stringsAsFactors = FALSE) %>% 
       mutate(d_pred_selected_team_win=ifelse(current_roster_used==0, selected_team_win, ifelse(is.na(selected_team_win), as.numeric(prob_selected_team_win_d>0.5), selected_team_win)),
           prob_selected_team_win=ifelse(current_roster_used==0, selected_team_win, ifelse(is.na(selected_team_win), prob_selected_team_win_d, selected_team_win)))
     ranks <- report(game_level, "d_pred_selected_team_win") %>%
       left_join(conferences, by="team") %>%
       select(team, games_season, games_played, pred_season_win_rate, ytd_win_rate, pred_win_rate, conference, division)
     models <- data.frame(rbindlist(model_details), stringsAsFactors = FALSE)
     parts <- data.frame(rbindlist(model_parts), stringsAsFactors = FALSE)
     details <- mutate(game_level, 
                    d_road_team_predicted_win=ifelse(is.na(d_pred_selected_team_win), NA, ifelse(selected_team==road_team_name, d_pred_selected_team_win, 1-d_pred_selected_team_win)), 
                    d_home_team_predicted_win=ifelse(is.na(d_pred_selected_team_win), NA, 1-d_road_team_predicted_win), 
                    predicted_winner=ifelse(is.na(d_pred_selected_team_win), "NA", ifelse(d_road_team_predicted_win==1, road_team_name, home_team_name)),
                    actual_winner=ifelse(is.na(selected_team_win), "NA", ifelse(selected_team_win==1, selected_team, opposing_team)),
                    home_team_prob_win=ifelse(is.na(d_pred_selected_team_win), NA, ifelse(selected_team==home_team_name, prob_selected_team_win_d, 1-prob_selected_team_win_d)), 
                    road_team_prob_win=ifelse(is.na(d_pred_selected_team_win), NA, 1-home_team_prob_win)) %>%
    select(DATE, home_team_name, road_team_name, road_team_prob_win, home_team_prob_win, predicted_winner, actual_winner)
    if (save==1){
      write.csv(ranks, paste0(root, "/rankings/rankings_",Sys.Date(), ".csv"), row.names = FALSE)
      write.csv(details, paste0(root,"/rankings/game_level_predictions_",Sys.Date(), ".csv"), row.names = FALSE)
      write.csv(clusters_and_players, paste0(root, "/modeldetails/cluster_details_",Sys.Date(), ".csv"), row.names = FALSE)
      write.csv(models, paste0(root, "/modeldetails/coefficients_", Sys.Date(), ".csv"), row.names = FALSE)
      write.csv(parts, paste0(root, "/modeldetails/score_decomp_", Sys.Date(), ".csv"), row.names = FALSE)
    }
    return(list(game_level, ranks, models, details))
  } else{
    game_level <- data.frame(rbindlist(scores), stringsAsFactors = FALSE) %>% 
      mutate(prob_selected_team_win=ifelse(current_roster_used==0, NA, prob_selected_team_win_d), 
             d_pred_selected_team_win=ifelse(current_roster_used==0, NA, as.numeric(prob_selected_team_win>0.5)))
    ranks <- report(filter(game_level, current_roster_used==1), "d_pred_selected_team_win")
    models <- data.frame(rbindlist(model_details), stringsAsFactors = FALSE)
    return(list(game_level, ranks, models))
  }
}
manipulate_and_save_output <- function(clusters_and_players, scores, model_parts, model_details, root, back_test, save, overwrite_date=NA, predvar){
  
  ## Get the conferences
  conferences <- read.csv(paste0(root, "/rawdata/Conferences.csv"), stringsAsFactors = FALSE)
  
  Date <- Sys.Date()
  if (is.na(overwrite_date)==FALSE){
    Date <- overwrite_date
  } 
  if (back_test==0){
    
     ft8 <- read.csv(paste0(root, "/rawdata/FiveThirtyEight_current.csv"), stringsAsFactors = FALSE) %>%
       rename(team=selected_team) %>%
       dplyr::select(team, pred_win_rate_538)
     
     game_level <- data.frame(scores, stringsAsFactors = FALSE) %>% 
       filter(playoffs==0) %>%
       dplyr::select(-prob_selected_team_win_b) %>%
       mutate(prob_selected_team_win=ifelse(current_season_data_used==0, NA, prob_selected_team_win_d))
     
     ranks <- report(game_level, predvar) %>%
       left_join(conferences, by="team") %>%
       dplyr::select(team, games_season, games_played, games_future, season_win_rate, ytd_win_rate, ytd_pred_win_rate, future_win_rate, conference, division) %>%
       left_join(ft8, by="team") %>%
       arrange(conference, -season_win_rate) %>%
       group_by(conference) %>%
       mutate(seed=row_number()) %>%
       ungroup()
     
     models <- data.frame(rbindlist(model_details), stringsAsFactors = FALSE)
     parts <- data.frame(rbindlist(model_parts), stringsAsFactors = FALSE)
     
     details <- mutate(game_level, 
                    actual_winner=ifelse(is.na(selected_team_win), "NA", ifelse(selected_team_win==1, selected_team, opposing_team)),
                    home_team_prob_win=ifelse(is.na(prob_selected_team_win), NA, ifelse(selected_team==home_team_name, prob_selected_team_win_d, 1-prob_selected_team_win_d)), 
                    road_team_prob_win=ifelse(is.na(prob_selected_team_win), NA, 1-home_team_prob_win)) %>%
     dplyr::select(DATE, home_team_name, road_team_name, selected_team, road_team_prob_win, home_team_prob_win, prob_selected_team_win, selected_team_win, actual_winner, current_season_data_used, future_game)
       
    if (save==1){
      write.csv(ranks, paste0(root, "/rankings/rankings_",Date, ".csv"), row.names = FALSE)
      write.csv(details, paste0(root,"/rankings/game_level_predictions_",Date, ".csv"), row.names = FALSE)
      write.csv(clusters_and_players, paste0(root, "/modeldetails/cluster_details_",Date, ".csv"), row.names = FALSE)
      write.csv(models, paste0(root, "/modeldetails/coefficients_", Date, ".csv"), row.names = FALSE)
      write.csv(parts, paste0(root, "/modeldetails/score_decomp_", Date, ".csv"), row.names = FALSE)
    }
    return(list(details, ranks, models, parts))
  } else{
    game_level <- data.frame(scores, stringsAsFactors = FALSE) %>% 
      filter(playoffs==0) %>%
      dplyr::select(-prob_selected_team_win_b) %>%
      mutate(prob_selected_team_win=ifelse(current_season_data_used==0, NA, prob_selected_team_win_d))
    ranks <- report(game_level, predvar)
    models <- data.frame(rbindlist(model_details), stringsAsFactors = FALSE)
    return(list(game_level, ranks, models))
  }
}
save_results <- function(root){
  write.csv(ranks, paste0(root, "/rankings/rankings_",Sys.Date(), ".csv"))
  write.csv(details, paste0(root,"/rankings/game_level_predictions_",Sys.Date(), ".csv"))
  write.csv(clusters_and_players, paste0(root, "/modeldetails/cluster_details_",Sys.Date(), ".csv"))
  write.csv(models, paste0(root, "/modeldetails/coefficients_", Sys.Date(), ".csv"))
  write.csv(parts, paste0(root, "/modeldetails/score_decomp_", Sys.Date(), ".csv"))
  #write.csv(title_chances, paste0(root, "/rankings/title_probabilities_", Sys.Date(), ".csv"))
}
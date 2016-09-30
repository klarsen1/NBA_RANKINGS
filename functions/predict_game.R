predict_game(X, b, gamedata, team1, team2){
  ### First get the average minutes and standard deviations for each player
  players <- filter(gamedata, selected_team==team1 | opposing_team==team2) %>%
    group_by(FULL_PLAYER_NAME) %>%
    

  ## 
  
  
}
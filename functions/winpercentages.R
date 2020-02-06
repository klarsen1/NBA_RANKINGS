winpercentages <- function(data, s, use_weights){

  df <- subset(data, season==s) %>% distinct(game_id, .keep_all=TRUE) %>%
    select(selected_team, opposing_team, selected_team_win, season_day_std, wins_538_selected_team, wins_538_opposing_team, carm_elo_full_opposing_team, carm_elo_full_selected_team)

  MM <- month(max(subset(data, season==s)$DATE))
  SS <- filter(data, DATE==max(DATE))[1,"season_day_std"]
  
  if (nrow(df)>0){
    all_teams <- distinct(data, OWN_TEAM)
    winrates <- list()
    for (k in 1:nrow(all_teams)){
      t <- as.character(all_teams[k,"OWN_TEAM"])
      if (use_weights==1){
        df <- mutate(df, weight=ifelse(selected_team==t, carm_elo_full_opposing_team, carm_elo_full_selected_team))
        #print("--- Using 538 weights")
      }  else{
        df$weight <- 1
      }
      winrate <- filter(df, selected_team==t | opposing_team==t) %>%
        ungroup() 

      winrate <- 
        mutate(winrate, win=ifelse(selected_team==t, selected_team_win, 1-selected_team_win)) 

      winrate <- 
        summarise(winrate, 
                  win_rate=weighted.mean(win, weight), 
                  n=n()) 
      
      winrate <- 
        mutate(winrate, 
               opposing_team=t, 
               selected_team=t)

      winrates[[k]] <- winrate
    }
    winratesdf <- data.frame(rbindlist(winrates), stringsAsFactors=FALSE) %>%
        mutate(month=MM,
               win_rate_season=win_rate,
               win_rate_season_adj=win_rate_season*as.numeric(SS),
               first_game=0) %>%
      select(selected_team, opposing_team, win_rate_season, first_game, win_rate_season_adj) %>%
      replace(is.na(.), 0)
    return(winratesdf)
  } else{
    t <- distinct(data, selected_team) %>%
      mutate(win_rate_season=0, opposing_team=selected_team, first_game=1, win_rate_season_adj=0)
    return(data.frame(t))
  }
}
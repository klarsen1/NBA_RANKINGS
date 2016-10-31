weighted_winpercentages <- function(data, s){

  df <- subset(data, season==s) %>% distinct(game_id, .keep_all=TRUE) %>%
    select(selected_team, opposing_team, selected_team_win)
  
  if (nrow(df)>0){
    all_teams <- distinct(data, OWN_TEAM)
    winrates <- list()
    for (k in 1:nrow(all_teams)){
      t <- all_teams[k,"OWN_TEAM"]
      winrate <- filter(df, selected_team==t | opposing_team==t) %>%
        ungroup() %>%
        mutate(win=ifelse(selected_team==t, selected_team_win, 1-selected_team_win)) %>%
        summarise(win_rate=mean(win), n=n()) %>%
        mutate(opposing_team=t, 
               selected_team=t)
      winrates[[k]] <- winrate
    }
    winratesdf1 <- data.frame(rbindlist(winrates), stringsAsFactors=FALSE)
    
    df1 <- inner_join(df, select(winratesdf1, win_rate, selected_team), by="selected_team") %>%
      rename(winrate_selected_team=win_rate)
    df2 <- inner_join(df1, select(winratesdf1, win_rate, opposing_team), by="opposing_team") %>%
      rename(winrate_opposing_team=win_rate) %>%
    select(selected_team, selected_team_win, opposing_team, winrate_selected_team, winrate_opposing_team)  
    rm(winrates)

    winrates <- list()
    for (k in 1:nrow(all_teams)){
      t <- all_teams[k,"OWN_TEAM"]
      winrate <- filter(df2, selected_team==t | opposing_team==t) %>%
        ungroup() %>%
        mutate(win=ifelse(selected_team==t, selected_team_win*winrate_opposing_team, (1-selected_team_win)*winrate_selected_team)) %>%
        summarise(w_win_rate=mean(win)) %>%
        mutate(selected_team=t)
      winrates[[k]] <- winrate
    }
    winratesdf2 <- data.frame(rbindlist(winrates), stringsAsFactors=FALSE) %>%
      inner_join(winratesdf1, by="selected_team") %>%
      mutate(early_season=as.numeric(n<9),
             win_rate_early_season=win_rate*as.numeric(early_season==1),
             win_rate_season=win_rate*as.numeric(early_season==0),
             w_win_rate=w_win_rate*as.numeric(early_season==0),
             first_game=0) %>%
      select(selected_team, opposing_team, win_rate_early_season, win_rate_season, first_game)
    
    return(winratesdf2)
  } else{
    t <- distinct(data, selected_team) %>%
      mutate(win_rate_early_season=0, win_rate_season=0, opposing_team=selected_team, first_game=1)
    return(data.frame(t))
  }
}
weighted_winpercentages <- function(data, s){

  df <- filter(data, season==s) %>% distinct(game_id, .keep_all=TRUE)
  
  all_teams <- distinct(data, OWN_TEAM)
  
  if (nrow(df)>0){
    winrates <- list()
    for (k in 1:nrow(all_teams)){
      t <- all_teams[k,"OWN_TEAM"]
      winrate <- filter(df, selected_team==t | opposing_team==t) %>%
        ungroup() %>%
        mutate(win=ifelse(selected_team==t, selected_team_win, 1-selected_team_win)) %>%
        summarise(win=mean(win)) %>%
        mutate(opposing_team=t, 
               selected_team=t)
      winrates[[k]] <- winrate
    }
    winratesdf <- data.frame(rbindlist(winrates), stringsAsFactors=FALSE)
    
    df1 <- inner_join(df, select(winratesdf, win, selected_team, n), by="selected_team") %>%
      rename(winrate_selected_team=win, n1=n)
    df1 <- inner_join(df, select(winratesdf, win, opposing_team, n), by="opposing_team") %>%
      rename(winrate_opposing_team=win, n2=n) 
    
    winrates <- list()
    for (k in 1:nrow(all_teams)){
      t <- all_teams[k,"OWN_TEAM"]
      winrate <- filter(df1, selected_team==t | opposing_team==t) %>%
        ungroup() %>%
        mutate(win=ifelse(selected_team==t, selected_team_win*winrate_opposing_team, (1-selected_team_win)*winrate_selected_team)) %>%
        summarise(win=mean(win)) %>%
        mutate(opposing_team=t, 
               selected_team=t)
      winrates[[k]] <- winrate
    }
    winratesdf <- data.frame(rbindlist(winrates), stringsAsFactors=FALSE)
    
    df2 <- inner_join(df1, select(winratesdf, win, selected_team), by="selected_team") %>%
      rename(w_winrate_selected_team=win)
    df2 <- inner_join(df1, select(winratesdf, win, opposing_team), by="opposing_team") %>%
      rename(w_winrate_opposing_team=win) %>%
      mutate(early_season=as.numeric(n1<9), 
             win_perc_delta=ifelse(early_season==1, 0, w_winrate_selected_team-w_winrate_opposing_team))
    
      return(df2)
  } else{
    t <- all_teams %>%
      mutate(win_perc_delta=0, early_season=1, weighted_win_perc1=0, weighted_win_perc2=0, wins1=0, wins2=0, n1=0, n2=0) %>% 
      select(-right)
    return(data.frame(t))
  }
}
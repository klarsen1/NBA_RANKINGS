weighted_winpercentages <- function(data, s){

  df <- subset(data, season==s) %>% distinct(game_id, .keep_all=TRUE)
  
  if (nrow(df)>0){
    all_teams <- distinct(data, OWN_TEAM)
    winrates <- list()
    for (k in 1:nrow(all_teams)){
      t <- all_teams[k,"OWN_TEAM"]
      winrate <- filter(df, selected_team==t | opposing_team==t) %>%
        ungroup() %>%
        mutate(win=ifelse(selected_team==t, selected_team_win, 1-selected_team_win)) %>%
        summarise(win_rate=mean(win), wins=sum(win), n=n()) %>%
        mutate(opposing_team=t, 
               selected_team=t)
      winrates[[k]] <- winrate
    }
    winratesdf <- data.frame(rbindlist(winrates), stringsAsFactors=FALSE)
    
    df1 <- inner_join(df, select(winratesdf, win_rate, wins, selected_team, n), by="selected_team") %>%
      rename(winrate_selected_team=win_rate, n1=n, wins_selected_team=wins)
    df2 <- inner_join(df1, select(winratesdf, win_rate, wins, opposing_team, n), by="opposing_team") %>%
      rename(winrate_opposing_team=win_rate, n2=n, wins_opposing_team=wins) %>%
    select(selected_team, selected_team_win, opposing_team, winrate_selected_team, winrate_opposing_team, wins_selected_team, wins_opposing_team, n1, n2)  
    rm(winratesdf)
    rm(winrates)

    winrates <- list()
    for (k in 1:nrow(all_teams)){
      t <- all_teams[k,"OWN_TEAM"]
      winrate <- filter(df2, selected_team==t | opposing_team==t) %>%
        ungroup() %>%
        mutate(win=ifelse(selected_team==t, selected_team_win*winrate_opposing_team, (1-selected_team_win)*winrate_selected_team)) %>%
        summarise(w_win_rate=mean(win)) %>%
        mutate(opposing_team=t, 
               selected_team=t)
      winrates[[k]] <- winrate
    }
    winratesdf <- data.frame(rbindlist(winrates), stringsAsFactors=FALSE)

    df3 <- inner_join(df2, select(winratesdf, w_win_rate, selected_team), by="selected_team") %>%
      rename(w_winrate_selected_team=w_win_rate)
    df4 <- inner_join(df3, select(winratesdf, w_win_rate, opposing_team), by="opposing_team") %>%
      rename(w_winrate_opposing_team=w_win_rate) %>%
      mutate(early_season=as.numeric(n1<9), 
             win_perc_delta=ifelse(early_season==1, 0, w_winrate_selected_team-w_winrate_opposing_team)) %>%
      select(selected_team, early_season, w_winrate_selected_team, w_winrate_opposing_team, winrate_selected_team, winrate_opposing_team, win_perc_delta, n1, n2)
    return(df4)
  } else{
    t <- distinct(data, selected_team) %>%
      mutate(early_season=1, w_winrate_selected_team=0, w_winrate_opposing_team=0, winrate_selected_team=0, winrate_opposing_team=0, win_perc_delta=0, n1=0, n2=0)
    return(data.frame(t))
  }
}
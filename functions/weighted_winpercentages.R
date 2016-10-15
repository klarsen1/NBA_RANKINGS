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
    
    df2 <- inner_join(df1, select(winratesdf, win, selected_team, n), by="selected_team") %>%
      rename(winrate_selected_team=win, n1=n)
    df <- inner_join(df, select(winratesdf, win, opposing_team, n), by="opposing_team") %>%
      rename(winrate_opposing_team=win, n2=n) 
    
    
    %>%
      mutate(early_season=as.numeric(n1<9), 
             win_perc_delta=ifelse(early_season==1, 0, ))
    
  }
  
  ### Calculate the percentages  
  if (nrow(df)==0){
    t <- all_teams %>%
      mutate(win_perc_delta=0, early_season=1, weighted_win_perc1=0, weighted_win_perc2=0, wins1=0, wins2=0, n1=0, n2=0) %>% 
      select(-right)
    return(data.frame(t))
  } else{
    df <- distinct(df, game_id, .keep_all=TRUE)
    
    win_perc_opposing <- group_by(df, opposing_team) %>% 
      summarise(win_opposing=mean(as.numeric(selected_team_win==0))) %>%
      ungroup()

    win_perc1 <- group_by(df, selected_team) %>% 
      inner_join(win_perc_opposing, by="opposing_team") %>%
      mutate(w=selected_team_win*(win_opposing+1.00)) %>%
      summarise(n=sum(w), d=sum(win_opposing+1.00), n1=n(), wins1=sum(selected_team_win)) %>%
      mutate(weighted_win_perc1=ifelse(d>0, as.numeric(n/d), 0)) %>%
      replace(is.na(.), 0) %>%
      select(weighted_win_perc1, selected_team, n1, wins1) %>%
      ungroup()

   rm(win_perc_opposing)

    ### Get weighted win percentages for the opposing team
    win_perc_opposing <- group_by(df, selected_team) %>% 
      summarise(win_opposing=mean(selected_team_win)) %>%
      ungroup() %>%
      rename(opposing_team=selected_team)

    win_perc2 <- group_by(df, opposing_team) %>% 
      inner_join(win_perc_opposing, by="opposing_team") %>%
      mutate(w=as.numeric(selected_team_win==0)*(win_opposing+1.00)) %>%
      summarise(n=sum(w), d=sum(win_opposing+1.00), n2=n(), wins2=sum(as.numeric(selected_team_win==0))) %>%
      mutate(weighted_win_perc2=ifelse(d>0, as.numeric(n/d), 0)) %>%
      replace(is.na(.), 0) %>%
      select(weighted_win_perc2, opposing_team, n2, wins2) %>%
      ungroup() %>%
      rename(selected_team=opposing_team)

    win_perc <- inner_join(win_perc1, win_perc2, by="selected_team") %>%
      full_join(all_teams, by="selected_team") %>%
      mutate(early_season=ifelse(is.na(n1), 1, as.numeric(n1<9)),
             win_perc_delta = ifelse(early_season==1, 0, weighted_win_perc1 - weighted_win_perc2)) %>%
      select(selected_team, win_perc_delta, early_season, weighted_win_perc1, weighted_win_perc2, wins1, wins2, n1, n2) %>%
      replace(is.na(.), 0)
    
    return(data.frame(win_perc))
  }
}
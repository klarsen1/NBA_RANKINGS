weighted_winpercentages <- function(data){

  win_perc_opposing <- group_by(thisdata, opposing_team) %>% 
    summarise(win_opposing=mean(as.numeric(selected_team_win==0))) %>%
    ungroup()

  win_perc1 <- group_by(thisdata, selected_team) %>% 
    inner_join(win_perc_opposing, by="opposing_team") %>%
    mutate(w=selected_team_win*win_opposing) %>%
    summarise(n=sum(w), d=sum(win_opposing)) %>%
    mutate(weighted_win_perc1=ifelse(d>0, n/d, 0)) %>%
    replace(is.na(.), 0) %>%
    select(weighted_win_perc1, selected_team) %>%
    ungroup()

  rm(win_perc_opposing)

  ### Get weighted win percentages for the opposing team
  win_perc_opposing <- thisdata %>% group_by(selected_team) %>% 
    summarise(win_opposing=mean(selected_team_win)) %>%
    ungroup() %>%
    rename(opposing_team=selected_team)

  win_perc2 <- thisdata %>% group_by(opposing_team) %>% 
    inner_join(win_perc_opposing, by="opposing_team") %>%
    mutate(w=as.numeric(selected_team_win==0)*win_opposing) %>%
    summarise(n=sum(w), d=sum(win_opposing)) %>%
    mutate(weighted_win_perc2=ifelse(d>0, n/d, 0)) %>%
    replace(is.na(.), 0) %>%
    select(weighted_win_perc2, opposing_team) %>%
    ungroup() %>%
    rename(selected_team=opposing_team)

  win_perc <- inner_join(win_perc1, win_perc2, by="selected_team") %>%
    mutate(win_perc_delta = weighted_win_perc1 - weighted_win_perc2) %>%
    select(selected_team, win_perc_delta)
  
  return(win_perc)
}
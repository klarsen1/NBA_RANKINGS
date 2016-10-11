report <- function(scores){

  ### Summarize team performances -- have to do this twice because the data was collapsed into matchups
  summary1 <- group_by(scores, selected_team) %>%
    summarise(win=sum(selected_team_win),
              pred_win=sum(prob_selected_team_win_b),
              games=n()) %>%
    rename(team=selected_team) %>%
  ungroup()

  summary2 <- group_by(scores, opposing_team) %>%
    summarise(win=sum(as.numeric(selected_team_win==0)),
              pred_win=sum(1-prob_selected_team_win_b),
              games=n()) %>%
    rename(team=opposing_team) %>%
  ungroup()
  
  summary <- bind_rows(summary1, summary2) %>% 
     group_by(team) %>%
     summarise_each(funs(sum)) %>%
     mutate(win_rate = win/games, 
            pred_win_rate = pred_win/games) %>%
     arrange(pred_win_rate) %>%
     select(team, games, win_rate, pred_win_rate) %>%
  ungroup()
     
  return(data.frame(summary))   
}
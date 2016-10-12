report <- function(scores, type=1){

  ### Pick the type of score
  if (type==1){
    scores$pred <- scores$prob_selected_team_win_d
  } else{
    scores$pred <- scores$prob_selected_team_win_b
  }
  
  ### Summarize team performances -- have to do this twice because the data was collapsed into matchups
  summary1 <- group_by(scores, selected_team) %>%
    summarise(win=sum(selected_team_win),
              pred_win=sum(pred),
              games=n()) %>%
    rename(team=selected_team) %>%
  ungroup()

  summary2 <- group_by(scores, opposing_team) %>%
    summarise(win=sum(as.numeric(selected_team_win==0)),
              pred_win=sum(1-pred),
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
     mutate(rank_actual=min_rank(-win_rate), 
            rank_pred=min_rank(-pred_win_rate)) %>%
  ungroup()
     
  return(data.frame(summary))   
}
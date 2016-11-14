##### Run the playoffs
## Need to run the season first using run_model.R

inwindow <- filter(box_scores_plus, DATE_INDEX<=max_real_date) 
thisseason <- filter(inwindow, DATE==max(DATE))[1,"season"]
win_perc1 <- winpercentages(filter(inwindow, DATE_INDEX>datemap[max_real_date-winstreak_window, "DATE_INDEX"]), thisseason)
win_perc2 <- winpercentages(filter(inwindow, DATE_INDEX>datemap[max_real_date-winstreak_window_s, "DATE_INDEX"]), thisseason)


ncore <- detectCores()-1
registerDoParallel(ncore)
loop_result <- foreach(p=1:100) %dopar% {
  playoffs <- sim_playoff(ranks, inwindow, playing_time_window, win_perc1, win_perc2, datemap, 1, "/Users/kimlarsen/Documents/Code/NBA_RANKINGS", c, max_real_date, thisseason, end_date)
  winner <- subset(playoffs[[1]], status=="W")$team
  return(data.frame(p, winner))
}

title_chances <- data.frame(rbindlist(loop_result)) %>% group_by(winner) %>%
  summarise(n=n()) %>%
  mutate(prob_win_title=n/100) %>%
  select(-n)


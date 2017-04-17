##### Run the playoffs
## Need to run the season first using run_model.R

source("/Users/kim.larsen/Documents/Code/NBA_RANKINGS/functions/sim_playoffs.R")
library(stringr)
library(stringi)

inwindow <- filter(box_scores_plus, DATE_INDEX<=max_real_date & DATE_INDEX>max_real_date-playing_time_window+1)
thisseason <- filter(inwindow, DATE==max(DATE))[1,"season"]
win_perc1 <- winpercentages(inwindow, thisseason, w)
win_perc2 <- win_perc1

inwindow_active <- mutate(inwindow,
                          today=as.Date(end_date),                        
                          injured=ifelse(is.na(injury_status), 0, ifelse(today>=injury_scrape_date & today<return_date, 1, 0))
)
injured_players <- unique(subset(inwindow_active, injured==1)$PLAYER_FULL_NAME)
if (length(injured_players)>0){
  print(paste0("Injuries: ", injured_players))
  inwindow_active <- filter(inwindow_active, injured==0)
}

sims <- 100
ncore <- detectCores()-1
registerDoParallel(ncore)
loop_result <- foreach(p=1:sims) %dopar% {
  playoffs <- sim_playoff(results[[2]], inwindow_active, playing_time_window, win_perc1, win_perc2, datemap, 1, "/Users/kim.larsen/Documents/Code/NBA_RANKINGS", c, max_real_date, thisseason, end_date)
  #winner <- subset(playoffs[[1]], status=="W")$team
  winner <- playoffs[[2]]
  return(data.frame(p, winner))
}

title_chances <- data.frame(rbindlist(loop_result)) %>% group_by(round, winner) %>%
  summarise(n=n()) %>%
  mutate(prob_win=n/sims) %>%
  select(-n)

View(title_chances)
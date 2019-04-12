##### Run the playoffs
## Need to run the season first using run_model.R

root <- "/Users/kim.larsen/Documents/Code/NBA_RANKINGS"

source(paste0(root, "/functions/sim_playoffs.R"))
library(stringr)
library(stringi)

playoff_start_date <- as.Date("2019-04-14") ## faking it a bit here

runs <- 0

rankings <- results[[2]] %>% 
  mutate(season_win_rate=ifelse(team=="San Antonio", season_win_rate+0.0001, ifelse(team=="Portland", season_win_rate+0.0001, season_win_rate))) %>% 
  arrange(season_win_rate)


inwindow <- filter(box_scores_plus, DATE_INDEX<=max_real_date & DATE_INDEX>max_real_date-playing_time_window+1)
thisseason <- filter(inwindow, DATE==max(DATE))[1,"season"]
win_perc1 <- winpercentages(inwindow, thisseason, w)
win_perc2 <- win_perc1


inwindow_active <- mutate(inwindow,
                          today=as.Date(end_date),                        
                          injured=ifelse(is.na(injury_status), 0, ifelse(playoff_start_date>=injury_scrape_date & playoff_start_date<=return_date, 1, 0))
)
injured_players <- unique(subset(inwindow_active, injured==1)$PLAYER_FULL_NAME)
if (length(injured_players)>0){
  print(paste0("Injuries: ", sort(injured_players)))
  inwindow_active <- filter(inwindow_active, injured==0)
}

combine <- function(x, ...) {
  lapply(seq_along(x),
         function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
}

ncore <- detectCores()-1
registerDoParallel(ncore)
sims <- 1
loopResult <- foreach(i=1:sims, .combine='combine', .multicombine=TRUE,
                      .init=list(list(), list(), list())) %dopar% {
  playoffs <- sim_playoff(rankings, inwindow_active, playing_time_window, win_perc1, win_perc2, datemap, runs, root, c, max_real_date, thisseason, end_date, seed=1000*i + runif(1)*1000)
  playoffs[[2]]$sim <- i
  return(list(playoffs[[2]], playoffs[[3]], data.frame(playoffs[[1]])))
}

full_results <- data.frame(rbindlist(loopResult[[1]]))
r <- max(full_results$round)
m <- max(full_results$matchup)


coin_flips <- list()
f <- 1
for (i in 1:r){
  for (j in 1:m){
    d <- data.frame(filter(full_results, round==i & matchup==j))
    if (nrow(d)>0){
       dd <- filter(d, winner != "NONE")
       t1 <- dd$winner
       t2 <- dd$loser
       d <- filter(d, game<5)
       p <- d$prob_selected_team_win
       s <- d$selected_team
       n1 <- 0
       n2 <- 0
       p <- c(p, p[1], p[3], p[1])
       s <- c(s, s[1], s[3], s[1])
       for (k in 1:20000){
         nn1 <- 0
         nn2 <- 0
         for (g in 1:length(p)){
             binomial <- as.numeric(rbinom(n=1, size=1, prob=p[g]))
             if (t1==s[g]){
                if (binomial==1){nn1 <- nn1+1}
                else {nn2 <- nn2+1}
             } else{
               if (binomial==1){nn2 <- nn2+1}
               else {nn1<- nn1+1}
             }
         }
         if (nn1>nn2){
            n1 <- n1+1
          } else{
            n2 <- n2+1
          }
       }
       prob_win <- n1/(n1+n2)
       simresult <- data.frame(matrix(nrow=1, ncol=7))
       names(simresult) <- c("round", "matchup", "selected_team", "winner", "loser", "prob_win", "sims")
       simresult$round <- i
       simresult$matchup <- j
       simresult$selected_team <- s[1]
       simresult$winner <- t1
       simresult$loser <- t2
       simresult$prob_win <- prob_win
       simresult$sims <- n1+n2
       coin_flips[[f]] <- simresult
       f <- f+1
    }
  }
}

coin_flip_results <- data.frame(rbindlist(coin_flips)) %>% arrange(winner, round, matchup)


winners <- data.frame(rbindlist(loopResult[[1]])) %>%
  group_by(round, matchup) %>%
  mutate(r=row_number(), n=n()) %>%
  filter(r==n) %>%
  select(round, matchup, winner, loser) %>%
  rename(final_winner=winner, final_loser=loser)

ranks <- arrange(data.frame(loopResult[[3]]), conference, season_win_rate) %>%
  rename(final_winner=team) %>%
  select(final_winner, conference, season_win_rate)

final_results <- data.frame(rbindlist(loopResult[[1]])) %>%
  inner_join(winners, by=c("round", "matchup")) %>%
  group_by(round, matchup, final_winner, final_loser) %>%
  mutate(spread=2*abs(prob_selected_team_win-.5)) %>%
  summarise(games=n(), win_spread=mean(spread)) %>%
  inner_join(ranks, by="final_winner") %>%
  arrange(round, conference, -season_win_rate)



decomps <- data.frame(rbindlist(loopResult[[2]])) %>%
  inner_join(winners, by=c("round", "matchup")) %>%
  mutate(s=ifelse(selected_team==final_winner, 1, -1)) %>%
  group_by(round, matchup, final_winner, final_loser) %>%
  summarise(performance=mean(performance*s), roster=mean(roster*s), circumstances=mean(circumstances))



#write.csv(decomps, "/Users/kim.larsen/Documents/Code/NBA_RANKINGS/modeldetails/2017_playoff_decomp.csv", row.names = FALSE)

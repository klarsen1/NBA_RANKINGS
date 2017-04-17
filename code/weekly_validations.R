setwd("/Users/kim.larsen/Documents/Code/NBA_RANKINGS/rankings/")

actuals <- read.csv("game_level_predictions_2017-04-14.csv") %>% 
  select(DATE, road_team_name, home_team_name, actual_winner) %>%
  filter(is.na(actual_winner)==FALSE) %>%
  mutate(DATE=as.Date(DATE))

validate_week <- function(date){
  pred <- read.csv(paste0("game_level_predictions_", date, ".csv")) %>% 
    mutate(DATE=as.Date(DATE)) %>%
    select(DATE, road_team_name, home_team_name, predicted_winner) %>%
    filter(DATE<=as.Date(date)+6 & is.na(predicted_winner)==FALSE)
  fit <- inner_join(pred, actuals, by=c("DATE", "road_team_name", "home_team_name")) %>%
    mutate(Week=date, 
           Match=ifelse(actual_winner==predicted_winner, 1, 0)) %>%
    select(Week, Match)
  return(fit)
}

accuracy <- list()
#run_date <- "2016-11-27"
run_date <- "2016-12-04"
for (i in 1:10){
  accuracy[[i]] <- validate_week(run_date)
  accuracy[[i]]$week <- run_date
  run_date <- as.character(as.Date(run_date) + 7)
  print(run_date)
}

final <- data.frame(rbindlist(accuracy)) %>%
  filter(week != "2017-01-22") %>% # something went wrong this week
  summarise(match=mean(Match))

summary(final)


f <-"/Users/kim.larsen/Documents/Code/NBA_RANKINGS/modeldetails/score_decomp_2017-02-12.csv"
#f <-"/Users/kim.larsen/Documents/Code/NBA_RANKINGS/modeldetails/score_decomp_2017-04-14.csv"

center <- function(x){return(x-median(x))}
read.csv(f, stringsAsFactors = FALSE) %>%
  select(selected_team, roster, circumstances, performance) %>%
  group_by(selected_team) %>%
  summarise_each(funs(mean)) %>% ## get averages across games by team
  ungroup() %>%
  mutate_each(funs(center), which(sapply(., is.numeric))) %>% ## standardize across teams
  gather(modelpart, value, roster:performance) %>% ## transpose
  rename(team=selected_team) %>%
  ggplot(aes(team, value)) + geom_bar(aes(fill=modelpart), stat="identity") + coord_flip() +
  xlab("") + ylab("") + theme(legend.title = element_blank())

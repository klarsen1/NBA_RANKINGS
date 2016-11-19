library(xlsx)
library(dplyr)
library(readxl)
library(stringi)
library(tidyr)
library(reshape2)
library(data.table)
library(dplyr)
library(ggmap)
library(parallel)
library(foreach)
library(doParallel)
library(rvest)

ft8 <- read_html("http://projects.fivethirtyeight.com/2017-nba-predictions/") %>%
  html_nodes("#standings-table") %>% html_table(fill=TRUE)
ft8df <- data.frame(rbindlist(ft8))
team <- gsub("[0-9, -]", "", ft8df[4:nrow(ft8df),"V5"])
elo <- ft8df[4:nrow(ft8df),"V1"]
carm_elo <- ft8df[4:nrow(ft8df),"V2"]
team[team=="ers"] <- "Philadelphia"
team[team=="Hornets"] <- "Charlotte"
team[team=="Clippers"] <- "LA Clippers"
team[team=="Cavaliers"] <- "Cleveland"
team[team=="Warriors"] <- "Golden State"
team[team=="Spurs"] <- "San Antonio"
team[team=="Raptors"] <- "Toronto"
team[team=="Jazz"] <- "Utah"
team[team=="Thunder"] <- "Oklahoma City"
team[team=="TrailBlazers"] <- "Portland"
team[team=="Rockets"] <- "Houston"
team[team=="Pelicans"] <- "New Orleans"
team[team=="Celtics"] <- "Boston"
team[team=="Timberwolves"] <- "Minnesota"
team[team=="Bulls"] <- "Chicago"
team[team=="Hawks"] <- "Atlanta"
team[team=="Pistons"] <- "Detroit"
team[team=="Nuggets"] <- "Denver"
team[team=="Mavericks"] <- "Dallas"
team[team=="Wizards"] <- "Washington"
team[team=="Lakers"] <- "LA Lakers"
team[team=="Kings"] <- "Sacramento"
team[team=="Knicks"] <- "New York"
team[team=="Grizzlies"] <- "Memphis"
team[team=="Pacers"] <- "Indiana"
team[team=="Bucks"] <- "Milwaukee"
team[team=="Magic"] <- "Orlando"
team[team=="Heat"] <- "Miami"
team[team=="Suns"] <- "Phoenix"
team[team=="Nets"] <- "Brooklyn"

fivethirtyeight <- data.frame(cbind(team, elo, carm_elo), stringsAsFactors = FALSE) %>%
  mutate(selected_team=as.character(team), opposing_team=as.character(team), 
         elo=elo, carm_elo=carm_elo) %>%
  select(-team)

fivethirtyeight$elo <- as.numeric(fivethirtyeight$elo)
fivethirtyeight$carm_elo <- as.numeric(fivethirtyeight$carm_elo)


source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/distance_between.R")

setwd("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/rawdata/")

stats_page <- read_html("http://www.nbastuffer.com/2016-2017_NBA_Regular_Season_Player_Stats.html")

players <- stats_page %>%
  html_nodes("tbody#PLAYER tr td:nth-child(2)") %>%
  html_text()

teams <- stats_page %>%
  html_nodes("tbody#PLAYER tr td:nth-child(3)") %>%
  html_text()

rosters <- data.frame(
  PLAYER_FULL_NAME = players,
  NBAstuffer.Initials = teams)
rosters$NBAstuffer.Initials <- as.character(rosters$NBAstuffer.Initials)
PLAYER_FULL_NAME <- as.character(rosters$PLAYER_FULL_NAME)

team_map <- data.frame(read_excel("schedule.xlsx", sheet=2)) %>% 
  select(City, NBAstuffer.Initials) %>% distinct(NBAstuffer.Initials, .keep_all=TRUE)

rosters <- inner_join(rosters, team_map, by="NBAstuffer.Initials") %>%
  rename(OWN_TEAM=City) %>%
  select(OWN_TEAM, PLAYER_FULL_NAME) %>%
  arrange(OWN_TEAM, PLAYER_FULL_NAME)

write.csv(rosters, "./current_rosters.csv", row.names = FALSE)
write.csv(rosters, paste0("./rosters_", Sys.Date(), ".csv"), row.names = FALSE)
write.csv(fivethirtyeight, paste0("FiveThirtyEight_", Sys.Date(), ".csv"), row.names = FALSE)


ncore <- detectCores()-1
registerDoParallel(ncore)


## Read the raw data
read_player_data <- function(season, first_labels, suffix){
  data <- data.frame(read_excel(paste0("s", suffix, ".xlsx"), sheet=1))
  meta <- data.frame(read_excel(paste0("s", suffix, ".xlsx"), sheet=2, col_names = FALSE))
  labels <- c(first_labels, meta$X1)
  attr(data, "variable.labels") <- labels
  n <- gsub("_$", "", gsub("__", "_", gsub(".", "_", names(data), fixed=T)))
  names(data) <- n
  data <- rename(data, 
                 points=PTS, 
                 assists=A,
                 offensive_rebounds=OR,
                 defensive_rebounds=DR, 
                 turnovers=TO,
                 threepointers_made=X3P, 
                 steals=ST,
                 minutes=MIN, 
                 threepoint_attempts=X3PA, 
                 fieldgoal_attempts=FGA, 
                 fieldgoals_made=FG,
                 freethrows_made=FT,
                 freethrow_attempts=FTA, 
                 fouls=PF, 
                 blocks=BL)
  return(data)
}

s1 <- read_player_data("NBA-2012-2013", c("SEASON", "DATE", "PLAYER FULL NAME", "POSITION"), 1)
s2 <- read_player_data("NBA-2013-2014", c("SEASON", "DATE", "PLAYER FULL NAME", "POSITION"), 2)
s3 <- read_player_data("NBA-2014-2015", c("SEASON", "DATE", "PLAYER FULL NAME", "POSITION"), 3)
s4 <- read_player_data("NBA-2015-2016", c("SEASON", "DATE", "PLAYER FULL NAME", "POSITION"), 4)
s5 <- read_player_data("NBA-2016-2017", c("SEASON", "DATE", "PLAYER FULL NAME", "POSITION"), 5) 
  

## Add some indicators
f <- rbind.data.frame(s1, s2, s3, s4, s5) %>%
     filter(is.na(DATA_SET)==FALSE) %>%
     mutate(home_team=as.numeric(VENUE_R_H=='H'), 
            road_team=as.numeric(VENUE_R_H=='R'), 
            playoffs=as.numeric(substr(DATA_SET, 6, 13)=="Playoffs"),
            season=ifelse(playoffs==0, as.numeric(substr(DATA_SET, 1, 4)), as.numeric(substr(DATA_SET, 1, 4))-1), 
            playoff_minutes=playoffs*minutes,
            playoff_points=playoffs*points,
            DATE=as.Date(DATE, format="%m/%d/%Y"),
            quarter=quarter(DATE),
            future_game=0,
            OWN_TEAM=ifelse(OWN_TEAM=="LA", "LA Clippers", OWN_TEAM),
            OPP_TEAM=ifelse(OPP_TEAM=="LA", "LA Clippers", OPP_TEAM))

max_date <- max(f$DATE)

## Get the altitudes
altitudes <- data.frame(read.csv("altitudes.csv", stringsAsFactors = FALSE))

## Read the schedule
schedule <- data.frame(read_excel("schedule.xlsx", sheet=1))

home <- rename(schedule, NBAstuffer.Initials=HOME) %>% 
  inner_join(team_map, by="NBAstuffer.Initials") %>%
  rename(home_team=City) %>%
  mutate(DATE=as.Date(Date, format="%m/%d/%Y")) %>%
  select(home_team)

road <- rename(schedule, NBAstuffer.Initials=ROAD) %>% 
  inner_join(team_map, by="NBAstuffer.Initials") %>%
  rename(road_team=City) %>% 
  mutate(DATE=as.Date(Date, format="%m/%d/%Y")) %>%
  select(road_team, DATE)

set.seed(2015)

future_schedule <- data.frame(cbind(home, road)) %>% filter(DATE>max_date) %>%
  mutate(r=runif(n()),
         future_game=1,
         OWN_TEAM=ifelse(r>0.5, home_team, road_team),
         OPP_TEAM=ifelse(OWN_TEAM==home_team, road_team, home_team),
         VENUE_R_H=ifelse(OWN_TEAM==home_team, 'H', 'R'), 
         DATA_SET="2016-2017 Regular Season", 
         season=2016,
         PLAYER_FULL_NAME="BLANK") %>%
  select(DATE, OWN_TEAM, OPP_TEAM, VENUE_R_H, DATA_SET, future_game, PLAYER_FULL_NAME, season)
  
f <- bind_rows(f, future_schedule) %>%
  replace(is.na(.), 0)
  

setwd("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/cleandata")

## Create an ID
f$cat <- paste0(f$OWN_TEAM, f$OPP_TEAM)
striHelper <- function(x) stri_c(x[stri_order(x)], collapse = "")
f$game_id <- paste0(f$DATE, vapply(stri_split_boundaries(f$cat, type = "character"), striHelper, ""))
f$cat <- NULL

## Team/game level points
team_pts <- group_by(f, game_id, OWN_TEAM, OPP_TEAM, VENUE_R_H, DATE, future_game, season) %>%
            summarise(total_playoff_minutes=sum(minutes*playoffs),
                      total_playoff_points=sum(points*playoffs),
                      total_minutes=sum(minutes), 
                      total_points=sum(points),
                      playoffs=max(playoffs)) %>%
            ungroup()

## Game level points
game_pts <- group_by(team_pts, game_id, future_game, season) %>%
            mutate(home_team_points=total_points*(VENUE_R_H=='H'), 
                   road_team_points=total_points*(VENUE_R_H=='R')) %>%
            summarise(max_game_points=max(total_points), 
                      home_team_points=sum(home_team_points), 
                      road_team_points=sum(road_team_points)) %>%
            ungroup()

## Random indicator to choose the selected team
game_pts$r <- as.numeric(rbinom(nrow(game_pts), 1, 0.5)>0.5)

## Create win indicators at the game/team level            
team_win <- inner_join(team_pts, select(game_pts, game_id, max_game_points, r), by="game_id") %>%
            mutate(win=as.numeric(total_points==max_game_points)) %>%
            select(-max_game_points)

city_names <- distinct(team_win, OWN_TEAM) %>% mutate(OWN_TEAM_NAME=OWN_TEAM) %>%
  mutate(OWN_TEAM_NAME=ifelse(OWN_TEAM_NAME=="Golden State", "Oakland", OWN_TEAM_NAME), 
         OWN_TEAM_NAME=ifelse(OWN_TEAM_NAME=="Minnesota", "Minneapolis", OWN_TEAM_NAME),
         OWN_TEAM_NAME=ifelse(OWN_TEAM_NAME=="LA Clippers", "Los Angeles", OWN_TEAM_NAME), 
         OWN_TEAM_NAME=ifelse(OWN_TEAM_NAME=="Indiana", "Indianapolis", OWN_TEAM_NAME), 
         OWN_TEAM_NAME=ifelse(OWN_TEAM_NAME=="LA Lakers", "Los Angeles", OWN_TEAM_NAME), 
         OWN_TEAM_NAME=ifelse(OWN_TEAM_NAME=="Washington", "Washington, DC", OWN_TEAM_NAME), 
         OWN_TEAM_NAME=ifelse(OWN_TEAM_NAME=="Utah", "Salt Lake City", OWN_TEAM_NAME)) %>%
  arrange(OWN_TEAM_NAME)

city_lat_long <- cbind(rbindlist(lapply(split(city_names$OWN_TEAM_NAME, city_names$OWN_TEAM_NAME), function(x) return(geocode(as.character(x))))), city_names$OWN_TEAM)
names(city_lat_long) <- c("lon","lat","OWN_TEAM")


## Create win indicators at the game level
game_win <- group_by(team_win, game_id, DATE, future_game, season) %>%
            mutate(selected_team_win=ifelse(r==1, win*(VENUE_R_H=='H'), win*(VENUE_R_H=='R'))) %>%
            summarise(selected_team_win=max(selected_team_win),
                      playoffs=max(playoffs)) %>%
            ungroup()

## Create a game level summary file to be saved
future_flipped <- filter(team_win, future_game==1) %>%
  mutate(VENUE_R_H2=ifelse(VENUE_R_H=='H', 'R', 'H'),
         OWN_TEAM2=OPP_TEAM, 
         OPP_TEAM2=OWN_TEAM) %>%
  select(-OPP_TEAM, -OWN_TEAM) %>%
  rename(OPP_TEAM=OPP_TEAM2, 
         OWN_TEAM=OWN_TEAM2,
         VENUE_R_H=VENUE_R_H2)
  
team_win <- bind_rows(team_win, future_flipped) %>% arrange(DATE, game_id)

split <- split(team_win, team_win$game_id)
game_scores <- data.frame(rbindlist(lapply(split, function(x) spread(select(x, game_id, VENUE_R_H, OWN_TEAM), VENUE_R_H, OWN_TEAM))), stringsAsFactors = FALSE) %>%
                   inner_join(select(game_pts, -max_game_points, -future_game, -season), by="game_id") %>%
                   inner_join(game_win, by="game_id") %>%
                   mutate(selected_team=ifelse(r==1, H, R), 
                          opposing_team=ifelse(r==1, R, H), 
                          selected_team_win=ifelse(future_game==1, NA, selected_team_win)) %>% 
                   select(-r, -future_game) %>%
                   rename(home_team_name=H, road_team_name=R) %>%
  ungroup()

saveRDS(game_scores, "GAME_SCORES.RDA")

get_rest_days <- function(id){

  selected <- subset(game_scores, game_id==id)$selected_team
  opposing <- subset(game_scores, game_id==id)$opposing_team
  
  t <- rename(altitudes, OWN_TEAM=team, selected_team_altitude=altitude)
  
  df1 <- subset(game_scores, home_team_name==selected | road_team_name==selected) %>% 
    arrange(DATE) %>%
    mutate(days_since_last_game=DATE-lag(DATE), 
           start_of_season=ifelse(days_since_last_game>14 | is.na(days_since_last_game)==TRUE, 1, 0),
           selected_team_rest=ifelse(start_of_season==1, 0, days_since_last_game),
           selected_team_last_city=ifelse(start_of_season==1, selected, lag(home_team_name)), 
           OWN_TEAM=home_team_name) %>%
    inner_join(city_lat_long, by="OWN_TEAM") %>%
    rename(lat1=lat, lon1=lon) %>%
    mutate(OWN_TEAM=selected_team_last_city) %>%
    inner_join(city_lat_long, by="OWN_TEAM") %>%
    rename(lat2=lat, lon2=lon) %>%
    mutate(selected_team_travel=ifelse(selected_team_rest>2 & home_team_name==selected, 0, abs(distance_between(lon1,lat1,lon2,lat2)))) %>%
    filter(game_id==id) %>%
    left_join(t, by="OWN_TEAM") %>%
    select(selected_team_rest, selected_team_last_city, selected_team_travel, selected_team_altitude)
 
  t <- rename(altitudes, OWN_TEAM=team, opposing_team_altitude=altitude)
  
  df2 <- subset(game_scores, home_team_name==opposing | road_team_name==opposing) %>% 
    arrange(DATE) %>%
    mutate(days_since_last_game=as.numeric(DATE-lag(DATE)), 
           start_of_season=ifelse(days_since_last_game>14 | is.na(days_since_last_game)==TRUE, 1, 0),
           opposing_team_rest=ifelse(start_of_season==1, 0, days_since_last_game),
           opposing_team_last_city=ifelse(start_of_season==1, opposing, lag(home_team_name)),
           OWN_TEAM=home_team_name) %>%
    inner_join(city_lat_long, by="OWN_TEAM") %>%
    rename(lat1=lat, lon1=lon) %>%
    mutate(OWN_TEAM=opposing_team_last_city) %>%
    inner_join(city_lat_long, by="OWN_TEAM") %>%
    rename(lat2=lat, lon2=lon) %>%
    mutate(opposing_team_travel=ifelse(opposing_team_rest>2 & home_team_name==opposing, 0, abs(distance_between(lon1,lat1,lon2,lat2)))) %>%
    filter(game_id==id) %>%
    left_join(t, by="OWN_TEAM") %>%
    select(opposing_team_rest, opposing_team_last_city, game_id, opposing_team_travel, opposing_team_altitude)

  return(data.frame(cbind(df1, df2)))
}


ids <- unique(game_scores$game_id)
loop_result <- foreach(i=1:length(ids)) %dopar% {
  return(get_rest_days(ids[i]))
}
rest_days <- data.frame(rbindlist(loop_result), stringsAsFactors = FALSE) %>% 
  mutate(rest_differential=selected_team_rest-opposing_team_rest, 
         travel_differential=opposing_team_travel-selected_team_travel) %>%
  select(game_id, rest_differential, travel_differential, opposing_team_travel, opposing_team_rest, selected_team_rest, selected_team_travel, selected_team_last_city, opposing_team_last_city, selected_team_altitude, opposing_team_altitude)

#dateindex <- distinct(f, DATE) %>% mutate(DATE_INDEX=row_number())

games_last_week <- function(id){
  game <- filter(game_scores, game_id==id)
  t1 <- game$selected_team
  t2 <- game$opposing_team
  date <- game$DATE
  s <- game$season
  
  team1 <- filter(game_scores, DATE<date & DATE>date-6 & season==s) %>%
    filter(selected_team==t1 | opposing_team==t1) %>%
    arrange(DATE) %>%
    ungroup() %>%
    mutate(back2back=as.numeric(DATE-lag(DATE)==1)) %>%
    replace(is.na(.), 0) %>%
    summarise(back2back=sum(back2back))
    
  team2 <- filter(game_scores, DATE<date & DATE>date-6 & season==s) %>%
    filter(selected_team==t2 | opposing_team==t2) %>%
    arrange(DATE) %>%
    ungroup() %>%
    mutate(back2back=as.numeric(DATE-lag(DATE)==1)) %>%
    replace(is.na(.), 0) %>%
    summarise(back2back=sum(back2back))

  if (nrow(team1)>0 & nrow(team2)){
    df <- data.frame(cbind(id, team1$back2back, team2$back2back))
  } else{
    df <- data.frame(cbind(id, 0, 0))
  }
  names(df) <- c("game_id", "selected_team_games_prior_7d", "opposing_team_games_prior_7d")
  df$game_id <- as.character(df$game_id)
  return(df)
}

loop_result <- foreach(i=1:length(ids)) %dopar% {
  return(games_last_week(ids[i]))
}

trailing_games <- data.frame(rbindlist(loop_result), stringsAsFactors = FALSE)

## Check previous matchups
games <- unique(game_scores$game_id)
loop_result <- foreach(i=1:length(games)) %dopar% {
  t <- subset(game_scores, game_id==games[i])
  selected <- t$selected_team
  opposing <- t$opposing_team
  date <- t$DATE
  s <- t$season
  
  matchups <- filter(game_scores, selected_team %in% c(selected, opposing) & opposing_team %in% c(selected, opposing)) %>%
    filter(season==s & DATE<date) %>%
    select(-game_id)
  
  if (nrow(matchups)==0){
    df <- data.frame(0, 0, as.character(games[i]))
    names(df) <- c("selected_team_matchup_wins", "opposing_team_matchup_wins", "game_id")
  } else{
    df <- mutate(matchups, 
                 w1=ifelse(selected_team==selected, selected_team_win, 1-selected_team_win), 
                 w2=1-w1) %>%
      summarise(selected_team_matchup_wins=sum(w1), opposing_team_matchup_wins=sum(w2)) %>%
      mutate(t=selected_team_matchup_wins+opposing_team_matchup_wins,
             selected_team_matchup_wins=selected_team_matchup_wins/t,
             opposing_team_matchup_wins=opposing_team_matchup_wins/t,
             game_id=as.character(games[i])) %>%
      select(-t)
  }
  return(df)
}

prev_matchups <- data.frame(rbindlist(loop_result), stringsAsFactors=FALSE) %>% replace(is.na(.), 0)
prev_matchups$game_id <- as.character(prev_matchups$game_id)



## Create the fill box score file
final <- inner_join(f, select(team_win, -DATE, -VENUE_R_H, -r, -playoffs, -OPP_TEAM, -future_game, -season), by=c("game_id", "OWN_TEAM")) %>%
     inner_join(select(game_scores, -DATE, -playoffs, -season), by="game_id") %>%
     inner_join(rest_days, by="game_id") %>%
     inner_join(trailing_games, by="game_id") %>%
     inner_join(prev_matchups, by="game_id") %>%
     mutate(share_of_minutes=minutes/total_minutes, 
            share_of_playoff_minutes=ifelse(total_playoff_minutes>0, playoff_minutes/total_playoff_minutes, 0),
            share_of_playoff_points=ifelse(total_playoff_points>0, playoff_points/total_playoff_points, 0),
            share_of_points=points/total_points,
            #home_points=home_team*points, 
            #road_points=road_team*points,
            share_of_minutes_signed = ifelse(OWN_TEAM==selected_team, share_of_minutes, -share_of_minutes),
            home_team_selected = as.numeric(home_team_name==selected_team),
            selected_team_points=ifelse(home_team_selected==1, home_team_points, road_team_points),
            opposing_team_points=ifelse(home_team_selected==0, home_team_points, road_team_points),
            win=ifelse(future_game==1, NA, win)) %>%
     dplyr::select(-VENUE_R_H, -TOT) %>% arrange(DATE, game_id) %>%
     #inner_join(dateindex, by="DATE") %>%
     left_join(select(fivethirtyeight, -opposing_team), by="selected_team") %>%
     rename(elo_selected_team=elo, carm_elo_selected_team=carm_elo) %>%
     left_join(select(fivethirtyeight, -selected_team), by="opposing_team") %>%
     rename(elo_opposing_team=elo, carm_elo_opposing_team=carm_elo) %>%
     ungroup()

saveRDS(final, "BOX_SCORES.RDA")

rm(list = ls())
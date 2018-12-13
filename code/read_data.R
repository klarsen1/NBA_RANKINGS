library(xlsx)
library(dplyr)
library(readxl)
library(stringi)
library(tidyr)
library(data.table)
library(dplyr)
library(ggmap)
library(parallel)
library(foreach)
library(doParallel)
library(rvest)
library(stringr)

root <- "/Users/thirdlovechangethisname/Documents/Code/NBA_RANKINGS"
current_season <- "2018-2019 Regular Season"
current_season_numeric <- 2018

setwd(root)

source(paste0(root, "/functions/distance_between.R"))
source(paste0(root, "/functions/trim.R"))

setwd(paste0(root, "/rawdata/"))

city_lat_long <- read.csv("city_lat_long.csv", stringsAsFactors = FALSE)
city_lat_long$OWN_TEAM <- city_lat_long$OWN_TEAM %>% gsub("L.A.", "LA", .) %>% trim()

team_map <- data.frame(read_excel("schedule.xlsx", sheet=2)) %>% 
  rename(Team=FULL.NAME, NBAstuffer.Initials=SHORT.NAME, City=CITY) %>%
  mutate(OWN_TEAM=NBAstuffer.Initials, OWN_TEAM_NAME=OWN_TEAM) %>%
  distinct(Team, .keep_all=TRUE) %>% select(City, NBAstuffer.Initials, Team, OWN_TEAM_NAME, OWN_TEAM) %>%
  filter(!(Team %in% c("Charlotte Bobcats", "New Orleans Hornets")))

  
  

### 538 data
ft8 <- read_html("http://projects.fivethirtyeight.com/2019-nba-predictions/")
team <- ft8 %>% html_nodes("tbody tr td.team a") %>% html_text() %>% gsub("[0-9, -]", "", .) %>% trim()
wins <- ft8 %>% html_nodes("tbody tr td.proj-rec") %>% html_text() %>% gsub('-[0-9]+','', .) %>% trim()
losses <- ft8 %>% html_nodes("tbody tr td.proj-rec") %>% html_text() %>% gsub('[0-9]+-','', .) %>% trim()
#elo <- ft8 %>% html_nodes("tbody tr td.elo.original") %>% html_text()
carm_elo <- ft8 %>% html_nodes("tbody tr td.carmelo") %>% html_text() %>% trim()
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
#wins <- rep(0, length(carm_elo))
#losses <- rep(0, length(carm_elo))
elo <- rep(0, length(carm_elo))
fivethirtyeight <- data.frame(team, elo=as.numeric(elo), 
                              carm_elo=as.numeric(carm_elo), 
                              wins_538=as.numeric(wins), 
                              losses_538=as.numeric(losses), 
                              stringsAsFactors = FALSE) %>%
  mutate(selected_team=as.character(team), opposing_team=as.character(team), 
         elo=elo, carm_elo=carm_elo, 
         pred_win_rate_538=wins_538/(wins_538+losses_538)) %>%
  select(-team)


### Injury return dates from CBS
cbs_injuries <- read_html("http://www.cbssports.com/nba/injuries/daily")

PLAYER_FULL_NAME <- cbs_injuries %>% html_nodes(".CellPlayerName--long a") %>% html_text()

return_notes <- cbs_injuries %>% html_nodes(".TableBase-bodyTd:nth-child(5)") %>% html_text() %>% gsub("\n ","", .) %>% trim()

daily_injuries <- data.frame(PLAYER_FULL_NAME, 
                             return_notes, 
                             stringsAsFactors = FALSE) %>%
  mutate(clean_note=gsub("Expected to be out until at least ", "", return_notes))

convert_to_date <- function(data){
  if (data$clean_note=="Out for the season"){
    data$return_date <- Sys.Date() + 365
  } else if (data$clean_note=="Game Time Decision"){
    data$return_date <- Sys.Date() + 1
  } else{
    data$return_date <- as.Date(data$clean_note, format="%b %d")
  }
  if (data$return_date<Sys.Date()){
    data$return_date <- data$return_date + 365
  }
  return(data)
}

daily_injuries <- data.frame(rbindlist(lapply(split(daily_injuries, daily_injuries$PLAYER_FULL_NAME), convert_to_date)), stringsAsFactors = FALSE) %>%
  select(PLAYER_FULL_NAME, clean_note, return_date) %>% distinct(PLAYER_FULL_NAME, .keep_all=TRUE)


### Injury status from ESPN
source_injuries <- read_html("http://espn.go.com/nba/injuries")

players <- source_injuries %>%
  html_nodes('table tr.oddrow a, table tr.evenrow a') %>%
  html_text()

statuses <- source_injuries %>%
  html_nodes('table tr.oddrow td:nth-child(2), table tr.evenrow  td:nth-child(2)') %>%
  html_text()

players <- players[players != "Vincent Goodwill of NBC Sports Chicago"]

dates <- source_injuries %>%
  html_nodes('table tr.oddrow td:nth-child(3), table tr.evenrow  td:nth-child(3)') %>%
  html_text()

injuries <- data.frame(
  PLAYER_FULL_NAME = players,
  injury_status = statuses,
  injury_date = dates, 
  stringsAsFactors = FALSE
) %>% arrange(PLAYER_FULL_NAME, desc(injury_date)) %>% 
  distinct(PLAYER_FULL_NAME, .keep_all=TRUE) %>%
  full_join(daily_injuries, by="PLAYER_FULL_NAME") %>%
  distinct(PLAYER_FULL_NAME, .keep_all=TRUE) %>%
  mutate(injury_scrape_date=Sys.Date())

injuries[is.na(injuries$return_date),"return_date"] <- Sys.Date()+1
injuries[is.na(injuries$injury_status),"injury_status"] <- "Out"

team_pages <- read_html("http://www.espn.com/nba/teams") %>%
  html_nodes(".nowrap:nth-child(3) a") %>% html_attr("href")

rosters <- lapply(team_pages, function (team_link) {
  team_link <- paste0('http://www.espn.com', team_link)
  team_roster <- read_html(team_link)
  
  team_name <- team_roster %>% html_nodes(".headline__h1") %>% html_text() %>% gsub("Roster", "", .) %>% trim()
  if (team_name=="LA Clippers") {team_name<-"Los Angeles Clippers"}
  if (team_name=="Portland Trail Blazers") {team_name<-"Portland Trailblazers"}

  Team <- team_name
  PLAYER_FULL_NAME <- team_roster %>% html_nodes(".Table2__td:nth-child(2)") %>% html_text()
  Age <- as.numeric(team_roster %>% html_nodes(".Table2__td:nth-child(4)") %>% html_text())
  Weight <- as.numeric(team_roster %>% html_nodes(".Table2__td:nth-child(6)") %>% html_text() %>% gsub("lbs", "", .) %>% trim())
  Height <- team_roster %>% html_nodes(".Table2__td:nth-child(5)") %>% html_text() %>% gsub("lbs", "", .) %>% trim()
  Salary <- team_roster %>% html_nodes(".Table2__td:nth-child(8)") %>% html_text()
  Salary <- as.numeric(gsub(',','',gsub('\\$', '', Salary)) %>% trim())
  Position <- team_roster %>% html_nodes(".Table2__td:nth-child(3)") %>% html_text()
  return(data.frame(Team, Position, PLAYER_FULL_NAME, Age, Weight, Height, Salary))
})

all_rosters <- bind_rows(lapply(rosters, function(x) as.data.frame(x))) %>%
  left_join(team_map, by="Team") %>%
  select(PLAYER_FULL_NAME, OWN_TEAM, Position, Age, Height, Weight, Salary, Team) %>%
  arrange(PLAYER_FULL_NAME, OWN_TEAM) %>%
  left_join(injuries, by="PLAYER_FULL_NAME") %>%
  distinct(PLAYER_FULL_NAME, .keep_all=TRUE) %>%
  mutate(OWN_TEAM=gsub("L.A.", "LA", OWN_TEAM))
 

## Save scraped data
write.csv(all_rosters, "rosters_current.csv", row.names = FALSE)
write.csv(all_rosters, paste0("rosters_", Sys.Date(), ".csv"), row.names = FALSE)
write.csv(fivethirtyeight, paste0("FiveThirtyEight_", Sys.Date(), ".csv"), row.names = FALSE)
write.csv(fivethirtyeight, paste0("FiveThirtyEight_current.csv"), row.names = FALSE)
write.csv(injuries, paste0("injuries_", Sys.Date(), ".csv"), row.names = FALSE)
write.csv(injuries, "injuries_current.csv", row.names = FALSE)


## Register cores
ncore <- detectCores()-2
registerDoParallel(ncore)

data <- data.frame(read_excel(paste0("s", "7", ".xlsx"), sheet=1))
n <- gsub("_$", "", gsub("__", "_", gsub(".", "_", names(data), fixed=T))) %>% gsub("__","_", .)
names(data) <- n
if ("DATASET" %in% names(data)) {data$DATA_SET <- data$DATASET}
if ("OPPONENT_TEAM" %in% names(data)) {data$OPP_TEAM <- data$OPPONENT_TEAM}


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
               blocks=BL) %>% select(DATA_SET, DATE, PLAYER_FULL_NAME, POSITION, OWN_TEAM, OPP_TEAM, VENUE_R_H, TOT, points, 
                                     assists, offensive_rebounds, defensive_rebounds, turnovers, threepointers_made, 
                                     steals, minutes, threepoint_attempts, fieldgoal_attempts, fieldgoals_made, freethrows_made,
                                     freethrow_attempts, fouls, blocks)


## Read the raw data
read_player_data <- function(season, first_labels, suffix){
  data <- data.frame(read_excel(paste0("s", suffix, ".xlsx"), sheet=1))
  meta <- data.frame(read_excel(paste0("s", suffix, ".xlsx"), sheet=2, col_names = FALSE))
  labels <- c(first_labels, meta$X1)
  attr(data, "variable.labels") <- labels
  n <- gsub("_$", "", gsub("__", "_", gsub(".", "_", names(data), fixed=T))) %>% gsub("__","_", .)
  names(data) <- n
  if ("DATASET" %in% names(data)) {data$DATA_SET <- data$DATASET}
  if ("OPPONENT_TEAM" %in% names(data)) {data$OPP_TEAM <- data$OPPONENT_TEAM}
  data$DATA_SET <- data$DATA_SET %>% gsub("NBA", "", .) %>% trim() 
  data$OPP_TEAM %>% trim()
  data$OWN_TEAM %>% trim()
  data$PLAYER_FULL_NAME %>% trim()
  data$file <- suffix
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
                 blocks=BL) %>% select(DATA_SET, DATE, PLAYER_FULL_NAME, POSITION, OWN_TEAM, OPP_TEAM, VENUE_R_H, TOT, points, 
                                       assists, offensive_rebounds, defensive_rebounds, turnovers, threepointers_made, 
                                       steals, minutes, threepoint_attempts, fieldgoal_attempts, fieldgoals_made, freethrows_made,
                                       freethrow_attempts, fouls, blocks, file)
  return(data)
}

s1 <- read_player_data("NBA-2012-2013", c("SEASON", "DATE", "PLAYER FULL NAME", "POSITION"), 1)
s2 <- read_player_data("NBA-2013-2014", c("SEASON", "DATE", "PLAYER FULL NAME", "POSITION"), 2)
s3 <- read_player_data("NBA-2014-2015", c("SEASON", "DATE", "PLAYER FULL NAME", "POSITION"), 3)
s4 <- read_player_data("NBA-2015-2016", c("SEASON", "DATE", "PLAYER FULL NAME", "POSITION"), 4)
s5 <- read_player_data("NBA-2016-2017", c("SEASON", "DATE", "PLAYER FULL NAME", "POSITION"), 5) 
s6 <- read_player_data("NBA-2017-2018", c("SEASON", "DATE", "PLAYER FULL NAME", "POSITION"), 6) 
s7 <- read_player_data("NBA-2018-2019", c("SEASON", "DATE", "PLAYER FULL NAME", "POSITION"), 7) 


## Add some indicators
f <- rbind.data.frame(s1, s2, s3, s4, s5, s6, s7) %>%
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
schedule_team_name_map <- data.frame(read_excel("schedule.xlsx", sheet=2)) %>% 
  mutate(home_team=SHORT.NAME, road_team=SHORT.NAME, 
         home_team=gsub("L.A.", "LA", home_team), 
         road_team=gsub("L.A.", "LA", road_team), 
         ROAD.TEAM=FULL.NAME, 
         HOME.TEAM=FULL.NAME)

schedule <- data.frame(read_excel("schedule.xlsx", sheet=1)) %>% mutate(DATE=as.Date(DATE)) %>%
  inner_join(select(schedule_team_name_map, HOME.TEAM, home_team), by="HOME.TEAM") %>%
  inner_join(select(schedule_team_name_map, ROAD.TEAM, road_team), by="ROAD.TEAM") %>%
  select(DATE, home_team, road_team)

set.seed(2015)

future_schedule <- filter(schedule, DATE>max_date) %>%
  mutate(r=runif(n()),
         future_game=1,
         OWN_TEAM=ifelse(r>0.5, home_team, road_team),
         OPP_TEAM=ifelse(OWN_TEAM==home_team, road_team, home_team),
         VENUE_R_H=ifelse(OWN_TEAM==home_team, 'H', 'R'), 
         DATA_SET=current_season, 
         season=current_season_numeric,
         PLAYER_FULL_NAME="BLANK") %>%
  select(DATE, OWN_TEAM, OPP_TEAM, VENUE_R_H, DATA_SET, future_game, PLAYER_FULL_NAME, season)
  
f <- bind_rows(f, future_schedule) %>%
  replace(is.na(.), 0)
  
setwd(paste0(root, "/cleandata"))

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

game_scores$home_team_name <- trim(game_scores$home_team_name)
game_scores$road_team_name <- trim(game_scores$road_team_name)
game_scores$selected_team <- trim(game_scores$selected_team)
game_scores$opposing_team <- trim(game_scores$opposing_team)

saveRDS(game_scores, "GAME_SCORES.RDA")

get_rest_days <- function(id){

  selected <- subset(game_scores, game_id==id)$selected_team %>% trim()
  opposing <- subset(game_scores, game_id==id)$opposing_team %>% trim()
  
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
    select(opposing_team_rest, opposing_team_last_city, game_id, opposing_team_travel, opposing_team_altitude, lon1, lon2, lat1, lat2)

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
    df <- data.frame(cbind(id, team1$back2back, team2$back2back), stringsAsFactors = FALSE)
  } else{
    df <- data.frame(cbind(id, 0, 0), stringsAsFactors = FALSE)
  }
  names(df) <- c("game_id", "selected_team_games_prior_7d", "opposing_team_games_prior_7d")
  df$game_id <- as.character(df$game_id)
  df$selected_team_games_prior_7d <- as.numeric(df$selected_team_games_prior_7d)
  df$opposing_team_games_prior_7d <- as.numeric(df$opposing_team_games_prior_7d)
  
  tt <- subset(game_scores, home_team_name==t1 & DATE<=game$DATE & season==game$season)
  if (nrow(tt)>0){
    t1_last_home_game <- max(tt$DATE)
    df$days_on_road_selected_team <- as.numeric(game$DATE - t1_last_home_game)
  } else{
    df$days_on_road_selected_team <- 0
  }
  rm(tt)
  ttt <- subset(game_scores, home_team_name==t2 & DATE<=game$DATE & season==game$season)
  if (nrow(ttt)>0){
    t2_last_home_game <- max(ttt$DATE)
    df$days_on_road_opposing_team <- as.numeric(game$DATE - t2_last_home_game)
  } else{
    df$days_on_road_opposing_team <- 0
  }
  rm(tt)
  rm(ttt)
  
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
            share_of_minutes_signed = ifelse(OWN_TEAM==selected_team, share_of_minutes, -share_of_minutes),
            home_team_selected = as.numeric(home_team_name==selected_team),
            selected_team_points=ifelse(home_team_selected==1, home_team_points, road_team_points),
            opposing_team_points=ifelse(home_team_selected==0, home_team_points, road_team_points),
            win=ifelse(future_game==1, NA, win)) %>%
     dplyr::select(-VENUE_R_H, -TOT) %>% arrange(DATE, game_id) %>%
     left_join(select(fivethirtyeight, elo, carm_elo, selected_team), by="selected_team") %>%
     rename(elo_selected_team=elo, carm_elo_selected_team=carm_elo) %>%
     left_join(select(fivethirtyeight, elo, carm_elo, opposing_team), by="opposing_team") %>%
     rename(elo_opposing_team=elo, carm_elo_opposing_team=carm_elo) %>%
     left_join(injuries, by="PLAYER_FULL_NAME") %>%
     mutate(travel_differential=if_else(is.na(travel_differential), 0, travel_differential), 
            opposing_team_travel=if_else(is.na(opposing_team_travel), 0, opposing_team_travel), 
            selected_team_travel=if_else(is.na(selected_team_travel), 0, opposing_team_travel)) %>%
     ungroup()

saveRDS(final, paste0("BOX_SCORES_", Sys.Date(), ".RDA"))
saveRDS(final, "BOX_SCORES.RDA")

#rm(list=ls())
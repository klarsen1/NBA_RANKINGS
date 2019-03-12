s <- read.csv("/Users/kim.larsen/Documents/Code/NBA_RANKINGS/rankings/game_level_predictions_2019-03-10.csv", stringsAsFactors = FALSE) %>%
  filter(DATE==as.Date("2019-03-12"))
View(s)

s <- read.csv("/Users/kim.larsen/Documents/Code/NBA_RANKINGS/rankings/game_level_predictions_2019-03-10.csv", stringsAsFactors = FALSE) %>%
  filter(DATE==as.Date("2019-03-13"))
View(s)

s <- read.csv("/Users/kim.larsen/Documents/Code/NBA_RANKINGS/rankings/game_level_predictions_2019-03-10.csv", stringsAsFactors = FALSE) %>%
  filter(DATE==as.Date("2019-03-21"))
View(s)

s <- read.csv("/Users/kim.larsen/Documents/Code/NBA_RANKINGS/rankings/game_level_predictions_2019-03-10.csv", stringsAsFactors = FALSE) %>%
  filter(DATE==as.Date("2019-03-26"))
View(s)

c <- read.csv("/Users/kim.larsen/Documents/Code/NBA_RANKINGS/modeldetails/cluster_details_2019-03-10.csv", stringsAsFactors = FALSE) %>%
  filter(PLAYER_FULL_NAME=="as.Date("2019-03-26")=="Stephen Curry"")
View(c)


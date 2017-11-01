library(dplyr)

### Settings
cutoff_season <- 2017 ## for example, 2015 cuts off the 2015-2016 season and later seasons
cutoff <- 8 # minutes per game. if a player plays less than this amount, he is excluded
nclus <- 25 # number of archetypes

### Read the raw data
setwd("/Users/kim.larsen/Documents/Code/NBA_RANKINGS/cleandata")
box_scores <- readRDS("BOX_SCORES.RDA")

### Get means for centroids
means <- box_scores %>%
  group_by(PLAYER_FULL_NAME) %>%
  filter(season<cutoff_season & playoffs==0) %>%
  summarise(assists=mean(assists),
            offensive_rebounds=mean(offensive_rebounds),
            defensive_rebounds=mean(defensive_rebounds),
            turnovers=mean(turnovers),
            threepointers_made=mean(threepointers_made), 
            steals=mean(steals),
            points=mean(points),
            minutes=mean(minutes),
            threepoint_attempts=mean(threepoint_attempts), 
            fieldgoal_attempts=mean(fieldgoal_attempts), 
            fieldgoals_made=mean(fieldgoals_made),
            freethrows_made=mean(freethrows_made),
            freethrow_attempts=mean(freethrow_attempts), 
            blocks=mean(blocks),
            share_of_minutes=mean(share_of_minutes)) %>%
  mutate(fieldgoal_percent=ifelse(fieldgoal_attempts>0, fieldgoals_made/fieldgoal_attempts, 0), 
         freethrow_percent=ifelse(freethrow_attempts>0, freethrows_made/freethrow_attempts, 0), 
         efficiency=(blocks + points + offensive_rebounds + defensive_rebounds + assists + steals - (fieldgoal_attempts - fieldgoals_made) - (freethrow_attempts - freethrows_made) - turnovers)) %>%
  replace(is.na(.), 0) %>%
  select(-threepoint_attempts, -freethrow_attempts, -fieldgoal_attempts, -points)

means_no_scrubs <- subset(means, minutes>cutoff)
length(unique(means_no_scrubs$PLAYER_FULL_NAME))
means_no_scrubs$minutes <- NULL
standardized <- scale(means_no_scrubs[,sapply(means_no_scrubs, is.numeric)])


#### Get the final centroids
setwd("/Users/kim.larsen/Documents/Code/NBA_RANKINGS/centroids")
set.seed(2015)
km <- kmeans(standardized, centers=nclus, nstart=25)

saveRDS(km$centers, "centroids.RDA")

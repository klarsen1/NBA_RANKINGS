assign_clusters <- function(centroids, data, cutoff){

  means <- group_by(data, PLAYER_FULL_NAME) %>%
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

  scrubs <- filter(means, minutes<=cutoff) %>% mutate(Cluster=0) %>% select(PLAYER_FULL_NAME, Cluster)
  means_no_scrubs <- filter(means, minutes>cutoff) %>% select(-PLAYER_FULL_NAME, -minutes)
  names <- subset(means, minutes>cutoff)$PLAYER_FULL_NAME

  scaled <- scale(means_no_scrubs)

  closest.cluster <- function(x) {
    cluster.dist <- apply(centroids, 1, function(y) sqrt(sum((x-y)^2)))
    return(which.min(cluster.dist)[1])
  }
  
  clusters <- data.frame(cbind(apply(scaled, 1, closest.cluster), names), stringsAsFactors = FALSE)
  names(clusters) <- c("Cluster", "PLAYER_FULL_NAME")

  clusters$Cluster <- as.numeric(clusters$Cluster)
  clusters <- bind_rows(clusters, scrubs)

  return(clusters)
}
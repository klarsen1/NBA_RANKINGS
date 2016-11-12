assign_clusters <- function(centroids, data, cutoff, thisseason){
  
  df <- mutate(data, gap=DATE_INDEX-min(DATE_INDEX)+1, weight=log(gap),
               weight=ifelse(season==thisseason, weight, weight*0.25)) 
    
  means <- group_by(df, PLAYER_FULL_NAME) %>%
  summarise(assists=weighted.mean(assists, weight),
            offensive_rebounds=weighted.mean(offensive_rebounds, weight),
            defensive_rebounds=weighted.mean(defensive_rebounds, weight),
            turnovers=weighted.mean(turnovers, weight),
            threepointers_made=weighted.mean(threepointers_made, weight), 
            steals=weighted.mean(steals, weight),
            points=weighted.mean(points, weight),
            minutes=weighted.mean(minutes, weight),
            threepoint_attempts=weighted.mean(threepoint_attempts, weight), 
            fieldgoal_attempts=weighted.mean(fieldgoal_attempts, weight), 
            fieldgoals_made=weighted.mean(fieldgoals_made, weight),
            freethrows_made=weighted.mean(freethrows_made, weight),
            freethrow_attempts=weighted.mean(freethrow_attempts, weight), 
            blocks=weighted.mean(blocks, weight),
            share_of_minutes=weighted.mean(share_of_minutes, weight)) %>%
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
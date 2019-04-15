library(dplyr)

root <- "/Users/kim.larsen/Documents/Code/NBA_RANKINGS"

coeff_details <- read.csv(paste0(root, "/modeldetails/coefficients_2019-04-11.csv")) %>%
  filter(DATE=="2019-04-10") %>%
  mutate(Cluster=as.numeric(gsub("share_minutes_cluster_", "", Variable))) %>%
  filter(!is.na(Cluster)) %>%
  rename(archetype_strength=Coeff) %>%
  select(Cluster, archetype_strength)


rosters <- read.csv(paste0(root, "/rawdata/rosters_2019-04-11.csv"), stringsAsFactors = FALSE) %>%
  select(PLAYER_FULL_NAME, Team)

injuries

cluster_details <- read.csv(paste0(root, "/modeldetails/cluster_details_2019-04-11.csv"), stringsAsFactors = FALSE) %>%
  arrange(PLAYER_FULL_NAME, DATE) %>%
  group_by(PLAYER_FULL_NAME, Cluster) %>%
  summarise(games_in_cluster=n()) %>%
  filter(games_in_cluster==max(games_in_cluster)) %>%
  inner_join(coeff_details, by="Cluster") %>%
  inner_join(rosters, by="PLAYER_FULL_NAME") %>%
  filter(archetype_strength>0.09) %>%
  arrange(archetype_strength, games_in_cluster) %>%
  filter(Team %in% c("Golden State Warriors", "Houston Rockets", "Oklahoma City Thunder", "Utah Jazz", "Detroit Pistons", "Indiana Pacers", "Milwaukee Bucks", "Denver Nuggets", "Portland Trailblazers", "Philadelphia 76ers", "Orlando Magic", "Brooklyn Nets", "Toronto Raptors", "San Antonio Spurs", "Los Angeles Clippers", "Boston Celtics")) %>%
  mutate(super_cluster=(archetype_strength>0.7)) %>%
  filter(is.na(injury_status)==FALSE & playoff_start_date>=injury_scrape_date & playoff_start_date<=return_date, 1, 0))


View(cluster_details)  
  
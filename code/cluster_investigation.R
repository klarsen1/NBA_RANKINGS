library(dplyr)
library(tidyr)
library(ggplot2)

root <- "/Users/kim.larsen/Documents/Code/NBA_RANKINGS"

coeff_details <- read.csv(paste0(root, "/modeldetails/coefficients_2019-04-11.csv")) %>%
  filter(DATE=="2019-04-10") %>%
  mutate(Cluster=as.numeric(gsub("share_minutes_cluster_", "", Variable))) %>%
  filter(!is.na(Cluster)) %>%
  rename(archetype_strength=Coeff) %>%
  select(Cluster, archetype_strength)


rosters <- read.csv(paste0(root, "/rawdata/rosters_2019-04-11.csv"), stringsAsFactors = FALSE) %>%
  select(PLAYER_FULL_NAME, Team) %>%
  filter(PLAYER_FULL_NAME != "DeMarcus Cousins")

injuries <- read.csv(paste0(root, "/rawdata/injuries_2019-04-11.csv"), stringsAsFactors = FALSE)

cluster_details <- read.csv(paste0(root, "/modeldetails/cluster_details_2019-04-11.csv"), stringsAsFactors = FALSE) %>%
  arrange(PLAYER_FULL_NAME, DATE) %>%
  group_by(PLAYER_FULL_NAME, Cluster) %>%
  summarise(games_in_cluster=n()) %>%
  filter(games_in_cluster==max(games_in_cluster)) %>%
  inner_join(coeff_details, by="Cluster") %>%
  inner_join(rosters, by="PLAYER_FULL_NAME") %>%
  filter(archetype_strength>0) %>%
  arrange(archetype_strength, games_in_cluster) %>%
  filter(Team %in% c("Golden State Warriors", "Houston Rockets", "Oklahoma City Thunder", "Utah Jazz", "Detroit Pistons", "Indiana Pacers", "Milwaukee Bucks", "Denver Nuggets", "Portland Trailblazers", "Philadelphia 76ers", "Orlando Magic", "Brooklyn Nets", "Toronto Raptors", "San Antonio Spurs", "Los Angeles Clippers", "Boston Celtics")) %>%
  mutate(max=as.numeric((archetype_strength>0.75)), 
         super=as.numeric((archetype_strength>0.10 & max==0)), 
         power=(super+max==0), n=1) %>%
  left_join(injuries, by="PLAYER_FULL_NAME") %>%
  filter(is.na(injury_status)==TRUE | injury_status=="Day-To-Day") %>%
  group_by(Team)

unique(filter(cluster_details, max==1)$Cluster)
unique(filter(cluster_details, super==1)$Cluster)
unique(filter(cluster_details, power==1)$Cluster)

t <- select(cluster_details, Team, max, super, power) %>%
  summarise_all(funs(sum)) %>%
  mutate(order=max+power+super) %>%
  arrange(order) %>%
  ungroup() %>%
  gather(cluster, players, max:power) %>%
  mutate(Cluster=ifelse(cluster=="max", " 1. max", ifelse(cluster=="super", " 2. super", " 3. power")))

ggplot(data=t, 
       aes(x=reorder(Team, order), players)) + 
       geom_bar(aes(fill=Cluster), stat="identity") + coord_flip() +
  xlab("") + ylab("") + theme(legend.title = element_blank()) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 1.0))


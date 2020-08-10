library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

root <- "/Users/kim.larsen/Documents/Code/NBA_RANKINGS"

stamp <- "2020-03-11"

coeff_details <- read.csv(paste0(root, "/modeldetails/coefficients_",stamp,".csv"), stringsAsFactors = FALSE) %>%
  filter(as.Date(DATE)==max(as.Date(DATE))) %>%
  mutate(Cluster=as.numeric(gsub("share_minutes_cluster_", "", Variable))) %>%
  filter(!is.na(Cluster)) %>%
  rename(archetype_strength=Coeff) %>%
  select(Cluster, archetype_strength)


rosters <- read.csv(paste0(root, "/rawdata/rosters_",stamp,".csv"), stringsAsFactors = FALSE) %>%
  select(PLAYER_FULL_NAME, Team)

injuries <- read.csv(paste0(root, "/rawdata/injuries_",stamp,".csv"), stringsAsFactors = FALSE)

cluster_details <- read.csv(paste0(root, "/modeldetails/cluster_details_",stamp,".csv"), stringsAsFactors = FALSE) %>%
  filter(year(DATE)>2019) %>%
  inner_join(rosters, by="PLAYER_FULL_NAME") %>%
  group_by(PLAYER_FULL_NAME, Cluster) %>%
  mutate(minutes_in_cluster=sum(minutes)) %>%
  group_by(Team, PLAYER_FULL_NAME) %>%
  mutate(minutes=mean(minutes)) %>%
  group_by(PLAYER_FULL_NAME) %>%
  filter(DATE==max(DATE)) %>%
  inner_join(coeff_details, by="Cluster") %>%
  ungroup() %>%
  mutate(weighted_strength=(archetype_strength-min(archetype_strength))*minutes) %>%
  left_join(injuries, by="PLAYER_FULL_NAME") %>%
  filter(is.na(injury_status)==TRUE | injury_status=="Day-To-Day") %>%
  select(Team, Cluster, PLAYER_FULL_NAME, minutes, weighted_strength, archetype_strength) %>%
  arrange(Team, weighted_strength) %>%
  group_by(Team) %>%
  mutate(rank=dense_rank(-minutes)) %>%
  filter(rank<=13) %>%
  mutate(cumlative_strength=cumsum(weighted_strength))

ggplot(data=filter(cluster_details, Team=="Houston Rockets"), 
       aes(x=reorder(PLAYER_FULL_NAME, weighted_strength), y=cumlative_strength)) + 
  geom_bar(stat="identity") + coord_flip() +
  xlab("") + ylab("") + theme(legend.title = element_blank()) + 
  scale_y_continuous(limits = c(0, 650), breaks=seq(from=0, to=600, by=50))+ 
  theme(axis.text.x=element_text(angle=90,hjust=1))

ggplot(data=filter(cluster_details, Team=="Los Angeles Lakers"), 
       aes(x=reorder(PLAYER_FULL_NAME, weighted_strength), y=cumlative_strength)) + 
  geom_bar(stat="identity") + coord_flip() +
  xlab("") + ylab("") + theme(legend.title = element_blank()) +
  scale_y_continuous(limits = c(0, 650), breaks=seq(from=0, to=600, by=50)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1))

ggplot(data=filter(cluster_details, Team=="Los Angeles Clippers"), 
       aes(x=reorder(PLAYER_FULL_NAME, weighted_strength), y=cumlative_strength)) + 
  geom_bar(stat="identity") + coord_flip() +
  xlab("") + ylab("") + theme(legend.title = element_blank()) +
  scale_y_continuous(limits = c(0, 650), breaks=seq(from=0, to=600, by=50)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1))

ggplot(data=filter(cluster_details, Team=="Milwaukee Bucks"), 
       aes(x=reorder(PLAYER_FULL_NAME, weighted_strength), y=cumlative_strength)) + 
  geom_bar(stat="identity") + coord_flip() +
  xlab("") + ylab("") + theme(legend.title = element_blank()) +
  scale_y_continuous(limits = c(0, 650), breaks=seq(from=0, to=600, by=50)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1))


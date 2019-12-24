library(dplyr)
library(tidyr)
library(ggplot2)

root <- "/Users/kim.larsen/Documents/Code/NBA_RANKINGS"

stamp <- "2019-12-22"

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
  arrange(PLAYER_FULL_NAME, DATE) %>%
  group_by(PLAYER_FULL_NAME, Cluster) %>%
  summarise(games_in_cluster=n()) %>%
  filter(games_in_cluster==max(games_in_cluster)) %>%
  inner_join(coeff_details, by="Cluster") %>%
  inner_join(rosters, by="PLAYER_FULL_NAME") %>%
  #filter(archetype_strength>0) %>%
  arrange(archetype_strength, games_in_cluster) %>%
  mutate(max=as.numeric((archetype_strength>1.15)), 
         super=as.numeric((archetype_strength>0.05 & max==0)), 
         power=(super+max==0), n=1) %>%
  left_join(injuries, by="PLAYER_FULL_NAME") %>%
  filter(is.na(injury_status)==TRUE | injury_status=="Day-To-Day" | as.Date(stamp)<return_date<as.Date("2020-03-01")) %>%
  group_by(Team)

mean(filter(cluster_details, max==1)$archetype_strength)
mean(filter(cluster_details, super==1)$archetype_strength)
mean(filter(cluster_details, power==1)$archetype_strength)

t <- select(cluster_details, Team, max, super, power, archetype_strength) %>%
  mutate(d=as.numeric(archetype_strength>0.05)) %>%
  summarise(max=sum(max), super=sum(super), power=sum(power), var=n_distinct(archetype_strength*d)) %>%
  mutate(order=7*max+super) %>%
  arrange(order) %>%
  ungroup() %>%
  gather(cluster, players, max:power) %>%
  mutate(Cluster=ifelse(cluster=="max", " Max", ifelse(cluster=="super", " Super", " 3. power"))) %>%
  mutate(players2=ifelse(cluster=="power", -players, ifelse(cluster=="max", 1*players, players))) %>%
  filter(Cluster != " 3. power")

ggplot(data=t, 
       aes(x=reorder(Team, order), players2)) + 
       geom_bar(aes(fill=Cluster), stat="identity") + coord_flip() +
  ylab("# of Players") + xlab("") + theme(legend.title = element_blank()) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 1.0)) 

tt <- filter(cluster_details, archetype_strength>0.8) %>% 
  mutate(cluster=ifelse(archetype_strength>1.15, "Max", "Super"))

nrow(tt)
nrow(cluster_details)

ggplot(data=tt,
       aes(x=reorder(PLAYER_FULL_NAME, archetype_strength), archetype_strength)) + 
  coord_flip() +
  geom_bar(aes(fill=factor(cluster)), stat="identity", size=2) +
  ylab("Strength") + xlab("") + theme(legend.title = element_blank()) + 
  theme(axis.text.x=element_blank())
  




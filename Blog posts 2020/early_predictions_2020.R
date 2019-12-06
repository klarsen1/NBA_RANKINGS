library(dplyr)
library(ggrepel)
library(tidyr)

stamp <- "2019-12-05"
  

root <- "/Users/kim.larsen/Documents/Code/NBA_RANKINGS"

f <- paste0(root, "/rankings/rankings_", stamp,".csv")

all_rankings <- read.csv(f, stringsAsFactors = FALSE) %>%
  group_by(conference) %>%
  mutate(elastic_ranking=min_rank(-season_win_rate),
         FiveThirtyEight=min_rank(-pred_win_rate_538),
         absdiff=ifelse(abs(elastic_ranking-FiveThirtyEight)>1, 0, 1)) %>%
  select(team, conference, division, elastic_ranking, FiveThirtyEight, absdiff, season_win_rate) %>%
  mutate(selected_team=team) %>%
  arrange(conference, elastic_ranking) %>%
  mutate(miss=ifelse(elastic_ranking<9, "Top 8", "9-15")) %>%
  ungroup(conference)

ggplot(data=filter(all_rankings, conference=="East"), aes(x=reorder(team, -elastic_ranking), season_win_rate, fill=factor(miss))) + 
  geom_bar(stat="identity") + coord_flip() +
  xlab("") + ylab("") + theme(legend.position = 'none') + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1))

ggplot(data=filter(all_rankings, conference=="West"), aes(x=reorder(team, -elastic_ranking), season_win_rate, fill=factor(miss))) + 
  geom_bar(stat="identity") + coord_flip() +
  xlab("") + ylab("") + theme(legend.position = 'none') + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1))

ggplot(filter(all_rankings, conference=="East"), aes(x=elastic_ranking, y=FiveThirtyEight)) +
  xlab("Elastic Ranking") + ylab("FiveThirtyEight") +
  geom_point(size = 2, color = 'black') +
  geom_smooth(method='lm') + 
  geom_label_repel(aes(elastic_ranking, FiveThirtyEight, label = team, fill=factor(absdiff)),
                   fontface = 'bold', color = 'white', size=2,
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.5, "lines")) + 
  theme(legend.title = element_blank()) + theme(legend.position="none") + 
  scale_y_reverse(limits=c(15,0)) + scale_x_reverse(limits=c(15,0))

ggplot(filter(all_rankings, conference=="West"), aes(x=elastic_ranking, y=FiveThirtyEight)) +
  xlab("Elastic Ranking") + ylab("FiveThirtyEight") +
  geom_point(size = 2, color = 'black') +
  geom_smooth(method='lm') + 
  geom_label_repel(aes(elastic_ranking, FiveThirtyEight, label = team, fill=factor(absdiff)),
                   fontface = 'bold', color = 'white', size=2,
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.5, "lines")) + 
  theme(legend.title = element_blank()) + theme(legend.position="none") + 
  scale_y_reverse(limits=c(15,0)) + scale_x_reverse(limits=c(15,0))

ggplot(all_rankings, aes(x=elastic_ranking, y=FiveThirtyEight)) +
  xlab("Elastic Ranking") + ylab("FiveThirtyEight") +
  geom_point(size = 2, color = 'black') +
  geom_smooth(method='lm') + 
  geom_label_repel(aes(elastic_ranking, FiveThirtyEight, label = team, fill=factor(absdiff)),
                   fontface = 'bold', color = 'white', size=2,
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.5, "lines")) + 
  theme(legend.title = element_blank()) + theme(legend.position="none") +
  scale_y_reverse(limits=c(15,0)) + scale_x_reverse(limits=c(15,0))

ff <- paste0(root, "/modeldetails/score_decomp_", stamp,".csv")

center <- function(x){return((x-median(x))/1)}

d <- read.csv(ff, stringsAsFactors = FALSE) %>%
  select(selected_team, roster, circumstances, performance) %>%
  inner_join(select(all_rankings, elastic_ranking, selected_team), by="selected_team") %>%
  group_by(selected_team) %>%
  summarise_each(funs(mean)) %>% ## get averages across games by team
  mutate(order=elastic_ranking) %>%
  arrange(order) %>%
  ungroup() %>%
  inner_join(select(all_rankings, conference, selected_team, division), by="selected_team") %>%
  group_by(conference) %>%
  mutate_each(funs(center), which(sapply(., is.numeric))) %>% ## standardize across teams
  ungroup() %>%
  mutate(roster_rank=round(100*(roster+abs(min(roster)))/(max(roster)+abs(min(roster))))) %>%
  rename(team=selected_team) 

dd <- gather(d, modelpart, value, roster:performance)
  

#ggplot(data=filter(dd, conference=="East"), aes(x=reorder(team, order), value)) + geom_bar(aes(fill=modelpart), stat="identity") + coord_flip() +
#  xlab("") + ylab("") + theme(legend.title = element_blank())

#ggplot(data=filter(dd, conference=="West"), aes(x=reorder(team, order), value)) + geom_bar(aes(fill=modelpart), stat="identity") + coord_flip() +
#  xlab("") + ylab("") + theme(legend.title = element_blank())


#ggplot(data=filter(d, conference=="East"), aes(x=reorder(team, roster_rank), roster_rank)) + geom_bar(stat="identity") + coord_flip() +
#  xlab("") + ylab("") + theme(legend.title = element_blank())


#ggplot(data=filter(d, conference=="West"), aes(x=reorder(team, roster_rank), roster_rank)) + geom_bar(stat="identity") + coord_flip() +
#  xlab("") + ylab("") + theme(legend.title = element_blank())


## playoffs

ffff <- paste0(root, "/rankings/playoff_prediction_",stamp,".csv")
playoffs <- read.csv(ffff, stringsAsFactors = FALSE) %>% 
  mutate(d=1, seed=winner_seed, 
         matchup=paste0(winner, "=", round(100*prob_win_series), "% ", loser,"=", round(100*(1-prob_win_series))," %"),
         round_text=case_when(round == 1 ~ "1. First Round",
                              round == 2 ~ "2. Conf Semifinals",
                              round == 3 ~ "3. Conf Finals",
                              round == 4 ~ "4. NBA Finals"))

ggplot(playoffs, aes(x=round_text, y=prob_win_series)) +
  xlab("") + ylab("Certainty") +
  geom_point(size = 2, color = 'black') +
  #geom_smooth(method='lm') + 
  geom_label_repel(aes(round, prob_win_series, label = matchup, fill=factor(round_text)),
                   fontface = 'bold', color = 'black', size=2,
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.5, "lines")) + 
  theme(legend.title = element_blank()) + theme(legend.position="none") +
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) 
  #theme(axis.text.y=element_blank())


library(dplyr)
library(ggrepel)
library(tidyr)

root <- "/Users/kim.larsen/Documents/Code/NBA_RANKINGS"

f <- paste0(root, "/rankings/rankings_2019-12-01.csv")

all_rankings <- read.csv(f, stringsAsFactors = FALSE) %>%
  group_by(conference) %>%
  mutate(elastic_ranking=min_rank(-season_win_rate),
         FiveThirtyEight=min_rank(pred_win_rate_538),
         absdiff=ifelse(abs(elastic_ranking-FiveThirtyEight)>2, 0, 1)) %>%
  select(team, conference, division, elastic_ranking, FiveThirtyEight, absdiff, season_win_rate) %>%
  mutate(selected_team=team) %>%
  arrange(conference, elastic_ranking) %>%
  mutate(miss=ifelse(elastic_ranking<8, "Top 8", "9-15")) %>%
  ungroup(conference)

ggplot(data=filter(all_rankings, conference=="East"), aes(x=reorder(team, -elastic_ranking), season_win_rate, fill=factor(miss))) + 
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
  theme(legend.title = element_blank()) + theme(legend.position="none")

ggplot(filter(all_rankings, conference=="West"), aes(x=elastic_ranking, y=FiveThirtyEight)) +
  xlab("Elastic Ranking") + ylab("FiveThirtyEight") +
  geom_point(size = 2, color = 'black') +
  geom_smooth(method='lm') + 
  geom_label_repel(aes(elastic_ranking, FiveThirtyEight, label = team, fill=factor(absdiff)),
                   fontface = 'bold', color = 'white', size=2,
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.5, "lines")) + 
  theme(legend.title = element_blank()) + theme(legend.position="none")

ff <- paste0(root, "/modeldetails/score_decomp_2019-12-01.csv")

center <- function(x){return(x-median(x))}

d <- read.csv(ff, stringsAsFactors = FALSE) %>%
  select(selected_team, roster, circumstances, performance) %>%
  inner_join(select(all_rankings, elastic_ranking, selected_team), by="selected_team") %>%
  group_by(selected_team) %>%
  summarise_each(funs(mean)) %>% ## get averages across games by team
  mutate(order=elastic_ranking) %>%
  arrange(order) %>%
  ungroup() %>%
  mutate_each(funs(center), which(sapply(., is.numeric))) %>% ## standardize across teams
  gather(modelpart, value, roster:performance) %>% ## transpose
  inner_join(select(all_rankings, conference, selected_team), by="selected_team") %>%
  rename(team=selected_team)

ggplot(data=filter(d, conference=="East"), aes(x=reorder(team, order), value)) + geom_bar(aes(fill=modelpart), stat="identity") + coord_flip() +
  xlab("") + ylab("") + theme(legend.title = element_blank())

ggplot(data=filter(d, conference=="West"), aes(x=reorder(team, order), value)) + geom_bar(aes(fill=modelpart), stat="identity") + coord_flip() +
  xlab("") + ylab("") + theme(legend.title = element_blank())



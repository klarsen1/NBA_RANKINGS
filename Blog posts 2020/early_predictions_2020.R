library(dplyr)
library(ggrepel)
library(tidyr)


f <- paste0(root, "/rankings/rankings_2019-12-01"

all_rankings <- read.csv(f) %>%
  mutate(elastic_ranking=min_rank(season_win_rate),
         FiveThirtyEight=min_rank(pred_win_rate_538),
         absdiff=ifelse(abs(elastic_ranking-FiveThirtyEight)>2, 0, 1)) %>%
  select(team, conference, division, elastic_ranking, FiveThirtyEight, absdiff) %>%
  arrange(conference, elastic_ranking)

ggplot(all_rankings, aes(x=elastic_ranking, y=FiveThirtyEight)) +
  xlab("Elastic Ranking") + ylab("FiveThirtyEight") +
  geom_point(size = 2, color = 'black') +
  geom_smooth(method='lm') + 
  geom_label_repel(aes(elastic_ranking, FiveThirtyEight, label = team, fill=factor(absdiff)),
                   fontface = 'bold', color = 'white', size=2,
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.5, "lines")) + 
  theme(legend.title = element_blank()) + theme(legend.position="none")


f <-
  "https://raw.githubusercontent.com/klarsen1/NBA_RANKINGS/master/modeldetails/score_decomp_2018-12-13.csv"
 
center <- function(x){return(x-median(x))}
d <- read.csv(f, stringsAsFactors = FALSE) %>%
  select(selected_team, roster, circumstances, performance) %>%
  group_by(selected_team) %>%
  summarise_each(funs(mean)) %>% ## get averages across games by team
  mutate(order=roster) %>%
  arrange(order) %>%
  ungroup() %>%
  mutate_each(funs(center), which(sapply(., is.numeric))) %>% ## standardize across teams
  gather(modelpart, value, roster:performance) %>% ## transpose
  rename(team=selected_team)

ggplot(data=d, aes(x=reorder(team, order), value)) + geom_bar(aes(fill=modelpart), stat="identity") + coord_flip() +
  xlab("") + ylab("") + theme(legend.title = element_blank())

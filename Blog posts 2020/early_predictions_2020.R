library(dplyr)
library(ggrepel)
library(tidyr)
library(formattable)
library("htmltools")
library("webshot")    


stamp <- "2020-02-05"
  

root <- "/Users/kim.larsen/Documents/Code/NBA_RANKINGS"

f <- paste0(root, "/rankings/rankings_", stamp,".csv")

all_rankings <- read.csv(f, stringsAsFactors = FALSE) %>%
  group_by(conference) %>%
  mutate(elastic_ranking=min_rank(-season_win_rate),
         FiveThirtyEight=min_rank(-pred_win_rate_538),
         absdiff=ifelse(abs(elastic_ranking-FiveThirtyEight)>0, 0, 1), 
         diff=case_when(elastic_ranking>FiveThirtyEight ~ "Elastic < 538", 
                   elastic_ranking<FiveThirtyEight ~ "Elastic > 538",
                   elastic_ranking==FiveThirtyEight ~ "Elastic == 538")) %>%
  select(team, conference, division, elastic_ranking, FiveThirtyEight, absdiff, season_win_rate, diff) %>%
  mutate(selected_team=team) %>%
  arrange(conference, elastic_ranking) %>%
  mutate(miss=ifelse(elastic_ranking<9, "Make Playoffs", "Miss Playoffs")) %>%
  ungroup(conference)

ggplot(data=filter(all_rankings, conference=="East"), aes(x=reorder(team, -elastic_ranking), season_win_rate, fill=factor(miss))) + 
  geom_bar(stat="identity") + coord_flip() +
  xlab("") + ylab("") + theme(legend.position = 'none') + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1))

ggplot(data=filter(all_rankings, conference=="West"), aes(x=reorder(team, -elastic_ranking), season_win_rate, fill=factor(miss))) + 
  geom_bar(stat="identity") + coord_flip() +
  xlab("") + ylab("") + theme(legend.position = 'none') + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1))

g1 <- ggplot(data=all_rankings, aes(x=reorder(team, season_win_rate), season_win_rate, fill=factor(conference))) + 
  geom_bar(stat="identity") + coord_flip() +
  xlab("") + ylab("") + 
  #theme(legend.position = 'none') + 
  theme(legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(accuracy=1))

g1

ggsave(g1, file=paste0(root, "/newsletter/season_win_rates.png"), device = "png", dpi=72, width=9, height=6)

ggplot(filter(all_rankings, conference=="East"), aes(x=elastic_ranking, y=FiveThirtyEight)) +
  xlab("Elastic Ranking") + ylab("FiveThirtyEight") +
  geom_point(size = 2, color = 'black') +
  geom_smooth(method='lm') + 
  geom_label_repel(aes(elastic_ranking, FiveThirtyEight, label = team, fill=factor(diff)),
                   fontface = 'bold', color = 'white', size=2,
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.5, "lines"))+
  theme(legend.title = element_blank()) + 
  #theme(legend.position="none") + 
  scale_y_reverse(limits=c(15,1), breaks=seq(1,15)) + scale_x_reverse(limits=c(15,1), breaks=seq(1,15)) + 
  guides(
    fill = guide_legend(
      override.aes = aes(label = "")
    )
  )

ggplot(filter(all_rankings, conference=="West"), aes(x=elastic_ranking, y=FiveThirtyEight)) +
  xlab("Elastic Ranking") + ylab("FiveThirtyEight") +
  geom_point(size = 2, color = 'black') +
  geom_smooth(method='lm') + 
  geom_label_repel(aes(elastic_ranking, FiveThirtyEight, label = team, fill=factor(diff)),
                   fontface = 'bold', color = 'white', size=2,
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.5, "lines"))+
  theme(legend.title = element_blank()) + 
  #theme(legend.position="none") + 
  scale_y_reverse(limits=c(15,1), breaks=seq(1,15)) + scale_x_reverse(limits=c(15,1), breaks=seq(1,15)) + 
  guides(
    fill = guide_legend(
      override.aes = aes(label = "")
    )
  )

g2 <- ggplot(all_rankings, aes(x=elastic_ranking, y=FiveThirtyEight)) +
  xlab("Elastic Ranking") + ylab("FiveThirtyEight") +
  geom_point(size = 2, color = 'black') +
  geom_smooth(method='lm') + 
  geom_label_repel(aes(elastic_ranking, FiveThirtyEight, label = team, fill=factor(diff)),
                   fontface = 'bold', color = 'white', size=2,
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.5, "lines"))+
  theme(legend.title = element_blank()) + 
  scale_y_reverse(limits=c(15,1), breaks=seq(1,15)) + scale_x_reverse(limits=c(15,1), breaks=seq(1,15)) + 
  guides(
    fill = guide_legend(
      override.aes = aes(label = "")
    )
  )

g2 

ggsave(g2, file=paste0(root, "/newsletter/comp_538.png"), device = "png", dpi=72, width=9, height=6)


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
  
## playoffs

ffff <- paste0(root, "/rankings/playoff_prediction_",stamp,".csv")
playoffs <- read.csv(ffff, stringsAsFactors = FALSE) %>% 
  mutate(d=1, seed=winner_seed, odds=round(prob_win_series/(1-prob_win_series), 2),
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

g3 <- ggplot(playoffs, aes(x=round_text, y=odds)) +
  xlab("") + ylab("Odds") +
  geom_point(size = 2, color = 'black') +
  #geom_smooth(method='lm') + 
  geom_label_repel(aes(round, odds, label = matchup, fill=factor(round_text)),
                   fontface = 'bold', color = 'black', size=2,
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.5, "lines")) + 
  theme(legend.title = element_blank()) + theme(legend.position="none")

g3

ggsave(g3, file=paste0(root, "/newsletter/playoffs.png"), device = "png", dpi=72, width=9, height=6)


five38<-read.csv(paste0(root,"/rawdata/FiveThirtyEight_", stamp, ".csv")) %>%
  filter(chance_winning_finals != "<1%") %>%
  mutate(chance_winning_finals=as.numeric(gsub("%", "", chance_winning_finals))/100, 
         chance_making_finals=as.numeric(gsub("%", "", chance_making_finals))/100) %>%
  select(selected_team, chance_winning_finals, chance_making_finals) %>%
  group_by(selected_team) %>%
  gather(metric, value, chance_winning_finals:chance_making_finals) %>%
  arrange(selected_team, metric) %>%
  mutate(order=min(value)) %>%
  ungroup() %>%
  mutate(metric=ifelse(metric=="chance_winning_finals", "Chance of winning finals (538)", "Chance of making finals (538)"),)

    

g4 <- ggplot(five38, aes(x=reorder(selected_team, order), y=value)) +
  geom_bar(stat="identity", position="dodge", aes(fill=metric)) + coord_flip() +
  xlab("Team") + ylab("") +
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
  theme(legend.title = element_blank())

g4
  
ggsave(g4, file=paste0(root, "/newsletter/title_probs_538.png"), device = "png", dpi=72, width=9, height=6)

games <- read.csv(paste0(root,"/rankings/game_level_predictions_",stamp,".csv"), stringsAsFactors = FALSE) %>%
  select(DATE, home_team_name,  road_team_name, home_team_prob_win) %>%
  mutate(DATE=as.Date(DATE))


raptor<-read.csv(paste0(root,"/rawdata/FiveThirtyEight_", stamp, ".csv"), stringsAsFactors = FALSE) %>%
  select(carm_elo, selected_team) %>%
  mutate(road_team_name=selected_team, home_team_name=selected_team,
         raptor_home=carm_elo, raptor_road=carm_elo)

raptor1 <- select(raptor, home_team_name, raptor_home) 
raptor2 <- select(raptor, road_team_name, raptor_road)

fff <- read_html("https://projects.fivethirtyeight.com/2020-nba-predictions/games/")
dates <- test %>% html_nodes(".h3") %>% html_text() %>% as.Date("%A, %b. %d")
teams <- test %>% html_nodes(".text") %>% html_text() %>% trim()
probs <- test %>% html_nodes(".number.chance") %>% html_text() %>% gsub('%','', .) %>% as.numeric()/100
games_per_day <- test %>% html_nodes(".day") %>% html_text() 
l <- list()
k <- 1
for (i in 1:length(games_per_day)){
  c <- str_count(games_per_day[[i]],'%')/2
  for (j in 1:c){
    l[[k]] <- data.frame(ID=k, DATE=as.Date(dates[i]), home_team_prob_win_538=probs[k+1], home_team_name_538=teams[k+1], road_team_name=teams[k], stringsAsFactors = FALSE)
    k <- k+2
  }
}
final_538_probs <- read.csv(paste0(root, "/rawdata/five38_probs_", stamp, ".csv"), stringsAsFactors = FALSE) %>%
  mutate(DATE=as.Date(DATE))

games_interesting <- 
  inner_join(games, raptor1, by="home_team_name") %>%
  inner_join(raptor2, by="road_team_name") %>%
  inner_join(final_538_probs, by=c("home_team_name", "DATE")) %>%
  filter(as.Date(DATE) < as.Date(stamp)+30 & as.Date(DATE)>as.Date(stamp)) %>%
  select(DATE, home_team_name, road_team_name, home_team_prob_win, home_team_prob_win_538) %>%
  mutate(Difference=round(abs(home_team_prob_win-home_team_prob_win_538),2), 
         home_win=ifelse(home_team_prob_win>.5, 1, 0), 
         home_win_538=ifelse(home_team_prob_win_538>.5, 1, 0)) %>%
  filter(Difference>.2 | (home_win != home_win_538 & Difference>0.05)) %>%
  arrange(-Difference) %>% select(-home_win, -home_win_538)
  


names(games_interesting) <- c("Date", "Home Team", "Road Team", "Home Team Win (Elastic)", "Home Team Win (538)", "Difference")

nice_table <- formattable(games_interesting, align = rep("c", NCOL(games_interesting)),
                       list(`Date` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")), 
                            area(col = 4:5) ~ function(x) percent(x, digits = 0),
                            area(col = 6) ~ color_tile("#DeF7E9","#71CA97")))
                             

export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

export_formattable(nice_table,paste0(root, "/newsletter/games.png"))

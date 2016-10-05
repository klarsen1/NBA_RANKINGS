source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/get_surplus_variables.R")

predict_game <- function(b, history, win_perc, id, date, runs=100){

  thisgame <- subset(box_scores, game_id==id)   
  team1 <- thisgame$home_team_name
  team2 <- thisgame$road_team_name
  selected <- thisgame$selected_team
  
  ### Read the overrides
  overrides <- data.frame(read_excel("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/rawdata/overrides.xlsx", sheet=1))

  ### First get the average minutes and standard deviations for each player
  dist <- filter(history, (OWN_TEAM==team1 | OWN_TEAM==team2)) %>%
    group_by(PLAYER_FULL_NAME) %>%
    summarise(m_share_of_minutes=mean(share_of_minutes), 
              s_share_of_minutes=sd(share_of_minutes)) %>%
    left_join(overrides, by="PLAYER_FULL_NAME") %>%
    mutate(OWN_TEAM=ifelse(is.na(NEW_TEAM)==TRUE, OWN_TEAM, NEW_TEAM)) %>%
    select(-NEW_TEAM)

  ## Infer active rosters
  dist_active <- filter(box_scores, (OWN_TEAM==team1 | OWN_TEAM==team2) & date-14<DATE) %>%
    group_by(PLAYER_FULL_NAME) %>%
    filter(DATE==max(DATE)) %>%
    inner_join(dist, by="PLAYER_FULL_NAME") %>%
    select(PLAYER_FULL_NAME, OWN_TEAM, m_share_of_minutes, s_share_of_minutes, home_team_name, road_team_name, selected_team) %>%
    group_by(OWN_TEAM) %>%
    mutate(player=row_number()) %>%
    arrange(OWN_TEAM, m_share_of_minutes) %>%
    filter(player<14) %>%
    ungroup()
  
  set.seed(2015)
  samples <- list()
  for (j in 1:runs){
    df <- mutate(dist_active, 
                 game_id=id,
                 share_of_minutes=rnorm(1, m_share_of_minutes, s_share_of_minutes), 
                 share_of_minutes_signed = ifelse(OWN_TEAM==selected_team, share_of_minutes, -share_of_minutes)) %>%
    
    x <- get_surplus_variables(df) %>%
      left_join(win_perc, by="selected_team") %>%
      left_join(select(thisgame, DATE, game_id, travel, rest_differential, home_team_name, road_team_name), by="game_id")
    
    samples[[i]] <- x
  }
  
  samplesdf <- data.frame(rbindlist(samples))
  f <- as.formula(~.)
  X <- model.matrix(f, samplesdf)
  prob_win <- 1/(1+exp(-X%*%b[-1]))
  
  samplesdf$prob_win <- prob_win
  samplesdf$d_selected_team_win <- ifelse(samplesdf$prob_win>.5, 1, 0)
  
  prediction <- group_by(samplesdf, DATE, game_id, home_team_name, road_team_name, selected_team) %>%
    summarise(selected_team_win=mean(d_selected_team_win)) %>%
    ungroup()
  
  return(data.frame(prediction))
  
}


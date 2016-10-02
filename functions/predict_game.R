source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/get_surplus_variables.R")

predict_game <- function(X, b, gamedata, team1, team2, date, runs=1000){
  
  ### First get the average minutes and standard deviations for each player
  dist <- filter(gamedata, OWN_TEAM==team1 | OWN_TEAM==team2) %>%
    group_by(PLAYER_FULL_NAME) %>%
    summarise(m_share_of_minutes=mean(share_of_minutes), 
              s_share_of_minutes=sd(share_of_minutes))

  ### Pick a team
  if (runif(1)>0.5) selected_team=team1
  else selected_team=team2
  
  ## Infer active rosters
  distactive <- filter(gamedata, (OWN_TEAM==team1 | OWN_TEAM==team2) & date-14<DATE & DATE<date) %>%
    group_by(PLAYER_FULL_NAME) %>%
    filter(DATE==max(DATE)) %>%
    inner_join(dist, by="PLAYER_FULL_NAME") %>%
    select(PLAYER_FULL_NAME, OWN_TEAM, m_share_of_minutes, s_share_of_minutes) %>%
    group_by(OWN_TEAM) %>%
    arrange(OWN_TEAM, m_share_of_minutes) %>%
    mutate(player=row_number()) %>%
    filter(player<14) %>%
    ungroup()
  
  set.seed(2015)
  samples <- list()
  for (j in 1:runs){
    df <- mutate(distactive, 
                 share_of_minutes=rnorm(1, m_share_of_minutes, s_share_of_minutes), 
                 share_of_minutes_signed = ifelse(OWN_TEAM==selected_team, share_of_minutes, -share_of_minutes)) %>%
    
      get_surplus_variables()  
    
    samples[[i]] <- df
  }
  
  samplesdf <- rbindlist(samples)
  
  return(data.frame(distactive))
  
}


t=predict_game(1,2,f,"Golden State", "Cleveland", as.Date("2016-05-01"))
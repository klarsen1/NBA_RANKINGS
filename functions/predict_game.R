source("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/functions/get_surplus_variables.R")

predict_game <- function(X, b, box_scores, win_perc, id, date, runs=1000){
  
  team1 <- subset(box_scores, game_id==id)$OWN_TEAM
  team2 <- subset(box_scores, game_id==id)$OPP_TEAM

  ### First get the average minutes and standard deviations for each player
  dist <- filter(box_scores, (OWN_TEAM==team1 | OWN_TEAM==team2) & future_game==0) %>%
    group_by(PLAYER_FULL_NAME) %>%
    summarise(m_share_of_minutes=mean(share_of_minutes), 
              s_share_of_minutes=sd(share_of_minutes))

  ## Infer active rosters
  dist_active <- filter(box_scores, (OWN_TEAM==team1 | OWN_TEAM==team2) & date-14<DATE & DATE<date & future_game==0) %>%
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
    df <- mutate(dist_active, 
                 share_of_minutes=rnorm(1, m_share_of_minutes, s_share_of_minutes), 
                 share_of_minutes_signed = ifelse(OWN_TEAM==selected_team, share_of_minutes, -share_of_minutes)) %>%
    
    df <- get_surplus_variables(df)
    
    collapsed <- dplyr::select(df, selected_team, selected_team_win, starts_with("share_minutes_cluster_"), home_team_selected, game_id) %>%
      summarise_each(funs(sum)) %>%
      mutate(home_team_selected=as.numeric(home_team_selected>0), 
             game_id=id) %>%
      left_join(win_perc, by="selected_team") %>%
      left_join(filter(select(box_scores, game_id, travel, rest_differential), game_id==id), by="game_id")
    
    samples[[i]] <- df
  }
  
  samplesdf <- rbindlist(samples)
  
  return(data.frame(distactive))
  
}


t=predict_game(1,2,f,"Golden State", "Cleveland", as.Date("2016-05-01"))
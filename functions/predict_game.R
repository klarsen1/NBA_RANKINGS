predict_game <- function(b, history, win_perc1, win_perc2, id, runs, tobescored, nclus, prior, posterior, dir, model_variables, use_current_rosters=0){

  thisgame <- tobescored[1,]
  date <- thisgame$DATE
  thisseason <- thisgame$season
  team1 <- thisgame$home_team_name
  team2 <- thisgame$road_team_name
  selected <- thisgame$selected_team
  w <- thisgame$selected_team_win
  w1 <- thisgame$selected_team_matchup_wins
  w2 <- thisgame$opposing_team_matchup_wins
  
  ### Read the overrides
  #overrides <- data.frame(read.csv(paste0(dir, "overrides.csv"), stringsAsFactors = FALSE, header = TRUE))
  rosters <- data.frame(read.csv(paste0(dir, "current_rosters.csv"), stringsAsFactors = FALSE))

  ### First get the average minutes and standard deviations for each player
  dist <- filter(history, (OWN_TEAM==team1 | OWN_TEAM==team2)) %>%
    group_by(PLAYER_FULL_NAME) %>%
    mutate(gap=DATE_INDEX-min(DATE_INDEX)+1,
           weight=ifelse(season==thisseason, 1.0, 0.25)) %>%
    summarise(m_share_of_minutes=weighted.mean(share_of_minutes, weight),
              s_share_of_minutes=sd(share_of_minutes)) %>%
    replace(is.na(.), 0)
  
  ## Apply the overrides
  if (use_current_rosters==1){
     history_override <- inner_join(select(history, -OWN_TEAM), rosters, by="PLAYER_FULL_NAME")
  }
  #history_override <- left_join(history, overrides, by="PLAYER_FULL_NAME") %>%
  #  mutate(OWN_TEAM=ifelse(is.na(NEW_TEAM)==FALSE & DATE>=as.Date(OVERRIDE_DATE, format="%m/%d/%Y"), NEW_TEAM, OWN_TEAM)) %>%
  #  select(-NEW_TEAM, -OVERRIDE_DATE)
    
  ## Infer active rosters
  if (nrow(filter(history_override, OWN_TEAM==team1 & season==thisseason))==0 | nrow(filter(history_override, OWN_TEAM==team2 & season==thisseason))==0){
    d_current_roster <- 0
    thisseason2 <- thisseason-1
  } else{
    d_current_roster <- 1
    thisseason2 <- thisseason
  }
  
  dist_active <- filter(history_override, OWN_TEAM %in% c(team1,team2) & season==thisseason2) %>%
    group_by(PLAYER_FULL_NAME) %>%
    filter(DATE==max(DATE)) %>%
    inner_join(dist, by="PLAYER_FULL_NAME") %>%
    select(PLAYER_FULL_NAME, OWN_TEAM, m_share_of_minutes, s_share_of_minutes, Cluster, DATE_INDEX) %>%
    group_by(OWN_TEAM) %>%
    mutate(rank_minutes=percent_rank(m_share_of_minutes), 
           rank_time=percent_rank(DATE_INDEX), 
           likelihood_to_play=rank_minutes + rank_time) %>%
    arrange(OWN_TEAM, -likelihood_to_play) %>%
    mutate(player=row_number(),
           game_id=id, 
           selected_team=selected, 
           opposing_team=ifelse(selected_team==team1, team2, team1),
           home_team_selected=as.numeric(selected_team==team1), 
           selected_team_win=w,
           selected_team_matchup_wins=w1, 
           opposing_team_matchup_wins=w2) %>%
    filter(player<14) %>%
    select(-player, -DATE_INDEX) %>%
    ungroup()

  sim_share_of_minutes <- function(x){
    x$share_of_minutes=min(max(rnorm(1, x$m_share_of_minutes, x$s_share_of_minutes), 0), 1)
    return(x)
  }
  if (runs==0){
    dist_active$share_of_minutes <- dist_active$m_share_of_minutes
    x <- get_surplus_variables(dist_active, nclus) %>% select(-game_id)
    d <- attach_win_perc(thisgame, win_perc1, win_perc2)
    samplesdf <- data.frame(cbind(x, d, row.names=NULL), stringsAsFactors = FALSE)
  } else if (runs==1){
    dist_active_sim <- data.frame(rbindlist(lapply(split(dist_active, dist_active$PLAYER_FULL_NAME), sim_share_of_minutes)))  
    x <- get_surplus_variables(dist_active_sim, nclus)
    d <- attach_win_perc(thisgame, win_perc1, win_perc2)
    samplesdf <- inner_join(x, d, by="game_id")
  } else{
    ncore <- detectCores()-1
    registerDoParallel(ncore)
    loop_result <- foreach(j=1:runs) %dopar% {
      dist_active_sim <- data.frame(rbindlist(lapply(split(dist_active, dist_active$PLAYER_FULL_NAME), sim_share_of_minutes)))  
      x <- get_surplus_variables(dist_active_sim, nclus)
      d <- attach_win_perc(thisgame, win_perc1, win_perc2)
      return(data.frame(inner_join(x, d, by="game_id")))
    }
    samplesdf <- data.frame(rbindlist(loop_result))
  } 
  
  ### Offset to apply prior for the intercept
  offset <- log((1-prior)*posterior / (prior*(1-posterior)))

  ### Score the model
  x <- samplesdf[,names(samplesdf) %in% unique(model_variables$Variable)]
  f <- as.formula(~.)
  X <- model.matrix(f, x)
  prob_win <- 1/(1+exp(-X%*%b[-1] + offset))
  d <- data.frame(cbind(X*c[-1], distinct(select(samplesdf, game_id, DATE, home_team_name, road_team_name, selected_team, opposing_team), game_id, .keep_all=TRUE)), stringsAsFactors = FALSE) %>%
    select(-X.Intercept.) %>%
    mutate(roster=rowSums(.[1:26]), 
           circumstances=rowSums(.[27:30]),
           performance=rowSums(.[31:36]))
  
  samplesdf$prob_win <- prob_win
  samplesdf$d_prob_selected_team_win <- ifelse(samplesdf$prob_win>.5, 1.0, 0.0)
  
  prediction <- group_by(samplesdf, game_id, DATE, home_team_name, road_team_name, selected_team, opposing_team) %>%
    summarise(prob_selected_team_win_d=mean(as.numeric(prob_win)),
              prob_selected_team_win_b=mean(as.numeric(d_prob_selected_team_win))) %>%
    mutate(current_roster_used=d_current_roster) %>%
    ungroup()
  
  prediction$selected_team_win <- w
  
  return(list(data.frame(prediction), d))
}
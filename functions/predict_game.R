predict_game <- function(b, history, win_perc1, win_perc2, id, runs, tobescored, nclus, prior, posterior, dir, model_variables, use_current_rosters, offsets_by_team, seed=Sys.time()){

  set.seed(seed)
  
  thisgame <- tobescored[1,]
  date <- thisgame$DATE
  thisseason <- thisgame$season
  team1 <- thisgame$home_team_name
  team2 <- thisgame$road_team_name
  selected <- thisgame$selected_team
  w <- thisgame$selected_team_win
  w1 <- thisgame$selected_team_matchup_wins
  w2 <- thisgame$opposing_team_matchup_wins
  floating_base <- thisgame$fb
  future <- thisgame$future_game
  days_on_road1 <- thisgame$days_on_road_selected_team
  days_on_road2 <- thisgame$days_on_road_opposing_team
  
  ### Read the overrides
  rosters <- data.frame(read.csv(paste0(dir, "rosters_current.csv"), stringsAsFactors = FALSE))

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
     history_override <- inner_join(dplyr::select(history, -OWN_TEAM), rosters, by="PLAYER_FULL_NAME")
     print("-- Using current scraped rosters")
  } else{
     history_override <- history
     print("-- Inferring rosters")
  }
  
  #print(thisseason)
  #print(sort(unique(filter(history_override, season==thisseason)$OWN_TEAM)))
  #print(sort(unique(history_override$season)))
  
  ## Infer active rosters
  if (nrow(filter(history_override, OWN_TEAM==team1 & season==thisseason))==0 | nrow(filter(history_override, OWN_TEAM==team2 & season==thisseason))==0){
    d_current_season_data_available <- 0
    thisseason2 <- thisseason-1
  } else{
    d_current_season_data_available <- 1
    thisseason2 <- thisseason
  }

  if (nrow(filter(history_override, OWN_TEAM==team1 & season==thisseason2))==0){
    print(paste0("ERROR: no data in season ", thisseason2, " for team 1 =  ", team1))
    print(paste0("Seasons for ", team1, ": ", unique(filter(history_override, OWN_TEAM==team1)$season)))
    print(paste0("Teams for season ", thisseason2, ": ", unique(filter(history_override, season==thisseason2)$OWN_TEAM)))
  } 
  if (nrow(filter(history_override, OWN_TEAM==team2 & season==thisseason2))==0){
    print(paste0("predict_game ERROR: no data in season ", thisseason2, " for team 2 =  ", team2))
    print(paste0("Seasons for ", team2, ": ", unique(filter(history_override, OWN_TEAM==team2)$season)))
    print(paste0("Teams for season ", thisseason2, ": ", unique(filter(history_override, season==thisseason2)$OWN_TEAM)))
  } 
  
  dist_active <- filter(history_override, OWN_TEAM %in% c(team1,team2) & season==thisseason2) %>%
    group_by(PLAYER_FULL_NAME) %>%
    filter(DATE==max(DATE)) %>%
    inner_join(dist, by="PLAYER_FULL_NAME") %>%
    dplyr::select(PLAYER_FULL_NAME, OWN_TEAM, m_share_of_minutes, s_share_of_minutes, Cluster, DATE_INDEX) %>%
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
           opposing_team_matchup_wins=w2,
           days_on_road_selected_team=days_on_road1,
           days_on_road_opposing_team=days_on_road2,
           fb=floating_base) %>%
    filter(player<14) %>%
    dplyr::select(-player, -DATE_INDEX) %>%
    ungroup()

  sim_share_of_minutes <- function(x){
    x$share_of_minutes=rnorm(1, x$m_share_of_minutes, x$s_share_of_minutes)
    x$share_of_minutes <- ifelse(x$share_of_minutes<0,0, x$share_of_minutes)
    x$share_of_minutes <- ifelse(x$share_of_minutes>1,1, x$share_of_minutes)
    return(x)
  }
  if (runs==0){
    dist_active$share_of_minutes <- dist_active$m_share_of_minutes
    x <- get_surplus_variables(dist_active, nclus) %>% dplyr::select(-game_id)
    d <- attach_win_perc(thisgame, win_perc1, win_perc2)
    #print(nrow(dist_active))
    #print(nrow(d))
    #print(thisseason2)
    #print(team1)
    #print(team2)
    samplesdf <- data.frame(cbind(x, d, row.names=NULL), stringsAsFactors = FALSE)
  } else if (runs==1){
    dist_active_sim <- data.frame(rbindlist(lapply(split(dist_active, dist_active$PLAYER_FULL_NAME), sim_share_of_minutes)))
    #print(dplyr::select(dist_active_sim, PLAYER_FULL_NAME, m_share_of_minutes, share_of_minutes))
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
  #prob_win <- 1/(1+exp(-X%*%b[-1] + offset))
  #print(b[-1])
  #print(X)
  #print(length(b[-1]))
  #print(dim(X))
  XtB <- X%*%b[-1]
  samplesdf$xb <- as.numeric(XtB)
  d <- data.frame(cbind(X*c[-1], distinct(dplyr::select(samplesdf, game_id, DATE, home_team_name, road_team_name, selected_team, opposing_team), game_id, .keep_all=TRUE)), stringsAsFactors = FALSE) %>%
    dplyr::select(-X.Intercept.) 

  d$roster <- rowSums(dplyr::select(d, starts_with("share_minutes_cluster")))
  #d$circumstances <- rowSums(dplyr::select(d, opposing_team_travel, opposing_team_rest, selected_team_rest, selected_team_travel, home_team_selected))
  #d$performance <- rowSums(dplyr::select(d, selected_team_matchup_wins, opposing_team_matchup_wins, winrate_season_selected_team, winrate_season_selected_team_adj, winrate_season_opposing_team, winrate_season_opposing_team_adj))
  d$performance <- rowSums(dplyr::select(d, winrate_season_selected_team, winrate_season_opposing_team))
  if (is.null(offsets_by_team)==FALSE){
    if (nrow(offsets_by_team)>0){
      samplesdf <- left_join(samplesdf, offsets_by_team, by="selected_team") %>%
        replace(is.na(.), 0)
      samplesdf$prob_win=1/(1+exp(-samplesdf$xb + samplesdf$teamoffset))
    } else{
      samplesdf$offset <- offset
      samplesdf$prob_win=1/(1+exp(-samplesdf$xb + samplesdf$offset))
    }
  } else{
    samplesdf$offset <- offset
    samplesdf$prob_win=1/(1+exp(-samplesdf$xb + samplesdf$offset))
  }
  
  #samplesdf$prob_win <- prob_win
  samplesdf$d_prob_selected_team_win <- ifelse(samplesdf$prob_win>.5, 1.0, 0.0)
  
  prediction <- group_by(samplesdf, game_id, DATE, home_team_name, road_team_name, selected_team, opposing_team, playoffs, season) %>%
    summarise(prob_selected_team_win_d=mean(as.numeric(prob_win)),
              prob_selected_team_win_b=mean(as.numeric(d_prob_selected_team_win))) %>%
    mutate(current_season_data_used=d_current_season_data_available,
           future_game=future) %>%
    ungroup()
  
  prediction$selected_team_win <- w
  
  return(list(data.frame(prediction), d))
}
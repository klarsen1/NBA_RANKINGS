
predict_game <- function(b, history, win_perc, id, date, runs=100, tobescored, nclus, prior, posterior, dir){

  thisgame <- tobescored[1,]
  thisseason <- thisgame$season
  team1 <- thisgame$home_team_name
  team2 <- thisgame$road_team_name
  selected <- thisgame$selected_team
  w <- thisgame$selected_team_win
  
  print(paste0("Home team = ", team1, ", Road team = ", team2))
  print(paste0("Date = ", date))
  
  ### Read the overrides
  overrides <- data.frame(read_excel(paste0(dir, "overrides.xlsx"), sheet=1))

  ### First get the average minutes and standard deviations for each player
  dist <- filter(history, (OWN_TEAM==team1 | OWN_TEAM==team2)) %>%
    group_by(PLAYER_FULL_NAME) %>%
    summarise(n=sum(share_of_minutes*DATE_INDEX),
              d=sum(DATE_INDEX),
              m_share_of_minutes=mean(share_of_minutes),
              s_share_of_minutes=sd(share_of_minutes)) %>%
    select(-n, -d) %>%
    replace(is.na(.), 0)
  
  ## Apply the overrides
  history_override <- left_join(history, overrides, by="PLAYER_FULL_NAME") %>%
    mutate(OWN_TEAM=ifelse(is.na(NEW_TEAM)==FALSE & DATE>=as.Date(OVERRIDE_DATE, format="%m/%d/%Y"), NEW_TEAM, OWN_TEAM)) %>%
    select(-NEW_TEAM, -OVERRIDE_DATE)
    
  ## Infer active rosters
  dist_active <- filter(history_override, OWN_TEAM %in% c(team1,team2) & season %in% c(thisseason, thisseason-1)) %>%
    group_by(PLAYER_FULL_NAME) %>%
    filter(DATE==max(DATE)) %>%
    inner_join(dist, by="PLAYER_FULL_NAME") %>%
    select(PLAYER_FULL_NAME, OWN_TEAM, m_share_of_minutes, s_share_of_minutes, Cluster, DATE_INDEX) %>%
    group_by(OWN_TEAM) %>%
    arrange(OWN_TEAM, -DATE_INDEX, -m_share_of_minutes) %>%
    mutate(player=row_number(),
           game_id=id, 
           selected_team=selected, 
           home_team_selected=as.numeric(selected_team==team1), 
           selected_team_win=w) %>%
    filter(player<14) %>%
    select(-player, -DATE_INDEX) %>%
    ungroup()

  set.seed(2015)
  samples <- list()
  
  sim_share_of_minutes <- function(x){
    x$share_of_minutes=rnorm(1, x$m_share_of_minutes, x$s_share_of_minutes)
    return(x)
  }
  if (runs>1){
    ncore <- detectCores()-1
    registerDoParallel(ncore)
    loop_result <- foreach(j=1:runs) %dopar% {
      df <- data.frame(rbindlist(lapply(split(dist_active, dist_active$PLAYER_FULL_NAME), sim_share_of_minutes))) %>%
        group_by(OWN_TEAM) %>%
        mutate(share_of_minutes=share_of_minutes/sum(share_of_minutes),
               share_of_minutes_signed = ifelse(OWN_TEAM==selected_team, share_of_minutes, -share_of_minutes)) %>%
        ungroup()
        
      x <- get_surplus_variables(df, nclus) %>%
        left_join(win_perc, by="selected_team")

      return(data.frame(cbind(x, select(thisgame, DATE, travel, rest_differential, home_team_name, road_team_name, home_team_points, road_team_points, opposing_team), row.names=NULL), stringsAsFactors = FALSE))
    }
    samplesdf <- data.frame(rbindlist(loop_result))
  } else{
    df <- group_by(dist_active, OWN_TEAM) %>%
      mutate(share_of_minutes=m_share_of_minutes/sum(m_share_of_minutes),
             share_of_minutes_signed = ifelse(OWN_TEAM==selected_team, m_share_of_minutes, -m_share_of_minutes)) %>%
      ungroup()
    
    x <- get_surplus_variables(df, nclus) %>%
       left_join(win_perc, by="selected_team")
    samplesdf <- data.frame(cbind(x, select(thisgame, DATE, travel, rest_differential, home_team_name, road_team_name, home_team_points, road_team_points, opposing_team), row.names=NULL), stringsAsFactors = FALSE)
  }

  offset <- log((1-prior)*posterior / (prior*(1-posterior)))

  x <- select(samplesdf, -game_id, -DATE, -home_team_name, -road_team_name, -selected_team, -selected_team_win, -home_team_points, -road_team_points, -opposing_team)
  f <- as.formula(~.)
  X <- model.matrix(f, x)
  prob_win <- 1/(1+exp(-X%*%b[-1] + offset))
  
  samplesdf$prob_win <- prob_win
  samplesdf$d_prob_selected_team_win <- ifelse(samplesdf$prob_win>.5, 1.0, 0.0)
  
  prediction <- group_by(samplesdf, game_id, DATE, home_team_name, road_team_name, selected_team, opposing_team, selected_team_win) %>%
    summarise(prob_selected_team_win_d=mean(as.numeric(prob_win)),
              prob_selected_team_win_b=mean(as.numeric(d_prob_selected_team_win))) %>%
    ungroup()
  
  return(data.frame(prediction))
  
}
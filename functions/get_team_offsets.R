get_team_offsets <- function(ytd_scores){
  offsets <- group_by(ytd_scores, selected_team) %>%
    summarise(prior=mean(prob_selected_team_win_d), 
              posterior=mean(selected_team_win),
              teamoffset=log((1-prior)*posterior / (prior*(1-posterior)))) %>%
    select(selected_team, teamoffset) %>%
    ungroup()
  return(offsets)
}
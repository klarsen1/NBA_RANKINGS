sim_playoff <- function(ranks){
  
  qualifiers <- group_by(ranks, Conference) %>% arrange(ranks, -pred_win_rate) %>%
    filter
  
}
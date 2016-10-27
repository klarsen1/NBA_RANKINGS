sim_playoff <- function(ranks){
  
  qualifiers <- group_by(ranks, Conference) %>% arrange(-pred_win_rate) %>%
    filter(row_number()<9)
  
}
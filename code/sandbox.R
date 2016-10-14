mean(output$prob_selected_team_win_b)
mean(output$prob_selected_team_win_d)
mean(output$selected_team_win)

print(paste0("C: ", AUC(output$selected_team_win, output$prob_selected_team_win_d)[1]))
print(paste0("C: ", AUC(output$selected_team_win, output$prob_selected_team_win_b)[1]))
cor(output$prob_selected_team_win_b, output$prob_selected_team_win_d)

t <- readRDS("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/rankings/first_successful_sim.RDA")
t <- mutate(t, match=ifelse(selected_team_win==as.numeric(prob_selected_team_win_b>0.5), 1, 0))
mean(t$match)

t <- readRDS("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/rankings/first_successful_sim.RDA")
t <- mutate(t, match=ifelse(selected_team_win==as.numeric(prob_selected_team_win_d>0.5), 1, 0))
mean(t$match)
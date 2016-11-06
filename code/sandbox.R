t <- subset(game_level, current_roster_used==1)


mean(t$prob_selected_team_win_b)
mean(t$prob_selected_team_win_d)
mean(t$selected_team_win)

print(paste0("C: ", AUC(t$selected_team_win, t$prob_selected_team_win_d)[1]))
print(paste0("C: ", AUC(t$selected_team_win, t$prob_selected_team_win_b)[1]))
cor(game_level$prob_selected_team_win_b, game_level$prob_selected_team_win_d)


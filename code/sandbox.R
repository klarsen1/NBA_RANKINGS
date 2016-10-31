mean(game_level$prob_selected_team_win_b)
mean(game_level$prob_selected_team_win_d)
mean(game_level$selected_team_win)

print(paste0("C: ", AUC(game_level$selected_team_win, game_level$prob_selected_team_win_d)[1]))
print(paste0("C: ", AUC(game_level$selected_team_win, game_level$prob_selected_team_win_b)[1]))
cor(game_level$prob_selected_team_win_b, game_level$prob_selected_team_win_d)


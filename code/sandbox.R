t <- subset(results[[1]], current_roster_used==1)


mean(t$prob_selected_team_win_b)
mean(t$prob_selected_team_win_d)
mean(t$selected_team_win)

print(paste0("C: ", AUC(t$selected_team_win, t$prob_selected_team_win_d)[1]))

print(paste0("C: ", AUC(t$selected_team_win, t$prob_selected_team_win_b)[1]))

#playoffs, 0.10, 0.730336958637354
#playoffs, 0.25, 0.731457305755071
#0.707811159001788
#
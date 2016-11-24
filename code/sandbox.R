t <- subset(results[[1]], current_season_data_used==1 & is.na(selected_team_win)==FALSE) %>%
  mutate(match=as.numeric(selected_team_win==d_pred_selected_team_win), 
         first_half=as.numeric(DATE<'2016-02-01')) %>%
  group_by(first_half)

summarise(t, match=mean(match))

mean(t$prob_selected_team_win_d)
mean(t$selected_team_win)
mean(t$match)
mean(t$d_pred_selected_team_win)

print(paste0("C: ", AUC(t$selected_team_win, t$prob_selected_team_win_d)[1]))

tt <- subset(t, first_half==1)
print(paste0("C: ", AUC(tt$selected_team_win, tt$prob_selected_team_win_d)[1]))

ttt <- subset(t, first_half==0)
print(paste0("C: ", AUC(ttt$selected_team_win, ttt$prob_selected_team_win_d)[1]))

tttt <- subset(results[[1]], is.na(selected_team_win)==TRUE)
mean(tttt$prob_selected_team_win_d)
mean(tttt$prob_selected_team_win_d>0.5)
mean(tttt$d_pred_selected_team_win)

mean(t$match)
mean(t$d_pred_selected_team_win)


#box_scores <- readRDS("/Users/kimlarsen/Documents/Code/NBA_RANKINGS/cleandata/box_scores_2016-11-22.RDA") 

#box_scores <- mutate(box_scores, future_game = ifelse(DATE>=as.Date('2016-11-20'), 1, 0), 
#                                 selected_team_win=ifelse(DATE>=as.Date('2016-11-20'), NA, selected_team_win))



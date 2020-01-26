assign_clusters_and_win_rates <- function(root, datemap, box_scores, weighted_win_rates, cluster_window){

   centroids <- readRDS(paste0(root, "/centroids/centroids.RDA"))
   s <- min(subset(datemap, season==ignore_season_prior_to)$DATE_INDEX)
   e <-max(subset(datemap, future_game==0)$DATE_INDEX) 
   ncore <- detectCores()-2
   registerDoParallel(ncore)
   i=e
   loop_result <- foreach(i=s:e) %dopar% {

      ### Get the data inside the window  
      thisseason <- as.numeric(datemap[i, "season"])
      inwindow <- filter(box_scores, DATE_INDEX<i & DATE_INDEX>i-cluster_window)
      thisdate <- filter(box_scores, DATE_INDEX==i)
      #thisseason <- as.numeric(thisdate[1,"season"])
  
      ## Get the win percentages
      w <- weighted_win_rates 
      if (thisseason != current_season){
         w <-0 
      }
      win_perc1 <- winpercentages(filter(inwindow, DATE_INDEX>i-winstreak_window), thisseason, w)
      win_perc2 <- winpercentages(filter(inwindow, DATE_INDEX>i-winstreak_window_s), thisseason, w)
  
      ## Assign clusters
      clusters <- assign_clusters(centroids, inwindow, cutoff, thisseason)
      print(unique(clusters$Cluster))
  
      ### Join
      t <- inner_join(thisdate, select(clusters, PLAYER_FULL_NAME, Cluster), by="PLAYER_FULL_NAME")
      f <- attach_win_perc(t, win_perc1, win_perc2)
  
      rm(win_perc1)
      rm(win_perc2)
  
      return(f)
   }
   ttt <- data.frame(rbindlist(loop_result))
   return(ttt)
}

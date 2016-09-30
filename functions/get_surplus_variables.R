get_surplus_variables <- function(data){
  nclus <- length(unique(data$Cluster))
  for (j in min(data$Cluster):max(data$Cluster)){
    data[,paste0("share_minutes_cluster_", j)] <- data$share_of_minutes_signed * as.numeric(rfdata$Cluster==j)
  }
  return(data)
}
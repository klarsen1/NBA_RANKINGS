BayesianRegression <- function (time, y, rhs, data, betabar, sigma, draws, fixedvar, intercept) {
  if (intercept==TRUE){
    rhs <- paste("~", rhs, sep="")      
    sigma <- c(10000, sigma)
    betabar <- c(0, betabar)
  } else{
    rhs <- paste("~0+", rhs, sep="")      
    sigma <- c(sigma)
    betabar <- c(betabar)    
  }
  
  c <- 1
  if (fixedvar==TRUE){
    c <- 0
  }
  
  Actual <- data[,c(time, y)]
  names(Actual) <- c("Time", "Actual")
  
  X = model.matrix(as.formula(rhs), data)
  #print(paste("Number of variables in the model: ", dim(X)[2], sep=""))
  A = diag(1/sigma**2)
  k = length(betabar)
  RA = chol(A)
  W = rbind(X, RA)
  z = c(as.vector(data[[y]]), as.vector(RA %*% betabar))
  IR = backsolve(chol(crossprod(W)), diag(k))
  ResultList <- list(length=draws)
  for (i in 1:draws){
    draw <- data.frame(matrix(nrow=k, ncol=6))
    names(draw) <- c("Variable", "Posterior", "Draw")
    draw$Posterior <- as.numeric(crossprod(t(IR)) %*% crossprod(W, z) + c*IR %*% rnorm(k))
    draw$Variable <- colnames(X)
    draw$Draw <- i     
    cov <- solve(t(W) %*% W) * sum((z - W %*% draw$Posterior)**2)/(dim(X)[1]-dim(X)[2])
    draw$SE <- sqrt(diag(cov))
    draw$T <- draw$Posterior/draw$SE
    draw$P <- 2 * (1 - pt(abs(draw$T), dim(X)[1]-length(draw$Posterior)))
    ResultList[[i]] <- draw
    rm(draw)
  }
  ResultDF <- data.frame(data.table::rbindlist(ResultList))  
  rm(ResultList)
  gc()
  
  Means <- data.frame(aggregate(cbind(ResultDF$Posterior, ResultDF$SE, ResultDF$T, ResultDF$P) ~ ResultDF$Variable, ResultDF, FUN="mean"))
  names(Means) <- c("Variable", "Mean", "REGRESSION_SE", "REGRESSION_T", "REGRESSION_P")
  
  SDs <- data.frame(aggregate(ResultDF$Posterior ~ ResultDF$Variable, ResultDF, FUN="sd"))
  names(SDs) <- c("Variable", "CREDIBLE_SE")
  
  Means <- Means[match(colnames(X), Means$Variable),]  
  
  Predicted <- as.vector(X %*% as.vector(Means$Mean))
  Residual <- as.vector(data[[y]]) - Predicted
  Means[Means$Variable=="(Intercept)", "Mean"] <- Means[Means$Variable=="(Intercept)", "Mean"] + mean(Residual)
  
  Predicted <- as.vector(X %*% as.vector(Means$Mean))
  Residual <- as.vector(data[[y]]) - Predicted  
  
  FinalResult <- join(Means, SDs, by="Variable")
  FinalResult <- cbind.data.frame(FinalResult, betabar, sigma)
  
  names(FinalResult) <- c("Variable", "Posterior", "SE", "T", "P", "CREDIBLE_SE", "PriorMean", "PriorSE") 
  
  FinalResult$Posterior <- round(FinalResult$Posterior,5)
  FinalResult$SE <- round(FinalResult$SE,5)
  FinalResult$T <- round(FinalResult$T,5)
  FinalResult$P <- round(FinalResult$P,5)
  FinalResult$CREDIBLE_SE <- round(FinalResult$CREDIBLE_SE,5)
  
  if (draws>1){
    FinalResult$T <- NULL
    FinalResult$P <- NULL
    FinalResult$SE <- NULL    
  } else{
    FinalResult$CREDIBLE_SE <- NULL        
  }    
  return(list(FinalResult, cbind.data.frame(Actual, Predicted, Residual)))
}
TransferFunction <- function(x, lags=NULL, alpha=NULL, customweights=NULL){
  ones <- rep(1.0, length(x))
  if (is.null(customweights)){
    result <- x
    sumWeight <- 1
    if (lags>0){
      for (i in 1:lags){
        result <- result + alpha**i * lagp(x, i)  
        sumWeight <- sumWeight + lagp(ones, i) * alpha**i
      }  
    }
    return(result/sumWeight)
  } else{
    result <- x * customweights[1]
    for (i in 2:length(customweights)){
      result <- result + customweights[i] * lagp(x, i-1)  
    }  
    return(result)    
  }
}
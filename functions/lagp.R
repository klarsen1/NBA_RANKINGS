lagp <- function(x, p){  
  return(c(rep(0,p), x[1:(length(x)-p)]))
}
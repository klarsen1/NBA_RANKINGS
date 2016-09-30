get_xmatrix <- function(data, remove){
  f <- as.formula(Y ~ .)
  d <- data.frame(data)[,!(names(data) %in% remove)]
  X <- model.matrix(f, d)[, -1]
  return(X)
}





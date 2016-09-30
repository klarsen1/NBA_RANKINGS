get_coefficients <- function(X, Y){
  model <- cv.glmnet(y=Y, x=X, family="binomial", alpha=alpha, parallel=FALSE, nfolds=10)   
  c <- as.matrix(coef(model, s=model$lambda.1se))
  selected <- cbind.data.frame(sapply(row.names(c), as.character), sapply(c, as.numeric))
  names(selected) <- c("Variable", "Coeff")
  return(c[-1])
}
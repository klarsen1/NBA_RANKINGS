trim <- function(x){
  x <- gsub("^\\s+|\\s+$|", "", x)
  gsub("\\s+", " ", x)
}
env_mean_preprocessing <- function(data){
  env_mean <- data[,4:ncol(data)]
  env_mean <- textshape::column_to_rownames(env_mean,"Point")
  return(env_mean)
}
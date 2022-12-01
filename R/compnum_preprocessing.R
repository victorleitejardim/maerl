compnum_preprocessing <- function(data){
  compnum <- data[,4:ncol(data)]
  compnum$X <- data$X
  compnum <- compnum[, c("X", names(compnum)[names(compnum) != "X"])]
  compnum <- textshape::column_to_rownames(compnum,"Point")
  return(compnum)
}


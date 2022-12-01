compnum_preprocessing <- function(x){
  compnum <- x[,4:ncol(x)]
  compnum$X <- x$X
  compnum <- compnum[, c("X", names(compnum)[names(compnum) != "X"])]
  compnum <- textshape::column_to_rownames(compnum,"Point")
  return(compnum)
}


compnum_preprocessing <- function(data){
  compnum <- data[,4:ncol(data)]
  compnum <- textshape::column_to_rownames(compnum,"Point")
  return(compnum)
}

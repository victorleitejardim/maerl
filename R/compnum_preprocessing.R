compnum_preprocessing <- function(data){
  compnum <- data[,c(4, 5, 7:9, 12, 15, 17)]
  compnum <- textshape::column_to_rownames(compnum,"Point")
  return(compnum)
}

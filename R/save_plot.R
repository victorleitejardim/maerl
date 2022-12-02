#' Save a plot object
#'
#' @description 
#' This function a [`ggplot2`] object.
#'
#' @param path a character of length 1. The full path of the file name to save.
#' 
#' @param plot a [`ggplot2`] object.
#'
#' @return The full path of the file name
#' 
#' @importFrom ggplot2 ggsave
#' @export

save_plot <- function(path, plot){
  
  ggsave(path, plot)
  path
}
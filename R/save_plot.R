#' save_plot
#'
#' @param path 
#' @param plot 
#'
#' @return
#' @export
#'
#' @examples
save_plot <- function(path, plot){
  ggsave(path, plot)
  path
}
#' Read the bccomp_med data 
#'
#' @description 
#' This function reads the bccomp_med `csv` file named 
#' `env_mean.csv` stored in `data/`.
#' @param file a character of length 1. The path to the csv file.
#' @return A `tibble` 
#' @export
#'
#' @examples
#' #' ## Import data ----
#' bccomp_med_data <- read_bccomp_med(here::here("data",  
#'                                    "env_mean.csv"))

read_bccomp_med <- function(file) {
  
  ## Check if file exists ----
  
  if (!file.exists(file)) {
    stop("The file '", file, "' does not exist. Please run ", 
         "env_mean to download it.", call. = FALSE)
  }
  
  
  ## Check if file name is good ----
  
  if (basename(file) != "env_mean.csv") {
    stop("Wrong file name ('env_mean.csv')", call. = FALSE)
  }
  
  
  ## Open file ----
  
  suppressMessages(readr::read_csv(file))
}

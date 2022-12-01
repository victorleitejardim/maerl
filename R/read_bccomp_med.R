#' Read the bccomp_med data 
#'
#' @description 
#' This function reads the bccomp_med `csv` file named 
#' `bccomp_med.csv` stored in `data/`.
#' @param file a character of length 1. The path to the csv file.
#' @return A `tibble` 
#' @export
#'
#' @examples
#' #' ## Import data ----
#' bccomp_med_data <- read_bccomp_med(here::here("data",  
#'                                    "bccomp_med.csv"))

read_bccomp_med <- function(file) {
  
  ## Check if file exists ----
  
  if (!file.exists(file)) {
    stop("The file '", file, "' does not exist. Please run ", 
         "bccomp_med to download it.", call. = FALSE)
  }
  
  
  ## Check if file name is good ----
  
  if (basename(file) != "bccomp_med.csv") {
    stop("Wrong file name ('bccomp_med.csv')", call. = FALSE)
  }
  
  
  ## Open file ----
  
  suppressMessages(readr::read_csv(file))
}

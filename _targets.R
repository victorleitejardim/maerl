#' Targets plan
#' 
#' @author Liz Loutrage \email{liz.loutrage@gmail.com}
#' 
#' @date 2022/12/01


## Attach required packages ----
library(targets)
library(ggplot2)

tar_source()

list(
  #read data
  tar_target(bccomp_med_data,read_bccomp_med(here::here("data", "bccomp_med.csv"))),
  
  # Prepare data 
  tar_target (compnum, compnum_preprocessing(bccomp_med_data))
  
)
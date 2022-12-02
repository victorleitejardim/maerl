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
  # read complexity data
  tar_target(bccomp_med_data,read_bccomp_med(here::here("data", "bccomp_med.csv"))),
  
  # Prepare data 
  tar_target(compnum, compnum_preprocessing(bccomp_med_data)),
  
  # Plot rda for complexity data 
  tar_target(rda_bccomp_med, rda_function(compnum)),
  
  # read env data 
  tar_target(env_data,read_env_mean(here::here("data", "env_mean.csv"))),
  
  # Plot rda for env  data 
  tar_target(rda_env_data, rda_function(env_data))
  
)
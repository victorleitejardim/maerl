#' 
#' @author 
#' - Liz Loutrage
#' - Thomas Benoit
#' - Victor Leite Jardim
#' 
#' @date 2022/12/02

## Install Dependencies (listed in DESCRIPTION) ----
remotes::install_deps(upgrade = "never")

## Run Project ----

targets::tar_make()
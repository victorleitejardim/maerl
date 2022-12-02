#' Targets plan
#' 
#' @author Liz Loutrage \email{liz.loutrage@gmail.com}
#' 
#' @date 2022/12/01

## Attach required packages ----
library(targets)
library(ggplot2)
library(bgplot)

tar_source()

list(
  # read complexity data----
  tar_target(bccomp_med_data,read_bccomp_med(here::here("data", "bccomp_med.csv"))),
  
  # Prepare complexity data 
  tar_target(compnum, compnum_preprocessing(bccomp_med_data)),
  
  # Plot rda for complexity data 
  tar_target(rda_bccomp_med, rda_function(compnum)),
  
  # read env data----
  tar_target(env_data,read_env_mean(here::here("data", "env_mean.csv"))),
  
  # Prepare env data 
  tar_target(env_data_processing, env_mean_preprocessing(env_data)),
  
  # Plot rda for env  data 
  tar_target(rda_env_data, rda_function(env_data_processing)),
  
  #compute outputs ----
  #plot complexity data
  tar_target(plot_rdabccomp_med, bg_pca(rda_bccomp_med, metadata = bccomp_med_data, main.group = "Site",
                                         scale.fill = c("#4477AA", "#8ECDDE", "#44AA99", "#858c64", "#e8bb5a",
                                                       "#EE8866", "#d44e65", "#FFAABB", "#7b538c", "#80898f" ),
                                         scale.colour = c("#4477AA", "#8ECDDE", "#44AA99", "#858c64", "#e8bb5a",
                                                          "#EE8866", "#d44e65", "#FFAABB", "#7b538c", "#80898f" ),
                                         goodness.thresh = 0.0,
                                         add.centroids = TRUE, stat1 = "chull",
                                         ysites = c(-1.1, .8), xsites = c(-1.1, .8), ysp = c(-1.7, 1.7),
                                         xsp = c(-1.7, 1.7), axis.size = 12, axis.text = 12, c.size = 1.5,
                                         font.size = 8/.pt, ext.plot.scale = 2.5, add.labels = FALSE, point.size = 1)),
  #plot env data
  tar_target(plot_env_data, bg_pca(rda_env_data, metadata = env_data, main.group = "Site",
                                   scale.fill = c("#4477AA", "#8ECDDE", "#44AA99", "#858c64", "#e8bb5a",
                                                  "#EE8866", "#d44e65", "#FFAABB", "#7b538c", "#80898f" ),
                                   scale.colour = c("#4477AA", "#8ECDDE", "#44AA99", "#858c64", "#e8bb5a",
                                                    "#EE8866", "#d44e65", "#FFAABB", "#7b538c", "#80898f" ),
                                   goodness.thresh = 0.0, ysites = c(-1, 1.5), xsites = c(-1, 1.5),
                                   ysp = c(-2, 2.1), xsp = c(-2, 2.1),
                                   add.centroids = TRUE, stat1 = "chull", conf.level = .8,
                                   axis.size = 12, axis.text = 12, c.size = 1.5,
                                   font.size = 8/.pt, ext.plot.scale = 2.5, add.labels = FALSE, point.size = 1)),
  tar_target(pca_comp, save_plot(here::here("figures", "bgpca_comp.png"), plot_rdabccomp_med)),
  
  tar_target(pca_env, save_plot(here::here("figures", "bgpca_env.png"), plot_env_data))
  
  
)
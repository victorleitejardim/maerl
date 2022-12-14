
<!-- README.md is generated from README.Rmd. Please edit that file -->

# maerl

<!-- badges: start -->
<!-- badges: end -->

Research Compendium of the project maerl for the rdatatoolbox course
from the GDR Ecostat course at CESAB

### How to cite

Please cite this compendium as:

> Benoit T., Jardim V. L. & Loutrage L. (2022) maerl: Projet rdatoolbox
> CESAB. URL: <https://victorleitejardim.github.io/maerl/>.

### Content

This repository is structured as follow:

- [`data/`](https://github.com/victorleitejardim/maerl/tree/master/data):
  contains all raw data required to perform analyses

- [`analyses/`](https://github.com/victorleitejardim/maerl/tree/master/analyses/):
  contains R scripts to run each step of the workflow

- [`outputs/`](https://github.com/victorleitejardim/maerl/tree/master/outputs):
  contains all the results created during the workflow

- [`figures/`](https://github.com/victorleitejardim/maerl/tree/master/figures):
  contains all the figures created during the workflow

- [`R/`](https://github.com/victorleitejardim/maerl/tree/master/R):
  contains R functions developed especially for this project

- [`DESCRIPTION`](https://github.com/victorleitejardim/maerl/tree/master/DESCRIPTION):
  contains project metadata (author, date, dependencies, etc.)

- [`make.R`](https://github.com/victorleitejardim/maerl/tree/master/make.R):
  main R script to run the entire project by calling each R script
  stored in the `analyses/` folder

### Usage

Clone the repository, open R/RStudio and run:

``` r
source("make.R")
```

### Notes

- All required packages, listed in the `DESCRIPTION` file, will be
  installed (if necessary)
- All required packages and R functions will be loaded
- Some analyses listed in the `make.R` might take time

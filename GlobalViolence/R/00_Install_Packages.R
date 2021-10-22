

if (! "pacman" %in% rownames(installed.packages())){
  install.packages("pacman")
}

library(pacman)

CRAN_packages <- c("here","data.table","countrycode","ungroup","reshape2","magrittr","parallel","dplyr","readr",
                   "readxl","openxlsx","tidyverse","rio","purrr","viridis","RColorBrewer","ggpubr",
                   "lintr","sf","cowplot","gridExtra","grid","stringr","biscale","sp","cartography","tmap",
                   "magrittr")

# Install required CRAN packages if not available yet
if(!sum(!p_isinstalled(CRAN_packages))==0) {
  p_install(
    package = CRAN_packages[!p_isinstalled(CRAN_packages)], 
    character.only = TRUE
  )
}

GITHUB_packages <- c("DemoTools","MortalitySmooth","parallelsugar", "DistributionTTD", "DemoDecomp","ggrastr")


if (!p_isinstalled("DemoTools")) {
  install.packages("rstan", repos = "https://mc-stan.org/r-packages/", getOption("repos"))
  remotes::install_github("timriffe/DemoTools", build = FALSE)
}
if (!p_isinstalled("parallelsugar")){
  remotes::install_github("nathanvan/parallelsugar")
}
if (!p_isinstalled("MortalitySmooth")){
  remotes::install_github("timriffe/MortalitySmooth")
}
if (!p_isinstalled("DistributionTTD")){
  remotes::install_github("timriffe/DistributionTTD/DistributionTTD/R/DistributionTTD")
}
if (!p_isinstalled("DemoDecomp")){
  remotes::install_github("timriffe/DemoDecomp")
}

if (!p_isinstalled("ggrastr")){
  devtools::install_github('VPetukhov/ggrastr')
}




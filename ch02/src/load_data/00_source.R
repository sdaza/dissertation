#######################################
# source files CDC mortality micro-data
# author: sebastian daza
######################################

# clean working space
library(here)

# set directory
setwd(here())

# load mortality data
source('01_load_mortality_files.R')

# load population data
# source(paste0(path, "src/02_load_population_files.R"))

# merge with chetty's data
source(paste0(path, "src/03_merge_mortality_chetty.R"))

# source(paste0(path, "src/hipd.R"))
source("src/cdc.R")

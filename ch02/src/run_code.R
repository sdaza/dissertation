########################################
# CDC mortality - income mobility paper
# author: Sebastian Daza
########################################

# install packages

# install.packages('devtools')
# install.packages('haven')
# devtools::install_github("sdaza/sdazar")
# install.packages('ggplot2')
# install.packages('ggridges')
# install.packages('ggthemes')

# install.packages('fmsb')
# install.packages('texreg')
# install.packages('xtable')
# install.packages("INLA", repos = c(getOption("repos"),
#                  INLA = "https://inla.r-inla-download.org/R/testing"), dep = TRUE)
# devtools::install_github("julianfaraway/brinla")
# install.packages('maptools')
# install.packages('USAboundaries')


# # create mortality file diccionary
# source("src/utils/define_dict_mortality_cdc.R")

# # load data
# source('src/load_data/01_load_mortality_files.R')
# source('src/load_data/02_load_population_files.R')
# source('src/load_data/03_preprocessing_chetty_data.R')
# source("src/load_data/04_merge_mortality_chetty.R")
# source("src/load_data/05_county_graph_object.R")
# source('src/load_data/06_descriptives.R')

# # relative mobility analysis
# source('src/relative_mobility/00_cdc_inla_age_models_prior_sensitivity.R')
# source('src/relative_mobility/01_cdc_inla_age_models_pcprior_1_10.R')
# source('src/relative_mobility/02_cdc_inla_age_race_models_pcprior_1_10.R')
# source('src/relative_mobility/03_cdc_inla_age_cause_models_pcprior_1_10.R')


# # absolute mobility analysis
# source('src/absolute_mobility/01_cdc_inla_age_models_pcprior_1_10_abs.R')
# source('src/absolute_mobility/02_cdc_inla_age_race_models_pcprior_1_10_abs.R')
source('src/absolute_mobility/03_cdc_inla_age_cause_models_pcprior_1_10_abs.R')
########################################
# CDC mortality - income mobility paper
# author: Sebastian Daza
########################################

# create mortality file diccionary
source("src/utils/define_dict_mortality_cdc.R")

# load data
source('src/load_data/01_load_mortality_files.R')
source('src/load_data/02_load_population_files.R')
source('src/load_data/03_preprocessing_chetty_data.R')
source("src/load_data/04_merge_mortality_chetty.R")
source("src/load_data/05_count_graph_object.R")

# descriptive table and plot
source('src/descriptives.R')

# relative mobility analysis
source('src/relative_mobility/00_cdc_inla_age_models_prior_sensitivity.R')
source('src/relative_mobility/01_cdc_inla_age_models_pcprior_1_10.R')

# absolute mobility analysis

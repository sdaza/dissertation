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

# descriptive table
source('src/descriptive_table.R')

# relative mobility analysis

# absolute mobility analysis
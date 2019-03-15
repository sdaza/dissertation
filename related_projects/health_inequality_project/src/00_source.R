#############################################
# reproduce results health inequality paper
# author: sebastian daza
#############################################

# load data
source('related_projects/health_inequality_project/src/01_load_data.R')

# descriptive stats
source('related_projects/health_inequality_project/src/02_descriptive.R')

# run models by quartile
source('related_projects/health_inequality_project/src/03_models_by_quartile.R')

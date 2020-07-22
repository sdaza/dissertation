###################################
# county income mobility and individual health
# nlsy97 setup
# author: sebastian daza
###################################


source("src/utils.R")

# chetty's data
source("src/01_chetty_county_data.R")

# nlsy
source("src/nlsy97/01_nlsy97_location_data.R")
source("src/nlsy97/02_nlsy97_household_data.R")
source("src/nlsy97/03_nlsy97_individual_data.R")
source("src/nlsy97/04_nlsy97_descriptive_table.R")
source("src/nlsy97/05_nlsy97_imputation.R")
source("src/nlsy97/07_nlsy97_msm.R")
source("src/nlsy97/08_nlsy97_msm_tables.R")
source("src/nlsy97/09_nlsy97_weight_table.R")

# psid
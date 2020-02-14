###################################
# county income mobility and individual health
# nlsy97 setup
# author: sebastian daza
###################################


library(texreg)
source("ch03/src/utils.R")

# chetty's data
source("ch03/src/01_chetty_county_data.R")

# nlsy
source("ch03/src/nlsy97/01_nlsy97_location_data.R")
source("ch03/src/nlsy97/02_nlsy97_household_data.R")
source("ch03/src/nlsy97/03_nlsy97_individual_data.R")

source("ch03/src/04_nlsy97_descriptive_table.R")
source("ch03/src/05_nlsy97_imputation.R")
source("ch03/src/06_nlsy97_msm.R")

# psid
source("ch03/src/psid/01_psid_individual_data.R")
source("ch03/src/psid/02_psid_descriptive_table.R"

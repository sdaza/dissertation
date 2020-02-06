###################################
# county income mobility and individual health
# nlsy97 setup
# author: sebastian daza
###################################


library(texreg)

# chetty's data
source("ch03/src/00_chetty_county_data.R")

# nlsy
source("ch03/src/01_nlsy97_location_data.R")
source("ch03/src/02_nlsy97_household_data.R")
source("ch03/src/03_nlsy97_individual_data.R")

source("ch03/src/04_nlsy97_imputation.R")
source("ch03/src/04_nlsy97_msm.R")

# psid
source("ch03/src/07_psid_individual_data.R")

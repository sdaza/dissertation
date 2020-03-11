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
source("src/nlsy97/06_nlsy97_msm.R")

# linstat runs
R < src/nlsy97/05_nlsy97_imputation.R > output/log/nlsy97_z_relative_mob_imputation.log --no-save &
R < src/nlsy97/05_nlsy97_imputation.R > output/log/nlsy97_z_absolute_mob_imputation.log --no-save &

# psid
source("src/psid/01_psid_individual_data.R")
source("src/psid/02_psid_descriptive_table.R")
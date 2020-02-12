##############################
# county income mobility and individual health
# imputation PSID data
# author: sebastian daza
##############################

# libraries
library(data.table)
library(hash)
library(mice)
library(miceadds)
library(texreg)
source("ch03/src/utils.R")

# read data
mm = readRDS("ch03/output/data/psid_data_ready_for_imputation.rds")
mm[, age := imp_age]

center_vars = c("bmi", "depression", "imp_age",
                "famsize",  "head_education", "mother_age", "first_year")

mm[, (center_vars) := lapply(.SD, scale, scale = FALSE), .SDcol = center_vars]
mm = mm[head_wife == 1]

# 3167, about 3041 in the enclave server
length(unique(mm$pid))

# factors
mm[, race := factor(as.numeric(race))]
# mm[, first_year := as.factor(first_year)]
# mm[, head_education := as.factor(head_education)]
# mm[, imp_age2 := imp_age ^ 2]
# mm[, health := as.factor(health)]

# select variables to impute
mm = mm[, .(pid, year, time, imp_age, year_born, first_year, male, race, 
            log_income_adj, head_owns_house, famsize, head_working_binary, 
            mother_age, mother_marital_status, weight_less_55, 
            head_marital_status, head_education,
            # nmoves, z_gini, z_relative_mobility, z_absolute_mobility, 
            # log_population, log_county_income, 
            bmi, smoking, smoking_number, individual_health, depression)]

# revert some variables
mm[, rev_health := reverseScale(individual_health)]
table(mm$rev_health)

# run imputation files

# z_relative_mobility
source("ch03/src/imputations/psid_imputation_z_relative_mob.R")

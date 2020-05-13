# county income mobility and individual health
##############################
# imputation PSID data
# author: sebastian daza
##############################

# libraries
library(data.table)
library(hash)
library(mice)
library(miceadds)
library(texreg)
library(forcats)
source("src/utils.R")

# seed
set.seed(102030)


# read data
mm = readRDS("output/data/psid_data_ready_for_imputation_county_info.rds")
mm[, age := imp_age]

names(mm)
center_vars = c("bmi", "depression", "imp_age",
                "famsize",  "head_education",
                "mother_age", "first_year")

mm[, (center_vars) := lapply(.SD, scale, scale = FALSE), .SDcol = center_vars]
mm = mm[head_wife == 1]

# 3041 in the enclave server
length(unique(mm$pid))

# fill last version of time-varying variables
vars = c("bmi", "depression", "smoking", "smoking_number", "rev_health",
         "log_income_adj", "head_marital_status", "head_education",
         "head_owns_house", "famsize", "head_working_binary",
         "imp_age")

mm[year == 2017, paste0("last_wave_", vars) :=
     lapply(.SD[year == 2017], function(x) x), .SDcol = vars]

mm[, paste0("last_wave_", vars) :=
     lapply(.SD, fillWithFirstValue),
   .SDcol = paste0("last_wave_", vars), pid]


# factors
table(mm$race)
mm[, white := ifelse(race == "white", 1, 0)]
mm[, z_absolute_mob := z_absolute_mob * -1]
mm[, q_absolute_mob := fct_rev(q_absolute_mob)]
mm[, last_wave_smoking_number := as.integer(last_wave_smoking_number)]

# mm[, first_year := as.factor(first_year)]
# mm[, head_education := as.factor(head_educartion)]
# mm[, imp_age2 := imp_age ^ 2]
# mm[, health := as.factor(health)]

# select variables to impute
mm = mm[, .(pid, year, time, imp_age, year_born, first_year, male, white,
            log_income_adj, head_owns_house, famsize, head_working_binary,
            mother_age, mother_marital_status, weight_less_55,
            head_marital_status, head_education,
            nmoves, z_gini, z_relative_mob, z_absolute_mob,
            relative_mob_resid, absolute_mob_resid, gini_resid,
            q_relative_mob, q_absolute_mob, q_gini,
            q_relative_mob_resid, q_absolute_mob_resid, q_gini_resid,
            log_population, log_county_income, z_prop_black,
            last_wave_bmi, last_wave_smoking, last_wave_smoking_number,
            last_wave_rev_health, last_wave_depression,
            last_wave_imp_age)]


# run imputation files

# z_relative_mobility
# source("src/psid/imputations/psid_imputation_z_relative_mob.R")

# z_absolute_mobility
# source("src/psid/imputations/psid_imputation_z_absolute_mob.R")

# q_relative_mobility
# source("src/psid/imputations/psid_imputation_q_relative_mob.R")

# q_absolute_mobility
# source("src/psid/imputations/psid_imputation_q_absolute_mob.R")

# z_relative_mobility residuals
# source("src/psid/imputations/psid_imputation_zr_relative_mob.R")

# z_absolute_mobility residuals
# source("src/psid/imputations/psid_imputation_zr_absolute_mob.R")

# q_relative_mobility residuals
# source("src/psid/imputations/psid_imputation_qr_relative_mob.R")

# q_absolute_mobility residuals
# source("src/psid/imputations/psid_imputation_qr_absolute_mob.R")


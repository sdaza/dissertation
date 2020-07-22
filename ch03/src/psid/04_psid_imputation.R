##############################
# county income mobility and individual health
# imputation PSID data
# author: sebastian daza
##############################


# libraries
library(data.table)
library(miceadds)
library(micemd)
library(texreg)
library(hash)
library(forcats)
source("src/utils.R")

# set random seed
seed = 51420958
# slackr::slackr_setup(config_file = ".slackr")

# read data
mm = readRDS("output/data/psid_data_ready_for_imputation_county_info.rds")
mm[, age := imp_age]
print(summary(mm[, .N, .(pid)]))
print(paste0("Number of rows: ", nrow(mm)))

# add weights
sw = readRDS('output/data/psid_sampling_weights.rds')
mm = merge(mm, sw, by = "pid")
print(paste0("Number of rows: ", nrow(mm)))

# center weights for imputation models
mm[, csweight := scale(sweight, scale = FALSE)]

# center other variables
center_vars = c("bmi", "depression", "imp_age", "famsize",  "head_education", 
    "mother_age", "first_year")
mm[, (center_vars) := lapply(.SD, scale, scale = FALSE), .SDcol = center_vars]

# only heads or wifes in the household
mm = mm[head_wife == 1]

# 3041 in the enclave server
print(paste0("Number of respondents " , length(unique(mm$pid))))

# fill last version of time-varying variables
vars = c("bmi", "depression", "smoking", "smoking_number", "rev_health")

mm[year == 2017, paste0("last_wave_", vars) :=
    lapply(.SD[year == 2017], function(x) x), .SDcol = vars]

mm[, paste0("last_wave_", vars) := lapply(.SD, fillWithFirstValue), 
   .SDcol = paste0("last_wave_", vars), pid]

# revert absolute mobility variables
mm[, z_absolute_mob := z_absolute_mob * -1]
mm[, q_absolute_mob := fct_rev(q_absolute_mob)]
mm[, absolute_mob_resid := absolute_mob_resid * -1]
mm[, q_absolute_mob_resid := fct_rev(q_absolute_mob_resid)]

# select columns to be included in the imputation
mm = mm[, .(pid, year, time, imp_age, year_born, first_year, male, race,
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
    stratum, cluster, sweight, csweight)]

# run imputation looop
source("src/psid/05_psid_imputation_loop.R")
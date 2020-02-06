##############################
# county income mobility and individual health
# imputation PSID data
# author: sebastian daza
##############################

# libraries
library(data.table)
library(hash)
library(mice)
library(texreg)
source("ch03/src/utils.R")

# read data
mm = readRDS("ch03/output/data/psid_data_ready_for_imputation.rds")
mm[, age := imp_age]

names(mm)


center_vars = c("bmi", "depression", "life_satisfaction", "imp_age",
                "famsize",  "head_education", "mother_age", "")
mm[, (center_vars) := lapply(.SD, scale, scale = FALSE), .SDcol = center_vars]
mm = mm[head_wife == 1]

# 3167
length(unique(mm$pid))

# mm[, health := as.factor(health)]
mm[, race := factor(as.numeric(race))]
mm[, first_year := as.factor(first_year)]
# mm[, head_education := as.factor(head_education)]
# mm[, imp_age2 := imp_age ^ 2]


# hist(mm$imp_age)
# mm = mm[, .(pid, year, imp_age, year_born, first_year, male, race, log_income_adj,
#             weight_less_55, marital_status_mother, age_mother,
#             bmi, depression)]

# mm[, weight_less_55 := as.factor(weight_less_55)]
# mm[, marital_status_mother := as.factor(marital_status_mother)]

# run imputation files

# z_relative_mobility
source("ch03/src/imputations/psid_imputation_z_relative_mob.R")

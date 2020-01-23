##############################
# county income mobility and individual health
# imputation NLSY97 data
# author: sebastian daza
##############################

# libraries
library(data.table)
library(miceadds)
library(texreg)
library(hash)
library(forcats)
source("ch03/src/utils.R")

# read data
ldat = readRDS("ch03/output/data/nlsy97_data_ready_for_imputation.rds")
summary(ldat[, .N, .(id)])

# check number of moves
moves = ldat[, .(moves = max(nmoves)), id]
prop.table(table(moves$moves >= 3))
hist(moves$moves)

table(ldat$nmoves)
# explore some cases
ids = unique(ldat$id)
ldat[id == sample(ids, 1), .(id, year, time, male, age_interview_est)]

# select columns to be included in the imputation
mm = ldat[, .(id, time, stime, imp_fips, year, exposure_time,
              male, ethnicity, max_age_interview_est,
              age_interview_est, hhsize,
              z_relative_mob, z_absolute_mob, z_gini,
              relative_mob_resid, absolute_mob_resid, gini_resid,
              q_relative_mob, q_absolute_mob, q_gini,
              q_relative_mob_resid, q_absolute_mob_resid, q_gini_resid,
              log_population, log_county_income,
              asvab_score,
              imp_living_any_parent, imp_parent_employed,
              imp_parent_married,
              log_income_adj, parent_education, mother_age_at_birth,
              residential_moves_by_12, nmoves,
              rev_health, bmi, depression, smoking_ever, smoking_30,
              wt, stratum, type)]

# center variables
center_vars = c("hhsize", "asvab_score", "parent_education",
                "mother_age_at_birth", "residential_moves_by_12", "nmoves",
                "bmi", "depression")
mm[, (center_vars) := lapply(.SD, scale, scale = FALSE), .SDcol = center_vars]
mm[, rev_health := as.factor(rev_health)]
mm[, age_interview_est := as.factor(age_interview_est)]

# rever variables
mm[, z_absolute_mob := z_absolute_mob * -1]
mm[, q_absolute_mob := fct_rev(q_absolute_mob)]

# run imputation files

# z_relative_mobility
# source("ch03/src/imputations/nlsy97_imputation_z_relative_mob.R")

# z_relative_mobility quintile
# source("ch03/src/imputations/nlsy97_imputation_z_relative_mob_quintile.R")

# # z_absolute mobility
# source("ch03/src/imputations/nlsy97_imputation_z_absolute_mob.R")

# z_absolute_mobility quintile
source("ch03/src/imputations/nlsy97_imputation_z_absolute_mob_quintile.R")

# # relative_mobility_resid
# source("ch03/src/imputations/nlsy97_imputation_relative_mob_resid.R")

# # absolute_mobility_resid
# source("ch03/src/imputations/nlsy97_imputation_absolute_mob_resid.R")


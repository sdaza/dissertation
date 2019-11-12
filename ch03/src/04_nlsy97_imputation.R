##############################
# imputation NLSY97 data
# author: sebastian daza
# version: 0.01
##############################

# libraries
library(data.table)
library(sdazar)
library(hash)
library(mice)

source("ch03/src/utils.R")

# read data
ldat = readRDS("ch03/output/data/nlsy97_data_ready_for_imputation.rds")

# select columns to be included in the imputation
mm = ldat[, .(id, imp_fips, year, exposure_time, male, ethnicity, max_age_interview_est,
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
              residential_moves_by_12,
              health, bmi, depression, smoking_ever, smoking_30,
              wt, stratum, type)]

# center variables
center_vars = c("hhsize", "asvab_score", "parent_education",
                "mother_age_at_birth", "residential_moves_by_12")
mm[, (center_vars) := lapply(.SD, scale), .SDcol = center_vars]

center_vars = c("bmi", "depression")
mm[, (center_vars) := lapply(.SD, scale, scale = FALSE), .SDcol = center_vars]
mm[, health := as.factor(health)]
mm[, age_interview_est := as.factor(age_interview_est)]

# independent variable z_relative_mob and z_gini
ini = mice(mm, maxit = 0)
head(ini$loggedEvent)
pred = ini$pred
meth = ini$meth
pred[,] = 0

fluxplot(mm)
# fx = fluxplot(mm)

# set up methods and prediction matrix

methods = hash(
               "hhsize" = "2l.pmm",
               "z_relative_mob" = "",
               "z_absolute_mob" = "",
               "z_gini" = "",
               "relative_mob_resid" = "",
               "absolute_mob_resid" = "",
               "gini_resid" = "",
               "q_relative_mob_resid" = "",
               "q_absolute_mob_resid" = "",
               "q_gini_resid" = "",
               "q_relative_mob" = "",
               "q_absolute_mob" = "",
               "q_gini" = "",
               "log_population" = "",
               "log_county_income" = "",
               "log_income_adj" = "2l.pmm",
               "imp_living_any_parent" = "2l.pmm",
               "imp_parent_employed" = "2l.pmm",
               "imp_parent_married" = "2l.pmm",
               "parent_education" = "2lonly.pmm",
               "mother_age_at_birth" = "2lonly.pmm",
               "residential_moves_by_12" = "2lonly.pmm",
               "asvab_score" = "2lonly.pmm",
               "health" = "2l.pmm",
               "bmi" = "2l.pmm",
               "depression" = "2l.pmm",
               "smoking_ever" = "2l.pmm",
               "smoking_30" = "2l.pmm"
               )

meth[keys(methods)] = values(methods)

# check the structure is fine
# str(mm)

# predictors
predictors = hash(
     "id" = -2,
     "male" = 1,
     "ethnicity" = 1,
     "age_interview_est" = 1,
     "z_relative_mob" = 1,
     "z_absolute_mob" = 0,
     "z_gini" = 1,
     "relative_mob_resid" = 0,
     "absolute_mob_resid" = 0,
     "gini_resid" = 0,
     "q_relative_mob" = 0,
     "q_absolute_mob" = 0,
     "q_gini" = 0,
     "q_relative_mob_resid" = 0,
     "q_absolute_mob_resid" = 0,
     "q_gini_resid" = 0,
     "log_population" = 1,
     "log_county_income" = 1,
     "log_income_adj" = 1,
     "hhsize" = 1,
     "imp_living_any_parent" = 1,
     "imp_parent_employed" = 1,
     "imp_parent_married" = 1,
     "parent_education" = 1,
     "mother_age_at_birth" = 1,
     "residential_moves_by_12" = 1,
     "asvab_score" = 1,
     "health" = 1,
     "bmi" = 1,
     "depression" = 1,
     "smoking_ever" = 1,
     "smoking_30" = 0
    )

pred["hhsize", keys(predictors)] = values(predictors)
# pred["z_relative_mob", keys(predictors)] = values(predictors)
# pred["z_gini", keys(predictors)] = values(predictors)
# pred["log_population", keys(predictors)] = values(predictors)
# pred["log_county_income", keys(predictors)] = values(predictors)
# pred["z_gini", keys(predictors)] = values(predictors)
pred["log_income_adj", keys(predictors)] = values(predictors)
pred["imp_living_any_parent", keys(predictors)] = values(predictors)
pred["imp_parent_employed", keys(predictors)] = values(predictors)
pred["imp_parent_married", keys(predictors)] = values(predictors)

predictors['age_interview_est'] = 0
pred["parent_education", keys(predictors)] = values(predictors)
pred["mother_age_at_birth", keys(predictors)] = values(predictors)
pred["residential_moves_by_12", keys(predictors)] = values(predictors)
pred["asvab_score", keys(predictors)] = values(predictors)

predictors['age_interview_est'] = 1
pred["health", keys(predictors)] = values(predictors)
pred["depression", keys(predictors)] = values(predictors)
pred["bmi", keys(predictors)] = values(predictors)

predictors['smoking_ever'] = 0
predictors['smoking_30'] = 0
pred["smoking_ever", keys(predictors)] = values(predictors)
pred["smoking_30", keys(predictors)] = values(predictors)

# set diagonal of matrix to 0
diag(pred) = 0

countmis(mm)
# explore
pred["health",]
pred["smoking_30",]
pred["bmi",]
pred["z_relative_mob",]
pred["log_population",]
pred["asvab_score",]

# run imputation
imp = mice::mice(mm, predictorMatrix = pred, method = meth,
           m = 5, maxit = 5, seed = 123)

# explore quality of imputations

savepdf("ch03/output/imp_iterations")
print(plot(imp, c("bmi", "health")))
print(plot(imp, c("depression", "smoking_30", "smoking_ever")))
# print(plot(imp, c("z_relative_mob", "z_gini")))
print(plot(imp, c("hhsize", "log_income_adj")))
print(plot(imp, c("imp_living_any_parent", "imp_parent_married", "imp_parent_employed")))
print(plot(imp, c("parent_education", "mother_age_at_birth")))
print(plot(imp, c("asvab_score", "residential_moves_by_12")))
dev.off()

savepdf("ch03/output/imp_values")
print(densityplot(imp, ~ bmi + depression + smoking_30 + smoking_ever))
print(densityplot(imp, ~ health))
print(densityplot(imp, ~ hhsize + log_income_adj))
print(densityplot(imp, ~ imp_living_any_parent + imp_parent_married + imp_parent_employed))
# print(densityplot(imp, ~ z_relative_mob + z_gini))
# print(densityplot(imp, ~ log_population + log_county_income))
print(densityplot(imp, ~ parent_education + mother_age_at_birth + asvab_score + residential_moves_by_12))
dev.off()


# save results of imputation
saveRDS(imp, "ch03/output/data/nlsy97_imputation_individual.rds")

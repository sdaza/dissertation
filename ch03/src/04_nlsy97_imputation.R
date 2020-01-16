##############################
# imputation NLSY97 data
# author: sebastian daza
# version: 0.01
##############################

# libraries
library(data.table)
library(miceadds)
library(hash)
source("ch03/src/utils.R")

# read data
ldat = readRDS("ch03/output/data/nlsy97_data_ready_for_imputation.rds")
summary(ldat[, .N, .(id)])

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
               "rev_health" = "2l.pmm",
               "bmi" = "2l.pmm",
               "depression" = "2l.pmm",
               "smoking_ever" = "2l.pmm",
               "smoking_30" = "2l.pmm"
               )

meth[keys(methods)] = values(methods)

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
     "nmoves" = 1,
     "imp_living_any_parent" = 1,
     "imp_parent_employed" = 1,
     "imp_parent_married" = 1,
     "parent_education" = 1,
     "mother_age_at_birth" = 1,
     "residential_moves_by_12" = 1,
     "asvab_score" = 1,
     "rev_health" = 1,
     "bmi" = 1,
     "depression" = 1,
     "smoking_ever" = 1,
     "smoking_30" = 0
    )

# assign predictors
predictors_vectors = names(meth[meth != ""])
for (i in seq_along(predictors_vectors)) {
    pred[predictors_vectors[i], keys(predictors)] = values(predictors)
}

# set diagonal of matrix to 0
diag(pred) = 0

# variables without age as predictor
vars = c("parent_education", "mother_age_at_birth", "residential_moves_by_12", "asvab_score")
pred[vars, "age_interview_est"] = 0

# adjustments
predictors['smoking_ever'] = 0
predictors['smoking_30'] = 0
pred["smoking_ever", "smoking_30"] = 0
pred["smoking_30", "smoking_ever"] = 0

# some checks
pred["rev_health",]
pred["smoking_30",]
pred["bmi",]
pred["z_relative_mob",]
pred["log_population",]
pred["asvab_score",]


# run imputation
number_cores = 10
imputations_per_core = 10

imp_relative_mob = parlmice(mm,
                            predictorMatrix = pred,
                            method = meth,
                            n.core = number_cores,
                            n.imp.core = imputations_per_core,
                            maxit = 20)

# explore quality of imputations

savepdf("ch03/output/nlsy97_relative_mob_imp_iterations")
print(plot(imp_relative_mob, c("bmi", "rev_health")))
print(plot(imp_relative_mob, c("depression", "smoking_30", "smoking_ever")))
print(plot(imp_relative_mob, c("hhsize", "log_income_adj")))
print(plot(imp_relative_mob, c("imp_living_any_parent", "imp_parent_married", "imp_parent_employed")))
print(plot(imp_relative_mob, c("parent_education", "mother_age_at_birth")))
print(plot(imp_relative_mob, c("asvab_score", "residential_moves_by_12")))
dev.off()

savepdf("ch03/output/nlsy97_relative_mob_imp_values")
print(densityplot(imp_relative_mob, ~ bmi + depression + smoking_30 + smoking_ever))
print(densityplot(imp_relative_mob, ~ rev_health))
print(densityplot(imp_relative_mob, ~ hhsize + log_income_adj))
print(densityplot(imp_relative_mob, ~ imp_living_any_parent + imp_parent_married + imp_parent_employed))
# print(densityplot(imp_relative_mob, ~ z_relative_mob + z_gini))
# print(densityplot(imp_relative_mob, ~ log_population + log_county_income))
print(densityplot(imp_relative_mob, ~ parent_education + mother_age_at_birth + asvab_score + residential_moves_by_12))
dev.off()

# save results of imputation
saveRDS(imp_relative_mob, "ch03/output/data/nlsy97_relative_mob_imputation.rds")

# # imputation using absolute mobility

# # predictors
# predictors = hash(
#      "id" = -2,
#      "male" = 1,
#      "ethnicity" = 1,
#      "age_interview_est" = 1,
#      "z_relative_mob" = 0,
#      "z_absolute_mob" = 1,
#      "z_gini" = 1,
#      "relative_mob_resid" = 0,
#      "absolute_mob_resid" = 0,
#      "gini_resid" = 0,
#      "q_relative_mob" = 0,
#      "q_absolute_mob" = 0,
#      "q_gini" = 0,
#      "q_relative_mob_resid" = 0,
#      "q_absolute_mob_resid" = 0,
#      "q_gini_resid" = 0,
#      "log_population" = 1,
#      "log_county_income" = 1,
#      "log_income_adj" = 1,
#      "hhsize" = 1,
#      "nmoves" = 1,
#      "imp_living_any_parent" = 1,
#      "imp_parent_employed" = 1,
#      "imp_parent_married" = 1,
#      "parent_education" = 1,
#      "mother_age_at_birth" = 1,
#      "residential_moves_by_12" = 1,
#      "asvab_score" = 1,
#      "rev_health" = 1,
#      "bmi" = 1,
#      "depression" = 1,
#      "smoking_ever" = 1,
#      "smoking_30" = 0
#     )

# # assign predictors
# predictors_vectors = names(meth[meth != ""])
# for (i in seq_along(predictors_vectors)) {
#     pred[predictors_vectors[i], keys(predictors)] = values(predictors)
# }

# # set diagonal of matrix to 0
# diag(pred) = 0

# # variables without age as predictor
# vars = c("parent_education", "mother_age_at_birth", "residential_moves_by_12", "asvab_score")
# pred[vars, "age_interview_est"] = 0

# # adjustments
# predictors['smoking_ever'] = 0
# predictors['smoking_30'] = 0
# pred["smoking_ever", "smoking_30"] = 0
# pred["smoking_30", "smoking_ever"] = 0

# # somo checks
# pred["rev_health",]
# pred["smoking_30",]
# pred["bmi",]
# pred["z_absolute_mob",]
# pred["log_population",]
# pred["asvab_score",]

# # run imputation
# imp_absolute_mob = parlmice(mm,
#                             predictorMatrix = pred,
#                             method = meth,
#                             n.core = 10,
#                             n.imp.core = 10,
#                             maxit = 20)


# # explore quality of imputations

# savepdf("ch03/output/nlsy97_absolute_mob_imp_iterations")
# print(plot(imp_absolute_mob, c("bmi", "rev_health")))
# print(plot(imp_absolute_mob, c("depression", "smoking_30", "smoking_ever")))
# # print(plot(imp_absolute_mob, c("z_relative_mob", "z_gini")))
# print(plot(imp_absolute_mob, c("hhsize", "log_income_adj")))
# print(plot(imp_absolute_mob, c("imp_living_any_parent", "imp_parent_married", "imp_parent_employed")))
# print(plot(imp_absolute_mob, c("parent_education", "mother_age_at_birth")))
# print(plot(imp_absolute_mob, c("asvab_score", "residential_moves_by_12")))
# dev.off()

# savepdf("ch03/output/nlsy97_absolute_mob_imp_values")
# print(densityplot(imp_absolute_mob, ~ bmi + depression + smoking_30 + smoking_ever))
# print(densityplot(imp_absolute_mob, ~ rev_health))
# print(densityplot(imp_absolute_mob, ~ hhsize + log_income_adj))
# print(densityplot(imp_absolute_mob, ~ imp_living_any_parent + imp_parent_married + imp_parent_employed))
# # print(densityplot(imp_absolute_mob, ~ z_relative_mob + z_gini))
# # print(densityplot(imp_absolute_mob, ~ log_population + log_county_income))
# print(densityplot(imp_absolute_mob, ~ parent_education + mother_age_at_birth + asvab_score + residential_moves_by_12))
# dev.off()

# # save results of imputation
# saveRDS(imp_relative_mob, "ch03/output/data/nlsy97_absolute_mob_imputation.rds")


# # relative mobility categorical variable

# mm[, q_relative_mob := factor(q_relative_mob)]
# mm[, q_gini := factor(q_gini)]

# table(mm$q_relative_mob)

# # predictors
# predictors = hash(
#      "id" = -2,
#      "male" = 1,
#      "ethnicity" = 1,
#      "age_interview_est" = 1,
#      "z_relative_mob" = 0,
#      "z_absolute_mob" = 0,
#      "z_gini" = 0,
#      "relative_mob_resid" = 0,
#      "absolute_mob_resid" = 0,
#      "gini_resid" = 0,
#      "q_relative_mob" = 1,
#      "q_absolute_mob" = 0,
#      "q_gini" = 1,
#      "q_relative_mob_resid" = 0,
#      "q_absolute_mob_resid" = 0,
#      "q_gini_resid" = 0,
#      "log_population" = 1,
#      "log_county_income" = 1,
#      "log_income_adj" = 1,
#      "hhsize" = 1,
#      "imp_living_any_parent" = 1,
#      "imp_parent_employed" = 1,
#      "imp_parent_married" = 1,
#      "parent_education" = 1,
#      "mother_age_at_birth" = 1,
#      "residential_moves_by_12" = 1,
#      "asvab_score" = 1,
#      "rev_health" = 1,
#      "bmi" = 1,
#      "depression" = 1,
#      "smoking_ever" = 1,
#      "smoking_30" = 0
#     )

# # assign predictors
# predictors_vectors = names(meth[meth != ""])
# for (i in seq_along(predictors_vectors)) {
#     pred[predictors_vectors[i], keys(predictors)] = values(predictors)
# }

# # set diagonal of matrix to 0
# diag(pred) = 0

# # variables without age as predictor
# vars = c("parent_education", "mother_age_at_birth", "residential_moves_by_12", "asvab_score")
# pred[vars, "age_interview_est"] = 0

# # adjustments
# predictors['smoking_ever'] = 0
# predictors['smoking_30'] = 0
# pred["smoking_ever", "smoking_30"] = 0
# pred["smoking_30", "smoking_ever"] = 0

# # somo checks
# pred["rev_health",]
# pred["smoking_30",]
# pred["bmi",]
# pred["q_relative_mob",]
# pred["log_population",]
# pred["asvab_score",]

# # run imputation
# imp_qrelative_mob = parlmice(mm,
#                              predictorMatrix = pred,
#                              method = meth,
#                              n.core = 2,
#                              n.imp.core = 2,
#                              maxit = 10)


# # explore quality of imputations

# savepdf("ch03/output/nlsy97_qrelative_mob_imp_iterations")
# print(plot(imp_absolute_mob, c("bmi", "rev_health")))
# print(plot(imp_absolute_mob, c("depression", "smoking_30", "smoking_ever")))
# # print(plot(imp_absolute_mob, c("z_relative_mob", "z_gini")))
# print(plot(imp_absolute_mob, c("hhsize", "log_income_adj")))
# print(plot(imp_absolute_mob, c("imp_living_any_parent", "imp_parent_married", "imp_parent_employed")))
# print(plot(imp_absolute_mob, c("parent_education", "mother_age_at_birth")))
# print(plot(imp_absolute_mob, c("asvab_score", "residential_moves_by_12")))
# dev.off()

# savepdf("ch03/output/nlsy97_qrelative_mob_imp_values")
# print(densityplot(imp_absolute_mob, ~ bmi + depression + smoking_30 + smoking_ever))
# print(densityplot(imp_absolute_mob, ~ rev_health))
# print(densityplot(imp_absolute_mob, ~ hhsize + log_income_adj))
# print(densityplot(imp_absolute_mob, ~ imp_living_any_parent + imp_parent_married + imp_parent_employed))
# # print(densityplot(imp_absolute_mob, ~ z_relative_mob + z_gini))
# # print(densityplot(imp_absolute_mob, ~ log_population + log_county_income))
# print(densityplot(imp_absolute_mob, ~ parent_education + mother_age_at_birth + asvab_score + residential_moves_by_12))
# dev.off()

# # save results of imputation
# saveRDS(imp_relative_mob, "ch03/output/data/nlsy97_absolute_mob_imputation.rds")




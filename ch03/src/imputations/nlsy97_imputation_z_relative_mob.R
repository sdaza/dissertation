##############################
# county income mobility and individual health
# imputation NLSY97 data
# author: sebastian daza
##############################


# random seed to reproduce imputations
seed = 144305

# exposure variables to be imputed
all_exposure_vars = c("z_relative_mob", "z_absolute_mob", "z_gini",
                      "relative_mob_resid", "absolute_mob_resid",
                      "gini_resid", "q_relative_mob_resid",
                      "q_absolute_mob_resid", "q_gini_resid",
                      "q_relative_mob", "q_absolute_mob", "q_gini",
                      "log_population", "log_county_income", "z_prop_black"
                      )

exposure = c("z_relative_mob", "z_gini")

# create mice object
ini = mice(mm, maxit = 0)
head(ini$loggedEvent)
pred = ini$pred
meth = ini$meth
pred[,] = 0
meth
# fluxplot(mm)
# fx = fluxplot(mm)

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
               "z_prop_black" = "",
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
               "smoking" = "2l.pmm",
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
     "z_prop_black" = 1,
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
     "smoking" = 1,
     "smoking_30" = 0
    )

# assign predictors
predictors_vectors = names(meth[meth != ""])
for (i in seq_along(predictors_vectors)) {
    pred[predictors_vectors[i], keys(predictors)] = values(predictors)
}

# set diagonal of matrix to 0
diag(pred) = 0

# time invariant variables without age as predictor
vars = c("parent_education", "mother_age_at_birth",
         "residential_moves_by_12", "asvab_score")
pred[vars, "age_interview_est"] = 0

# don't predict smoking variables with each other
pred["smoking_30", "smoking"] = 0
pred["smoking_30", "smoking"]

# check variables being imputed are the right ones
vars = setdiff(all_exposure_vars, exposure)
if (sum(pred[vars, vars] != 0)) { stop("Exposure variables imputed are not right!")}
if (any(pred[predictors_vectors, exposure] == 0)) {
    stop("Some variables do not have exposure variables as predictors") }

pred["rev_health",]
pred["smoking_30",]
sum(pred["bmi",])
pred["z_relative_mob",]
sum(pred["log_population",])
pred["asvab_score",]

# relative mobility imputation
number_cores = 10
imputations_per_core = 10

imp = parlmice(
    mm,
    predictorMatrix = pred,
    method = meth,
    n.core = number_cores,
    n.imp.core = imputations_per_core,
    maxit = 20,
    cluster.seed = seed
)

# save results of imputation
saveRDS(imp, "ch03/output/data/nlsy97_z_relative_mob_imputation.rds")

# explore quality of imputations
savepdf("ch03/output/figures/nlsy97_z_relative_mob_imp_iterations")
print(plot(imp, c("bmi", "rev_health")))
print(plot(imp, c("depression", "smoking_30", "smoking")))
print(plot(imp, c("hhsize", "log_income_adj")))
print(plot(imp, c("imp_living_any_parent", "imp_parent_married", "imp_parent_employed")))
print(plot(imp, c("parent_education", "mother_age_at_birth")))
print(plot(imp, c("asvab_score", "residential_moves_by_12")))
dev.off()

savepdf("ch03/output/figures/nlsy97_z_relative_mob_imp_values")
print(densityplot(imp, ~ bmi + depression + smoking_30 + smoking))
print(densityplot(imp, ~ rev_health))
print(densityplot(imp, ~ hhsize + log_income_adj))
print(densityplot(imp, ~ imp_living_any_parent + imp_parent_married + imp_parent_employed))
# print(densityplot(imp, ~ z_relative_mob + z_gini))
# print(densityplot(imp, ~ log_population + log_county_income))
print(densityplot(imp, ~ parent_education + mother_age_at_birth + asvab_score + residential_moves_by_12))
dev.off()




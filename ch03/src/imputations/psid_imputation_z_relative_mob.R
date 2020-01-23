##############################
# county income mobility and individual health
# imputation psid data
# author: sebastian daza
##############################

# create mice object
ini = mice(mm, maxit = 0)
head(ini$loggedEvent)
pred = ini$pred
meth = ini$meth
pred[,] = 0

tt = fluxplot(mm)
# countmis(mm[head_wife == 1])

# length(unique(mm[relation_head %in% c(1, 10, 2, 20, 22), pid]))
# explore some respondents
# ids = unique(mm[head_wife == 1, pid])
# mm[pid == sample(ids, 1), .(pid, year, imp_age, relation_head, whynoresp, depression)]

methods = hash(
    "year_born" = "",
    "smoking_ever" = "",
    "individual_working_binary" = "",
    # time invariant covariates
    "race" = "2lonly.function",
    "weight_less_55" = "2lonly.function",
    "mother_marital_status" = "2lonly.function",
    "mother_age" = "2lonly.pmm",
    # time variant covariates
    "log_income_adj" = "2l.pmm",
    "head_marital_status" = "2l.pmm",
    "head_education" = "2l.pmm",
    "head_owns_house" = "2l.pmm",
    "head_working_binary" = "2l.pmm",
    "famsize" = "2l.pmm",
    # outcomes
    "life_satisfaction" = "",
    "depression" = "2l.pmm",
    "bmi" = "2l.pmm",
    "smoking" = "2l.pmm",
    "smoking_number" = "2l.pmm",
    "health_binary" = "",
    "individual_health" = "2l.pmm"
)

meth[keys(methods)] = values(methods)

imputationFunction = list("race" = "polyreg",
                          "weight_less_55" = "logreg",
                          "mother_marital_status" = "logreg"
                          )

cluster_var =  list("race" = "pid",
                    "weight_less_55" = "pid",
                    "mother_marital_status" = "pid"
                    )

predictors = hash(
    # time-invariante coviarates
    "pid" = -2,
    "male" = 1,
    "race" = 1,
    "imp_age" = 1,
    "first_year" = 1,
    "weight_less_55" = 1,
    "mother_marital_status" = 1,
    "mother_age" = 1,
    # time-varying covariates
    "log_income_adj" = 1,
    "head_marital_status" = 1,
    "head_education" = 1,
    "head_owns_house" = 1,
    "head_working_binary" = 1,
    "famsize" = 1,
    # outcomes
    "depression" = 1,
    "bmi" = 1,
    "life_satisfaction" = 0,
    "smoking" = 1,
    "smoking_number" = 0,
    "health_binary" = 0,
    "individual_health" = 0
)

# assign predictors
predictors_vectors = names(meth[meth != ""])
for (i in seq_along(predictors_vectors)) {
    pred[predictors_vectors[i], keys(predictors)] = values(predictors)
}

# set diagonal of matrix to 0
diag(pred) = 0

# explore
pred["log_income_adj",]
pred["age_mother",]
pred["mother_marital_status",]
pred["weight_less_55",]
pred["individual_health",]
pred["health_binary",]
pred["race",]
pred["depression", ]
pred["life_satisfaction", ]

# adjustments
# pred[c("depression", "bmi", "health_binary", "smoking"), c("life_satisfaction")] = 0
# pred["life_satisfaction", c("bmi", "health_binary", "smoking")] = 0

# imputation
number_cores = 2
imputations_per_core = 2

imp = parlmice(
    mm,
    predictorMatrix = pred,
    method = meth,
    n.core = number_cores,
    n.imp.core = imputations_per_core,
    maxit = 20,
    imputationFunction = imputationFunction,
    cluster_var = cluster_var
)

# explore imputations

savepdf("ch03/output/psid_relative_mob_iterations")
print(plot(imp, c("bmi", "depression", "individual_health")))
print(plot(imp, c("smoking", "smoking_number")))
print(plot(imp, c("log_income_adj", "age_mother")))
print(plot(imp, c("marital_status_mother", "weight_less_55", "famsize")))
print(plot(imp, c("head_marital_status", "head_education", "head_owns_house")))
print(plot(imp, c("head_working_binary", "head_education", "head_owns_house")))
dev.off()

savepdf("ch03/output/psid_relative_mob_imp_values")
print(densityplot(imp, ~ bmi + depression))
print(densityplot(imp, ~ individual_health + smoking_number + smoking))
print(densityplot(imp, ~ log_income_adj))
print(densityplot(imp, ~ age_mother))
print(densityplot(imp, ~ marital_status_mother + weight_less_55))
print(densityplot(imp, ~ head_education + head_owns_house))
print(densityplot(imp, ~ head_working_binary + famsize))
dev.off()


# save results of imputation
saveRDS(imp, "ch03/output/data/psid_relative_mob_imputation.rds")


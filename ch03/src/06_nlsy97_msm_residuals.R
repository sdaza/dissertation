

# comment out residuals
# for (i in seq_along(outcomes)) {
#     print(paste0("Running model ", i))
#     unadjusted_relative_mob_resid_results[[i]] = unadjustedRegression(
#         imputations = imp_relative_mob_resid,
#         exposure_variable = "relative_mob_resid",
#         exposure_type = "gaussian",
#         id_var = "id",
#         time_var = "stime",
#         max_time_exposure = 8,
#         outcome = outcomes[i],
#         final_model_type = model_type[i]
#     )
# }

# for (i in seq_along(outcomes)) {
#     print(paste0("Running model ", i))
#     unadjusted_absolute_mob_resid_results[[i]] = unadjustedRegression(
#         imputations = imp_absolute_mob_resid,
#         exposure_variable = "absolute_mob_resid",
#         exposure_type = "gaussian",
#         id_var = "id",
#         time_var = "stime",
#         max_time_exposure = 8,
#         outcome = outcomes[i],
#         final_model_type = model_type[i]
#     )
# }

# for (i in seq_along(outcomes)) {
#     print(paste0("Running model ", i))
#     unadjusted_gini_resid_results[[i]] = unadjustedRegression(
#         imputations = imp_relative_mob_resid,
#         exposure_variable = "gini_resid",
#         exposure_type = "gaussian",
#         id_var = "id",
#         time_var = "stime",
#         max_time_exposure = 8,
#         outcome = outcomes[i],
#         final_model_type = model_type[i]
#     )
# }


# relative mobility residuals

denominator_time1 = longText("
    male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12
")

numerator = longText("
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj + baseline_hhsize +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_rev_health + baseline_smoking_30 + baseline_bmi +
    baseline_relative_mob_resid + baseline_gini_resid +
    baseline_rev_health + baseline_smoking_30 + lag_relative_mob_resid) + as.factor(stime)
")

denominator = longText("
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj + baseline_hhsize +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_rev_health + baseline_smoking_30 + baseline_bmi +
    baseline_relative_mob_resid + baseline_gini_resid +
    nmoves + lag_imp_parent_employed + lag_imp_parent_married + lag_hhsize +
    log_income_adj + lag_rev_health + lag_bmi + lag_smoking_30 +
    lag_relative_mob_resid) + as.factor(stime)
")

predictors = longText(
    "average_relative_mob_resid + male + ethnicity +
     as.factor(max_age_interview_est) +
     parent_education + asvab_score + mother_age_at_birth +
     residential_moves_by_12 +
     baseline_log_income_adj + baseline_hhsize +
     baseline_imp_parent_employed + baseline_imp_parent_married +
     baseline_gini_resid
")


relative_mob_resid_continuous_results = list()

for (i in seq_along(outcomes)) {

    print(paste0("Running model ", i))

    final_model = formula(paste0(outcomes[i], " ~ ", predictors))
    relative_mob_resid_continuous_results[[outcomes[i]]] = ipwExposure(
        imputations = imp_relative_mob_resid,
        lag_variables = lag_vars,
        baseline_variables = baseline_vars,
        denominator_time1 = denominator_time1,
        numerator = numerator,
        denominator = denominator,
        exposure_variable = "relative_mob_resid",
        id_var = "id",
        time_var = "stime",
        max_time_exposure = 8,
        final_model =final_model,
        trim_p = 0.01,
        exposure_type = "gaussian",
        final_model_type = model_type[i]
    )
}

saveRDS(relative_mob_resid_continuous_results,
        file = "ch03/output/data/nlsy97_results_relative_mob_resid_continuous.rds")




# absolute mobility residuals

denominator_time1 = longText("
    male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12
")

numerator = longText("
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj + baseline_hhsize +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_rev_health + baseline_smoking_30 + baseline_bmi +
    baseline_absolute_mob_resid + baseline_gini_resid +
    baseline_rev_health + baseline_smoking_30 + lag_absolute_mob_resid) + as.factor(stime)
")

denominator = longText("
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj + baseline_hhsize +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_rev_health + baseline_smoking_30 + baseline_bmi +
    baseline_absolute_mob_resid + baseline_gini_resid +
    nmoves + lag_imp_parent_employed + lag_imp_parent_married + lag_hhsize +
    log_income_adj + lag_rev_health + lag_bmi + lag_smoking_30 +
    lag_absolute_mob_resid) + as.factor(stime)
")

predictors = longText(
    "average_absolute_mob_resid + male + ethnicity +
     as.factor(max_age_interview_est) +
     parent_education + asvab_score + mother_age_at_birth +
     residential_moves_by_12 +
     baseline_log_income_adj + baseline_hhsize +
     baseline_imp_parent_employed + baseline_imp_parent_married +
     baseline_gini_resid
")

absolute_mob_resid_continuous_results = list()

for (i in seq_along(outcomes)) {

    print(paste0("Running model ", i))

    final_model = formula(paste0(outcomes[i], " ~ ", predictors))

    print(final_model)

    absolute_mob_resid_continuous_results[[outcomes[i]]] = ipwExposure(
        imputations = imp_absolute_mob_resid,
        lag_variables = lag_vars,
        baseline_variables = baseline_vars,
        denominator_time1 = denominator_time1,
        numerator = numerator,
        denominator = denominator,
        exposure_variable = "absolute_mob_resid",
        id_var = "id",
        time_var = "stime",
        max_time_exposure = 8,
        final_model = final_model,
        trim_p = 0.01,
        exposure_type = "gaussian",
        final_model_type = model_type[i]
    )
}

saveRDS(absolute_mob_resid_continuous_results,
        file = "ch03/output/data/nlsy97_results_absolute_mob_resid_continuous.rds")




# gini residual

denominator_time1 = longText("
    male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12
")

numerator = longText("
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj + baseline_hhsize +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_rev_health + baseline_smoking_30 + baseline_bmi +
    baseline_relative_mob_resid +
    baseline_rev_health + baseline_smoking_30) + as.factor(stime) + lag_gini_resid
")

denominator = longText("
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj + baseline_hhsize +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_rev_health + baseline_smoking_30 + baseline_bmi +
    baseline_relative_mob_resid +
    nmoves + lag_imp_parent_employed + lag_imp_parent_married + lag_hhsize +
    log_income_adj + lag_rev_health + lag_bmi + lag_smoking_30) +
    as.factor(stime) + lag_gini_resid
")

outcomes = c("rev_health", "bmi", "depression", "smoking", "smoking_30")
model_type = c("ordinal", "gaussian", "gaussian", "binomial", "poisson")

predictors = longText(
    "average_gini_resid + male + ethnicity +
     as.factor(max_age_interview_est) +
     parent_education + asvab_score + mother_age_at_birth +
     residential_moves_by_12 +
     baseline_log_income_adj + baseline_hhsize +
     baseline_imp_parent_employed + baseline_imp_parent_married +
     baseline_relative_mob_resid
")

gini_resid_continuous_results = list()

for (i in seq_along(outcomes)) {

    print(paste0("Running model ", i))
    final_model = formula(paste0(outcomes[i], " ~ ", gsub("\n", "", predictors)))
    gini_resid_continuous_results[[outcomes[i]]] = ipwExposure(
        imputations = imp_relative_mob_resid,
        lag_variables = lag_vars,
        baseline_variables = baseline_vars,
        denominator_time1 = denominator_time1,
        numerator = numerator,
        denominator = denominator,
        exposure_variable = "gini_resid",
        id_var = "id",
        time_var = "stime",
        max_time_exposure = 8,
        final_model = final_model,
        trim_p = 0.01,
        exposure_type = "gaussian",
        final_model_type = model_type[i]
    )
}

saveRDS(gini_resid_continuous_results,
        file = "ch03/output/data/nlsy97_results_gini_resid_continuous.rds")


z_relative_mob_results = readRDS("ch03/output/data/nlsy97_results_z_relative_mob.rds")
relative_mob_resid_continuous_results = readRDS("ch03/output/data/nlsy97_results_relative_mob_resid_continuous.rds")
absolute_mob_continuous_results = readRDS("ch03/output/data/nlsy97_results_absolute_mob_continuous.rds")
absolute_mob_resid_continuous_results = readRDS("ch03/output/data/nlsy97_results_absolute_mob_resid_continuous.rds")
gini_continuous_results = readRDS("ch03/output/data/nlsy97_results_gini_continuous.rds")
gini_resid_continuous_results = readRDS("ch03/output/data/nlsy97_results_gini_resid_continuous.rds")




relative_mob_resid_continuous_models = list()
for (i in seq_along(relative_mob_resid_continuous_results)) {
    relative_mob_resid_continuous_models[[i]] = relative_mob_resid_continuous_results[[i]][["models"]]
}

absolute_mob_resid_continuous_models = list()
for (i in seq_along(absolute_mob_resid_continuous_results)) {
    absolute_mob_resid_continuous_models[[i]] = absolute_mob_resid_continuous_results[[i]][["models"]]
}

gini_resid_models = list()
for (i in seq_along(gini_resid_continuous_results)) {
    gini_resid_models[[i]] = gini_resid_continuous_results[[i]][["models"]]
}
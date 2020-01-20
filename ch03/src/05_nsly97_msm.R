##############################
# MSM with NLSY97 data
# author: sebastian daza
# version: 0.01
##############################


library(survey)
library(data.table)
library(texreg)
library(mitools)
library(MASS)
source("ch03/src/utils.R")


# read imputed data
imp = readRDS('ch03/output/data/nlsy97_relative_mob_imputation.rds')

# number of observations
N = length(unique(mice::complete(imp, 1)$id))

# lagged and baseline variables
lag_vars = c("imp_parent_employed", "imp_parent_married", "hhsize",
             "bmi", "rev_health", "smoking_ever", "smoking_30", "log_population",
             "log_county_income", "z_relative_mob", "z_gini",
             "q_relative_mob", "q_gini")

baseline_vars = c("z_relative_mob", "z_gini", "q_relative_mob", "q_gini",
                  "imp_parent_employed",
                  "imp_parent_married", "hhsize", "bmi", "rev_health",
                  "smoking_ever", "smoking_30", "log_population",
                  "log_county_income", "log_income_adj")

outcomes = c("rev_health", "bmi", "depression", "smoking_ever", "smoking_30")
model_type = c("ordinal", "gaussian", "gaussian", "binomial", "poisson")

# continous version analysis

# relative mobility

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
    baseline_z_relative_mob + baseline_z_gini +
    baseline_rev_health + baseline_smoking_30 + lag_z_relative_mob) + as.factor(stime)
")

denominator = longText("
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj + baseline_hhsize +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_rev_health + baseline_smoking_30 + baseline_bmi +
    baseline_z_relative_mob + baseline_z_gini +
    nmoves + lag_imp_parent_employed + lag_imp_parent_married + lag_hhsize +
    log_income_adj + lag_rev_health + lag_bmi + lag_smoking_30 +
    lag_z_relative_mob + lag_z_gini) + as.factor(stime)
")

predictors = longText(
    "average_z_relative_mob + male + ethnicity +
     as.factor(max_age_interview_est) +
     parent_education + asvab_score + mother_age_at_birth +
     residential_moves_by_12 +
     baseline_log_income_adj + baseline_hhsize +
     baseline_imp_parent_employed + baseline_imp_parent_married +
     baseline_z_gini
")

relative_mob_continous_results = list()

for (i in seq_along(outcomes)) {

    print(paste0("Running model ", i))

    final_model = formula(paste0(outcomes[i], " ~ ", predictors))
    relative_mob_continous_results[[outcomes[i]]] = ipwExposure(
        imputations = imp,
        lag_variables = lag_vars,
        baseline_variables = baseline_vars,
        denominator_time1 = denominator_time1,
        numerator = numerator,
        denominator = denominator,
        exposure_variable = "z_relative_mob",
        id_var = "id",
        time_var = "stime",
        max_time_exposure = 8,
        final_model =final_model,
        trim_p = 0.01,
        exposure_type = "gaussian",
        final_model_type = model_type[i]
    )
}

saveRDS(relative_mob_continous_results,
        file = "ch03/output/relative_mob_continous_results.rd")

# gini

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
    baseline_z_relative_mob +
    baseline_rev_health + baseline_smoking_30) + as.factor(stime) + lag_z_gini
")

denominator = longText("
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj + baseline_hhsize +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_rev_health + baseline_smoking_30 + baseline_bmi +
    baseline_z_relative_mob +
    nmoves + lag_imp_parent_employed + lag_imp_parent_married + lag_hhsize +
    log_income_adj + lag_rev_health + lag_bmi + lag_smoking_30 +
    lag_z_relative_mob) + as.factor(stime) + lag_z_gini
")

outcomes = c("rev_health", "bmi", "depression", "smoking_ever", "smoking_30")
model_type = c("ordinal", "gaussian", "gaussian", "binomial", "poisson")

predictors = longText(
    "average_z_gini + male + ethnicity +
     as.factor(max_age_interview_est) +
     parent_education + asvab_score + mother_age_at_birth +
     residential_moves_by_12 +
     baseline_log_income_adj + baseline_hhsize +
     baseline_imp_parent_employed + baseline_imp_parent_married +
     baseline_z_relative_mob
")

gini_continous_results = list()

for (i in seq_along(outcomes)) {

    print(paste0("Running model ", i))
    final_model = formula(paste0(outcomes[i], " ~ ", gsub("\n", "", predictors)))
    gini_continous_results[[outcomes[i]]] = ipwExposure(
        imputations = imp,
        lag_variables = lag_vars,
        baseline_variables = baseline_vars,
        denominator_time1 = denominator_time1,
        numerator = numerator,
        denominator = denominator,
        exposure_variable = "z_gini",
        id_var = "id",
        time_var = "stime",
        max_time_exposure = 8,
        final_model = final_model,
        trim_p = 0.01,
        exposure_type = "gaussian",
        final_model_type = model_type[i]
    )
}

saveRDS(gini_continous_results,
        file = "ch03/output/gini_continous_results.rd")

# list of results

# extract models

# relative_mob_continous_models = list()
# for (i in seq_along(relative_mob_continous_results)) {
#     relative_mob_continous_models[[i]] = relative_mob_continous_results[[i]][["models"]]
# }

# absolute_mob_continous_models = list()
# for (i in seq_along(absolute_mob_continous_results)) {
#     absolute_mob_continous_models[[i]] = absolute_mob_continous_results[[i]][["models"]]
# }

# list_rows = list(relative_mob_continous_results, gini_continous_results)

# row_names = c("average_z_relative_mob", "average_z_gini")
# row_labels = c("Income relative mobility average exposure",
#                "Inequality (Gini) average exposure")

# column_names = c("Health status", "BMI", "Depressive symptoms", "Smoking", "Days smoking last month")

# comment = longText("Each coefficient represents a model. Coefficients and standard errors are combined estimates from 100 multiple imputed datasets.
#                    Analyses based on exposure from 12 to 20 years old.
#                    We estimate different models depending on the outcome:
#                    Ordinal regression (self-reported health), Generalized linear model (BMI, depression),
#                    Logistic regression (smoking), Poisson regression (number of days smoking last month)
#                    Two-side tests of no effect: $^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$"
# )

# # groups = list("Income mobility" = 1, "Gini" = 3:4)

# # create tables

# list_weights = list(
#     relative_mob_continous_results[[1]][["weights"]],
#     absolute_mob_continous_results[[1]][["weights"]]
# )

# tableWeights(list_weights,
#              model_names = c("test1", "test2"),
#              caption  = "Test",
#              label = "tab:test",
#              comment = "This is a test",
#              filename = "ch03/output/weight_test.tex",
#              tabcolsep = 10,
#              arraystretch = 1)


# createModelTables(list_rows, column_names = column_names,
#                   row_names = row_names, row_labels = row_labels,
#                   filename = "ch03/output/test_table.tex",
#                   comment = comment,
#                   groups = groups
#                   )



##############################
# county income mobility and individual health
# MSM with NLSY97 data
# author: sebastian daza
##############################


library(survey)
library(data.table)
library(texreg)
library(mitools)
library(MASS)
source("ch03/src/utils.R")


# read imputed data
imp_relative_mob = readRDS('ch03/output/data/nlsy97_relative_mob_imputation.rds')
imp_absolute_mob = readRDS('ch03/output/data/nlsy97_absolute_mob_imputation.rds')
imp_relative_mob_resid = readRDS('ch03/output/data/nlsy97_relative_mob_resid_imputation.rds')
imp_absolute_mob_resid = readRDS('ch03/output/data/nlsy97_absolute_mob_resid_imputation.rds')

# number of observations
N = length(unique(mice::complete(imp_relative_mob, 1)$id))

outcomes = c("rev_health", "bmi", "depression", "smoking_ever", "smoking_30")
model_type = c("ordinal", "gaussian", "gaussian", "binomial", "poisson")

# unadjusted models
unadjusted_relative_mob_continuous_results = list()
unadjusted_relative_mob_resid_continuous_results = list()
unadjusted_absolute_mob_continuous_results = list()
unadjusted_absolute_mob_resid_continuous_results = list()
unadjusted_gini_continuous_results = list()
unadjusted_gini_resid_continuous_results = list()


for (i in seq_along(outcomes)) {
    print(paste0("Running model ", i))
    unadjusted_relative_mob_continuous_results[[i]] = unadjustedRegression(
        imputations = imp_relative_mob,
        exposure_variable = "z_relative_mob",
        exposure_type = "gaussian",
        id_var = "id",
        time_var = "stime",
        max_time_exposure = 8,
        outcome = outcomes[i],
        final_model_type = model_type[i]
    )
}


for (i in seq_along(outcomes)) {
    print(paste0("Running model ", i))
    unadjusted_relative_mob_resid_continuous_results[[i]] = unadjustedRegression(
        imputations = imp_relative_mob_resid,
        exposure_variable = "relative_mob_resid",
        exposure_type = "gaussian",
        id_var = "id",
        time_var = "stime",
        max_time_exposure = 8,
        outcome = outcomes[i],
        final_model_type = model_type[i]
    )
}


for (i in seq_along(outcomes)) {
    print(paste0("Running model ", i))
    unadjusted_absolute_mob_continuous_results[[i]] = unadjustedRegression(
        imputations = imp_absolute_mob,
        exposure_variable = "z_absolute_mob",
        exposure_type = "gaussian",
        id_var = "id",
        time_var = "stime",
        max_time_exposure = 8,
        outcome = outcomes[i],
        final_model_type = model_type[i]
    )
}


for (i in seq_along(outcomes)) {
    print(paste0("Running model ", i))
    unadjusted_absolute_mob_resid_continuous_results[[i]] = unadjustedRegression(
        imputations = imp_absolute_mob_resid,
        exposure_variable = "absolute_mob_resid",
        exposure_type = "gaussian",
        id_var = "id",
        time_var = "stime",
        max_time_exposure = 8,
        outcome = outcomes[i],
        final_model_type = model_type[i]
    )
}


for (i in seq_along(outcomes)) {
    print(paste0("Running model ", i))
    unadjusted_gini_continuous_results[[i]] = unadjustedRegression(
        imputations = imp_relative_mob,
        exposure_variable = "z_gini",
        exposure_type = "gaussian",
        id_var = "id",
        time_var = "stime",
        max_time_exposure = 8,
        outcome = outcomes[i],
        final_model_type = model_type[i]
    )
}


for (i in seq_along(outcomes)) {
    print(paste0("Running model ", i))
    unadjusted_gini_resid_continuous_results[[i]] = unadjustedRegression(
        imputations = imp_relative_mob_resid,
        exposure_variable = "gini_resid",
        exposure_type = "gaussian",
        id_var = "id",
        time_var = "stime",
        max_time_exposure = 8,
        outcome = outcomes[i],
        final_model_type = model_type[i]
    )
}

saveRDS(list(
    unadjusted_relative_mob_continuous_results,
    unadjusted_relative_mob_resid_continuous_results,
    unadjusted_absolute_mob_continuous_results,
    unadjusted_absolute_mob_resid_continuous_results,
    unadjusted_gini_continuous_results,
    unadjusted_gini_resid_continuous_results),
    file = "ch03/output/data/nlsy97_unadjusted_models.rds")

# adjusted models

# lagged and baseline variables
lag_vars = c("imp_parent_employed", "imp_parent_married", "hhsize",
             "bmi", "rev_health", "smoking_ever", "smoking_30", "log_population",
             "log_county_income",
             "z_relative_mob", "z_gini", "z_absolute_mob",
             "relative_mob_resid", "absolute_mob_resid", "gini_resid",
             "q_relative_mob", "q_absolute_mob", "q_gini",
             "q_relative_mob_resid", "q_absolute_mob_resid", "q_gini_resid")

baseline_vars = c("z_relative_mob", "z_gini", "z_absolute_mob",
                  "relative_mob_resid", "absolute_mob_resid", "gini_resid",
                  "q_relative_mob", "q_absolute_mob", "q_gini",
                  "q_relative_mob_resid", "q_absolute_mob_resid", "q_gini_resid",
                  "imp_parent_employed",
                  "imp_parent_married", "hhsize", "bmi", "rev_health",
                  "smoking_ever", "smoking_30", "log_population",
                  "log_county_income", "log_income_adj")

# continuous version analysis

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


z_relative_mob_results = list()

for (i in seq_along(outcomes)) {

    print(paste0("Running model ", i))

    final_model = formula(paste0(outcomes[i], " ~ ", predictors))
    z_relative_mob_results[[outcomes[i]]] = ipwExposure(
        imputations = imp_relative_mob,
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

saveRDS(relative_mob_continuous_results,
        file = "ch03/output/data/nlsy97_results_z_relative_mob.rds")

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


# absolute mobility

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
    baseline_z_absolute_mob + baseline_z_gini +
    baseline_rev_health + baseline_smoking_30 + lag_z_absolute_mob) + as.factor(stime)
")

denominator = longText("
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj + baseline_hhsize +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_rev_health + baseline_smoking_30 + baseline_bmi +
    baseline_z_absolute_mob + baseline_z_gini +
    nmoves + lag_imp_parent_employed + lag_imp_parent_married + lag_hhsize +
    log_income_adj + lag_rev_health + lag_bmi + lag_smoking_30 +
    lag_z_absolute_mob + lag_z_gini) + as.factor(stime)
")

predictors = longText(
    "average_z_absolute_mob + male + ethnicity +
     as.factor(max_age_interview_est) +
     parent_education + asvab_score + mother_age_at_birth +
     residential_moves_by_12 +
     baseline_log_income_adj + baseline_hhsize +
     baseline_imp_parent_employed + baseline_imp_parent_married +
     baseline_z_gini
")

absolute_mob_continuous_results = list()

for (i in seq_along(outcomes)) {

    print(paste0("Running model ", i))

    final_model = formula(paste0(outcomes[i], " ~ ", predictors))

    print(final_model)

    absolute_mob_continuous_results[[outcomes[i]]] = ipwExposure(
        imputations = imp_absolute_mob,
        lag_variables = lag_vars,
        baseline_variables = baseline_vars,
        denominator_time1 = denominator_time1,
        numerator = numerator,
        denominator = denominator,
        exposure_variable = "z_absolute_mob",
        id_var = "id",
        time_var = "stime",
        max_time_exposure = 8,
        final_model = final_model,
        trim_p = 0.01,
        exposure_type = "gaussian",
        final_model_type = model_type[i]
    )
}

saveRDS(absolute_mob_continuous_results,
        file = "ch03/output/data/nlsy97_results_absolute_mob_continuous.rds")


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

gini_continuous_results = list()

for (i in seq_along(outcomes)) {

    print(paste0("Running model ", i))
    final_model = formula(paste0(outcomes[i], " ~ ", gsub("\n", "", predictors)))
    gini_continuous_results[[outcomes[i]]] = ipwExposure(
        imputations = imp_relative_mob,
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

saveRDS(gini_continuous_results,
        file = "ch03/output/data/nlsy97_results_gini_continuous.rds")


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

outcomes = c("rev_health", "bmi", "depression", "smoking_ever", "smoking_30")
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


# list of results

# load saved results
relative_mob_continuous_results = readRDS("ch03/output/data/nlsy97_results_relative_mob_continuous.rds")
relative_mob_resid_continuous_results = readRDS("ch03/output/data/nlsy97_results_relative_mob_resid_continuous.rds")
absolute_mob_continuous_results = readRDS("ch03/output/data/nlsy97_results_absolute_mob_continuous.rds")
absolute_mob_resid_continuous_results = readRDS("ch03/output/data/nlsy97_results_absolute_mob_resid_continuous.rds")
gini_continuous_results = readRDS("ch03/output/data/nlsy97_results_gini_continuous.rds")
gini_resid_continuous_results = readRDS("ch03/output/data/nlsy97_results_gini_resid_continuous.rds")

# extract models

relative_mob_continuous_models = list()
for (i in seq_along(relative_mob_continuous_results)) {
    relative_mob_continuous_models[[i]] = relative_mob_continuous_results[[i]][["models"]]
}

relative_mob_resid_continuous_models = list()
for (i in seq_along(relative_mob_resid_continuous_results)) {
    relative_mob_resid_continuous_models[[i]] = relative_mob_resid_continuous_results[[i]][["models"]]
}

absolute_mob_continuous_models = list()
for (i in seq_along(absolute_mob_continuous_results)) {
    absolute_mob_continuous_models[[i]] = absolute_mob_continuous_results[[i]][["models"]]
}

absolute_mob_resid_continuous_models = list()
for (i in seq_along(absolute_mob_resid_continuous_results)) {
    absolute_mob_resid_continuous_models[[i]] = absolute_mob_resid_continuous_results[[i]][["models"]]
}

gini_continuous_models = list()
for (i in seq_along(gini_continuous_results)) {
    gini_continuous_models[[i]] = gini_continuous_results[[i]][["models"]]
}

gini_resid_continuous_models = list()
for (i in seq_along(gini_resid_continuous_results)) {
    gini_resid_continuous_models[[i]] = gini_resid_continuous_results[[i]][["models"]]
}

list_rows = list(relative_mob_continuous_models,
                 relative_mob_resid_continuous_models,
                 absolute_mob_continuous_models,
                 absolute_mob_resid_continuous_models,
                 gini_continuous_models,
                 gini_resid_continuous_models)

row_names = c("average_z_relative_mob",
              "average_relative_mob_resid",
              "average_z_absolute_mob",
              "average_absolute_mob_resid",
              "average_z_gini",
              "average_gini_resid")

row_labels = c("Average rank-rank correlation",
               "Average rank-rank correlation residuals",
               "Average upward mobility",
               "Average upward mobility residuals",
               "Gini",
               "Gini residuals")

column_names = c("Health status", "BMI", "Depression", "Smoking", "Days smoking last month")
caption = "Effects of average exposure (continuous) on health indicators, NLSY97"
label = "tab:nlsy97_exposure_continuous_models"

comment = longText("Each coefficient represents a model. Coefficients and standard errors are combined estimates from 100 multiple imputed datasets.
                   Analyses based on exposure from 12 to 20 years old.
                   We estimate different models depending on the outcome:
                   Ordinal regression (self-reported health), Generalized linear model (BMI, depression),
                   Logistic regression (smoking), Poisson regression (number of days smoking last month).
                   Two-side tests of no effect: $^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$"
)

groups = list("Relative" = 1:2,
              "Absolute" = 3:4,
              "Inequality" = 5:6)

createModelTables(
    list_rows,
    caption = caption,
    label = label,
    column_names = column_names,
    row_names = row_names, row_labels = row_labels,
    filename = "ch03/output/nlsy97_exposure_continuous_models.tex",
    comment = comment,
    groups = groups,
    observations = N
                  )

# create table unadjusted models

list_rows = readRDS("ch03/output/data/nlsy97_unadjusted_models.rds")

row_names = c("average_z_relative_mob",
              "average_relative_mob_resid",
              "average_z_absolute_mob",
              "average_absolute_mob_resid",
              "average_z_gini",
              "average_gini_resid")

row_labels = c("Rank-rank correlation",
               "Rank-rank correlation residuals",
               "Upward mobility $\\times$ -1",
               "Upward mobility residuals $\\times$ -1",
               "Gini",
               "Gini residuals"
               )

column_names = c("Health status",
                 "BMI",
                 "Depression",
                 "Smoking",
                 "Days smoking last month")

comment = longText("Each coefficient represents a model. Coefficients and standard errors are combined estimates from 100 multiple imputed datasets.
                   Analyses based on exposure from 12 to 20 years old.
                   We estimate different models depending on the outcome:
                   Ordinal regression (self-reported health), Generalized linear model (BMI, depression),
                   Logistic regression (smoking), Poisson regression (number of days smoking last month).
                   Two-side tests of no effect: $^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$"
)

caption = "Unadjusted estimates of average exposure (continuous) on health indicators, NLSY97"
label = "tab:nlsy97_unadjusted_continuous_models"

groups = list("Relative" = 1:2,
              "Absolute" = 3:4,
              "Inequality" = 5:6)

createModelTables(
    list_rows,
    caption = caption,
    label = label,
    column_names = column_names,
    row_names = row_names,
    row_labels = row_labels,
    filename = "ch03/output/nlsy97_unadjusted_continuous_models.tex",
    comment = comment,
    groups = groups,
    observations = N
)


# create weight tables

list_weights = list(
    relative_mob_continuous_results[[1]][["weights"]],
    relative_mob_resid_continuous_results[[1]][["weights"]],
    absolute_mob_continuous_results[[1]][["weights"]],
    absolute_mob_resid_continuous_results[[1]][["weights"]],
    gini_continuous_results[[1]][["weights"]],
    gini_resid_continuous_results[[1]][["weights"]]
)

tableWeights(list_weights,
             model_names = c("Rank-rank correlation",
                             "Rank-rankc correlation residuals",
                             "Upward mobility",
                             "Upward mobility residuals",
                             "Gini",
                             "Gini residuals"),
             caption  = "Stabilized treatment weights, continuous exposure",
             label = "tab:ipt_weigths_continuous",
             comment = "Analyses based on exposure from 12 to 20 years old. ",
             filename = "ch03/output/nlsy97_ipt_weights_continuous.tex",
             tabcolsep = 10,
             arraystretch = 1)


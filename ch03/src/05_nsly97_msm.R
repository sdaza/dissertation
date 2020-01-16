# simulate IPTW construction

# library(geepack)
library(survey)
library(ipw)
library(data.table)
library(texreg)
library(mitools)
library(MASS)
source("ch03/src/utils.R")


# read imputed data
imp = readRDS('ch03/output/data/nlsy97_relative_mob_imputation.rds')

# test solution following each step
lag_vars = c("imp_parent_employed", "imp_parent_married", "hhsize",
             "bmi", "health", "smoking_ever", "smoking_30", "log_population",
             "log_county_income", "z_relative_mob", "z_gini",
             "q_relative_mob", "q_gini")

baseline_vars = c("z_relative_mob", "z_gini", "q_relative_mob", "q_gini",
                  "imp_parent_employed",
                  "imp_parent_married", "hhsize", "bmi", "health",
                  "smoking_ever", "smoking_30", "log_population",
                  "log_county_income", "log_income_adj")

# continous version

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
    baseline_health + baseline_smoking_30 + baseline_bmi +
    baseline_z_relative_mob + baseline_z_gini +
    baseline_health + baseline_smoking_30 + lag_z_relative_mob) * as.factor(stime)
")

denominator = longText("
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj + baseline_hhsize +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_health + baseline_smoking_30 + baseline_bmi +
    baseline_z_relative_mob + baseline_z_gini +
    lag_imp_parent_employed + lag_imp_parent_married + lag_hhsize +
    log_income_adj + lag_health + lag_bmi + lag_smoking_30 +
    lag_z_relative_mob + lag_z_gini) * as.factor(stime)
")

outcomes = c("health", "bmi", "depression", "smoking_ever", "smoking_30")
model_type = c("ordinal", "gaussian", "gaussian", "binomial", "poisson")

predictors = longText(
    "average_z_relative_mob + male + ethnicity +
     as.factor(max_age_interview_est) +
     parent_education + asvab_score + mother_age_at_birth +
     residential_moves_by_12 +
     baseline_log_income_adj + baseline_hhsize +
     baseline_imp_parent_employed + baseline_imp_parent_married +
     baseline_z_gini
")

final_model = formula(paste0("bmi", " ~ ", predictors))

relative_mob_continous_results = list()

rr = ipwExposure(
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
    final_model_type = "gaussian"
)

for (i in seq_along(outcomes)) {

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

relative_mob_continous_results

# gini
denominator_time1 = "
    male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj +
    baseline_hhsize + baseline_bmi + baseline_z_relative_mob +
    baseline_imp_parent_employed + baseline_imp_parent_married +
    baseline_health + baseline_smoking_30
"

numerator = "
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj +
    baseline_hhsize + baseline_bmi +
    baseline_z_relative_mob + baseline_z_gini +
    baseline_imp_parent_employed + bbaseline_imp_parent_married +
    baseline_health + baseline_smoking_30) * as.factor(stime)
"

denominator = "
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj +
    baseline_hhsize + baseline_bmi +
    baseline_z_relative_mob + baseline_z_gini +
    baseline_imp_parent_employed + baseline_imp_parent_married +
    baseline_health + baseline_smoking_30 +
    lag_imp_parent_employed + lag_imp_parent_married + lag_hhsize +
    log_income_adj) * as.factor(stime)
"

outcomes = c("health", "bmi", "depression", "smoking", "smoking_30")
model_type = c("ordinal", "gaussian", "gaussian", "binomial", "poisson")

predictors = "
    average_z_gini + male + ethnicity +
    as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj +
    baseline_hhsize + baseline_bmi +
    baseline_z_relative_mob + baseline_z_gini +
    baseline_imp_parent_employed + baseline_imp_parent_married +
    baseline_health + baseline_smoking_30
"

gini_continous_results = list()

for (i in seq_along(outcomes)) {

    final_model = formula(paste0(outcomes[i], " ~ ", gsub("\n", "", predictors)))

    relative_mob_continous_results[[outcomes[i]]] = ipwExposure(
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



model_smoking = formula(bmi ~
             average_z_relative_mob + male + ethnicity +
             as.factor(max_age_interview_est) +
             parent_education + asvab_score + mother_age_at_birth +
             residential_moves_by_12 +
             baseline_log_income_adj +
             baseline_hhsize + baseline_bmi +
             baseline_z_relative_mob + baseline_z_gini +
             baseline_imp_parent_employed + baseline_imp_parent_employed +
             baseline_health + baseline_smoking_30)

model_bmi = formula(bmi ~
             average_z_relative_mob + male + ethnicity +
             as.factor(max_age_interview_est) +
             parent_education + asvab_score + mother_age_at_birth +
             residential_moves_by_12 +
             baseline_log_income_adj +
             baseline_hhsize + baseline_bmi +
             baseline_z_relative_mob + baseline_z_gini +
             baseline_imp_parent_employed + baseline_imp_parent_employed +
             baseline_health + baseline_smoking_30)

model_bmi = formula(bmi ~
             average_z_relative_mob + male + ethnicity +
             as.factor(max_age_interview_est) +
             parent_education + asvab_score + mother_age_at_birth +
             residential_moves_by_12 +
             baseline_log_income_adj +
             baseline_hhsize + baseline_bmi +
             baseline_z_relative_mob + baseline_z_gini +
             baseline_imp_parent_employed + baseline_imp_parent_employed +
             baseline_health + baseline_smoking_30)

relative_mob_continuous_bmi = ipwExposure(
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
    final_model = model_bmi,
    trim_p = 0.01,
    exposure_type = "gaussian",
    final_model_type = "gaussian"
)


# ordinal variable

# relative mobility

denominator_time1 = "
    male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj +
    baseline_hhsize + baseline_bmi + as.factor(baseline_q_gini) +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_health + baseline_smoking_30
"

numerator = "
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj +
    baseline_hhsize + baseline_bmi +
    as.factor(baseline_q_relative_mob) + as.factor(baseline_q_gini) +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_health + baseline_smoking_30) + as.factor(stime)
"

denominator = "
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj +
    baseline_hhsize + baseline_bmi +
    as.factor(baseline_q_relative_mob) + as.factor(baseline_q_gini) +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_health + baseline_smoking_30 +
    lag_imp_parent_employed + lag_imp_parent_married + lag_hhsize +
    log_income_adj) + as.factor(stime)
    "

model_bmi = formula(bmi ~
             average_q_relative_mob + male + ethnicity +
             as.factor(max_age_interview_est) +
             parent_education + asvab_score + mother_age_at_birth +
             residential_moves_by_12 +
             baseline_log_income_adj +
             baseline_hhsize + baseline_bmi +
             as.factor(baseline_q_relative_mob) + as.factor(baseline_q_gini) +
             baseline_imp_parent_employed + baseline_imp_parent_employed +
             baseline_health + baseline_smoking_30)

relative_mob_ordinal_bmi = ipwExposure(
    imputations = imp,
    lag_variables = lag_vars,
    baseline_variables = baseline_vars,
    denominator_time1 = denominator_time1,
    numerator = numerator,
    denominator = denominator,
    exposure_variable = "q_relative_mob",
    id_var = "id",
    time_var = "stime",
    max_time_exposure = 8,
    final_model = model_bmi,
    trim_p = 0.01,
    exposure_type = "ordinal",
    final_model_type = "gaussian"
)

relative_mob_ordinal_bmi
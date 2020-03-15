##############################
# county income mobility and individual health
# MSM with NLSY97 data
# author: sebastian daza
##############################


library(survey)
library(data.table)
library(texreg)
library(mice)
library(mitools)
library(MASS)
source("src/utils.R")

# read imputed data
imp_q_relative_mob = readRDS('output/data/nlsy97_q_relative_mob_imputations.rds')
imp_q_absolute_mob = readRDS('output/data/nlsy97_q_absolute_mob_imputations.rds')

# transform imputation data

# relative
long_imp_q_relative_mob = data.table(
    mice::complete(imp_q_relative_mob, "long", include = FALSE)
    )
setnames(long_imp_q_relative_mob, c(".imp", ".id"), c("imp_num", "row_num"))
long_imp_q_relative_mob[, age_interview_est :=
        as.numeric(as.character(age_interview_est))]
long_imp_q_relative_mob[, good_health :=
    ifelse(rev_health %in% c(4, 5), 1, 0)]
long_imp_q_relative_mob[, q_relative_mob := as.numeric(as.character(q_relative_mob))]
long_imp_q_relative_mob[, q_gini := as.numeric(as.character(q_gini))]
long_imp_q_relative_mob[, rev_health := factor(rev_health)]

str(long_imp_q_relative_mob)
# absolute
long_imp_q_absolute_mob = data.table(
    mice::complete(imp_q_absolute_mob, "long", include = FALSE)
    )
setnames(long_imp_q_absolute_mob, c(".imp", ".id"), c("imp_num", "row_num"))
long_imp_q_absolute_mob[, age_interview_est :=
        as.numeric(as.character(age_interview_est))]
long_imp_q_absolute_mob[, good_health :=
    ifelse(rev_health %in% c(4, 5), 1, 0)]
long_imp_q_absolute_mob[, rev_health := factor(rev_health)]

# number of observations
N = length(unique(long_imp_q_relative_mob$id))

# outcomes
outcomes = c("rev_health", "bmi", "depression", "smoking", "smoking_30")
model_types = c("ordinal", "gaussian", "gaussian", "binomial", "poisson")

# unadjusted models

# z_relative_mob models
unadjusted_q_relative_mob_results = unadjustedRegression(
    imputations = long_imp_q_relative_mob,
    exposure_variable = "q_relative_mob",
    exposure_type = "ordinal",
    id_var = "id",
    time_var = "stime",
    max_time_exposure = 8,
    outcomes = outcomes,
    final_model_types = model_types
)

# z_absolute_mob models
unadjusted_q_absolute_mob_results = unadjustedRegression(
    imputations = long_imp_q_absolute_mob,
    exposure_variable = "q_absolute_mob",
    exposure_type = "ordinal",
    id_var = "id",
    time_var = "stime",
    max_time_exposure = 8,
    outcomes = outcomes,
    final_model_type = model_types
)

# z_gini models
unadjusted_q_gini_results = unadjustedRegression(
    imputations = long_imp_q_relative_mob,
    exposure_variable = "q_gini",
    exposure_type = "ordinal",
    id_var = "id",
    time_var = "stime",
    max_time_exposure = 8,
    outcomes = outcomes,
    final_model_type = model_types
)

# create table unadjusted models

# put model outputs in a list
list_rows = list(
    unadjusted_q_relative_mob_results,
    unadjusted_q_absolute_mob_results,
    unadjusted_q_gini_results
)

row_names = c("average_q_relative_mob",
              "average_q_absolute_mob",
              "average_q_gini")

row_labels = c("Rank-rank",
               "Upward mobility $\\times$ -1",
               "Gini"
               )

column_names = c("Health status",
                 "BMI",
                 "Depression",
                 "Smoking",
                 "Days smoking last month")

comment = longText("Each coefficient represents a model. Coefficients and standard errors are combined estimates from 20 multiple imputed datasets.
Analyses based on exposure from 12 to 20 years old. We estimate different models depending on the outcome: Ordinal regression (self-reported health), Generalized linear model (BMI, depression), Logistic regression (smoking), Poisson regression (number of days smoking last month). $^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$"
)

caption = "Unadjusted estimates of average exposure (categorical) \\newline on health indicators, NLSY97"
label = "tab:nlsy97_unadjusted_q_models"

# groups = list("Relative" = 1:2,
#               "Absolute" = 3:4,
#               "Inequality" = 5:6)

createModelTables(
    list_rows,
    caption = caption,
    label = label,
    column_names = column_names,
    row_names = row_names,
    row_labels = row_labels,
    filename = "output/tables/nlsy97_unadjusted_q_models.tex",
    comment = comment,
    # groups = groups,
    observations = N
)

# adjusted models

# lagged and baseline variables
lag_vars = c("imp_parent_employed", "imp_parent_married", "hhsize",
             "bmi", "rev_health", "smoking", "smoking_30", "log_population",
             "log_county_income", "z_prop_black",
             "q_relative_mob", "q_gini", "q_absolute_mob")

baseline_vars = c("z_relative_mob", "z_gini", "z_absolute_mob",
                  "relative_mob_resid", "absolute_mob_resid", "gini_resid",
                  "q_relative_mob", "q_absolute_mob", "q_gini",
                  "q_relative_mob_resid", "q_absolute_mob_resid", "q_gini_resid",
                  "imp_parent_employed",
                  "imp_parent_married", "hhsize", "bmi", "rev_health",
                  "smoking", "smoking_30", "log_population",
                  "log_county_income", "z_prop_black", "log_income_adj")

# create outcome models

# z_relative_mob
denominator_time1 = longText("
    male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 + log_income_adj
")

numerator = longText("
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj + baseline_hhsize +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_rev_health + baseline_bmi + baseline_smoking +
    as.factor(baseline_q_relative_mob) + as.factor(baseline_q_gini) +
    baseline_log_population +
    baseline_log_county_income + baseline_z_prop_black +
    as.factor(lag_q_relative_mob)) + as.factor(stime)
")

denominator = longText("
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj + baseline_hhsize +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_rev_health + baseline_smoking + baseline_bmi +
    as.factor(baseline_q_relative_mob) + as.factor(baseline_q_gini) + baseline_log_population +
    baseline_log_county_income + baseline_z_prop_black +
    nmoves + lag_imp_parent_employed + lag_imp_parent_married + lag_hhsize +
    log_income_adj + lag_rev_health + lag_bmi + lag_smoking +
    as.factor(lag_q_relative_mob) + as.factor(lag_q_gini) + lag_log_county_income +
    lag_log_population + lag_z_prop_black) + as.factor(stime)
")

predictors = longText(
    "average_q_relative_mob + male + ethnicity +
     as.factor(max_age_interview_est) +
     parent_education + asvab_score + mother_age_at_birth +
     residential_moves_by_12 +
     baseline_log_income_adj + baseline_hhsize +
     baseline_imp_parent_employed + baseline_imp_parent_married +
     as.factor(baseline_q_gini) + baseline_log_population +
     baseline_log_county_income + baseline_z_prop_black
")

q_relative_mob_results = ipwExposure(
    imputations = long_imp_q_relative_mob,
    lag_variables = lag_vars,
    baseline_variables = baseline_vars,
    denominator_time1 = denominator_time1,
    numerator = numerator,
    denominator = denominator,
    exposure_variable = "q_relative_mob",
    id_var = "id",
    time_var = "stime",
    max_time_exposure = 8,
    trim_p = 0.01,
    exposure_type = "ordinal",
    outcomes = outcomes,
    predictors = predictors,
    final_model_types = model_types
)

# z_absolute_mob
denominator_time1 = longText("
    male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 + log_income_adj
")

numerator = longText("
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj + baseline_hhsize +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_rev_health + baseline_bmi + baseline_smoking +
    as.factor(baseline_q_absolute_mob) +
    as.factor(baseline_q_gini) + baseline_log_population +
    baseline_log_county_income + baseline_z_prop_black +
    as.factor(lag_q_absolute_mob)) + as.factor(stime)
")

denominator = longText("
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj + baseline_hhsize +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_rev_health + baseline_smoking + baseline_bmi +
    as.factor(baseline_q_absolute_mob) + as.factor(baseline_q_gini) + baseline_log_population +
    baseline_log_county_income + baseline_z_prop_black +
    nmoves + lag_imp_parent_employed + lag_imp_parent_married + lag_hhsize +
    log_income_adj + lag_rev_health + lag_bmi + lag_smoking +
    as.factor(lag_q_absolute_mob) + as.factor(lag_q_gini) + lag_log_county_income +
    lag_log_population + lag_z_prop_black) + as.factor(stime)
")

predictors = longText(
    "average_q_absolute_mob + male + ethnicity +
     as.factor(max_age_interview_est) +
     parent_education + asvab_score + mother_age_at_birth +
     residential_moves_by_12 +
     baseline_log_income_adj + baseline_hhsize +
     baseline_imp_parent_employed + baseline_imp_parent_married +
     as.factor(baseline_q_gini) + baseline_log_population +
     baseline_log_county_income + baseline_z_prop_black
")

q_absolute_mob_results = ipwExposure(
    imputations = long_imp_q_absolute_mob,
    lag_variables = lag_vars,
    baseline_variables = baseline_vars,
    denominator_time1 = denominator_time1,
    numerator = numerator,
    denominator = denominator,
    exposure_variable = "q_absolute_mob",
    id_var = "id",
    time_var = "stime",
    max_time_exposure = 8,
    trim_p = 0.01,
    exposure_type = "ordinal",
    outcomes = outcomes,
    predictors = predictors,
    final_model_types = model_types
    )


# z_gini
denominator_time1 = longText("
    male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 + log_income_adj
")

numerator = longText("
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj + baseline_hhsize +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_rev_health + baseline_smoking + baseline_bmi +
    as.factor(baseline_q_relative_mob) + as.factor(baseline_q_gini) +
    baseline_log_population +
    baseline_log_county_income + baseline_z_prop_black + as.factor(lag_q_gini)) +
    as.factor(stime)
")

denominator = longText("
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj + baseline_hhsize +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_rev_health + baseline_smoking + baseline_bmi +
    as.factor(baseline_q_relative_mob) + as.factor(baseline_q_gini) +
    baseline_log_population +
    baseline_log_county_income + baseline_z_prop_black +
    nmoves + lag_imp_parent_employed + lag_imp_parent_married + lag_hhsize +
    log_income_adj + lag_rev_health + lag_bmi + lag_smoking +
    as.factor(lag_q_relative_mob) + as.factor(lag_q_gini) +
    lag_log_county_income +
    lag_log_population + lag_z_prop_black) + as.factor(stime)
")

predictors = longText(
    "average_q_gini + male + ethnicity +
     as.factor(max_age_interview_est) +
     parent_education + asvab_score + mother_age_at_birth +
     residential_moves_by_12 +
     baseline_log_income_adj + baseline_hhsize +
     baseline_imp_parent_employed + baseline_imp_parent_married +
     as.factor(baseline_q_relative_mob) + baseline_log_population +
     baseline_log_county_income + baseline_z_prop_black
")

q_gini_results = ipwExposure(
    imputations = long_imp_q_relative_mob,
    lag_variables = lag_vars,
    baseline_variables = baseline_vars,
    denominator_time1 = denominator_time1,
    numerator = numerator,
    denominator = denominator,
    exposure_variable = "q_gini",
    id_var = "id",
    time_var = "stime",
    max_time_exposure = 8,
    trim_p = 0.01,
    exposure_type = "ordinal",
    outcomes = outcomes,
    predictors = predictors,
    final_model_types = model_types
)

# create tables adjusted results

# list of results
list_rows = list(q_relative_mob_results,
                 q_absolute_mob_results,
                 q_gini_results)

row_names = c("average_q_relative_mob",
              "average_q_absolute_mob",
              "average_q_gini"
              )

row_labels = c("Rank-rank",
               "Upward mobility $\\times$ -1",
               "Gini"
               )

column_names = c("Health status", "BMI", "Depression", "Smoking", "Days smoking last month")
caption = "Adjusted estimates of average exposure (categorical) \\newline on health indicators, NLSY97"
label = "tab:nlsy97_adjusted_q_models"

comment = longText("Each coefficient represents a model. Coefficients and standard errors are combined estimates from 30 multiple imputed datasets.
Analyses based on exposure from 12 to 20 years old. We estimate different models depending on the outcome: Ordinal regression (self-reported health), General linear model (BMI, depression), Logistic regression (smoking), Poisson regression (days smoking last month). $^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$"
)

# groups = list("Relative" = 1:2,
#               "Absolute" = 3:4,
#               "Inequality" = 5:6)

createModelTables(
    list_rows,
    caption = caption,
    label = label,
    column_names = column_names,
    row_names = row_names,
    row_labels = row_labels,
    filename = "output/tables/nlsy97_adjusted_q_models.tex",
    comment = comment,
    # groups = groups,
    observations = N
)

# create weight tables
list_weights = list(
    q_relative_mob_results[["weights"]],
    q_absolute_mob_results[["weights"]],
    q_gini_results[["weights"]]
)

tableWeights(list_weights,
             model_names = c("Rank-rank correlation",
                             "Upward mobility",
                             "Gini"),
             caption  = "NLSY97 Stabilized treatment weights, categorial exposure",
             label = "tab:ipt_weigths_q",
             comment = "Analyses based on exposure from 12 to 20 years old. ",
             filename = "output/tables/nlsy97_ipt_weights_q.tex",
             tabcolsep = 10,
             arraystretch = 1)


# residuals

# read imputed data
imp_qr_relative_mob = readRDS('output/data/nlsy97_qr_relative_mob_imputations.rds')
imp_qr_absolute_mob = readRDS('output/data/nlsy97_qr_absolute_mob_imputations.rds')

test = mice::complete(imp_qr_relative_mob, 1)
table(test$q_relative_mob)
table(test$q_relative_mob_resid)

# transform imputation data

# relative
long_imp_qr_relative_mob = data.table(
    mice::complete(imp_qr_relative_mob, "long", include = FALSE)
    )
setnames(long_imp_qr_relative_mob, c(".imp", ".id"), c("imp_num", "row_num"))
long_imp_qr_relative_mob[, age_interview_est :=
        as.numeric(as.character(age_interview_est))]
long_imp_qr_relative_mob[, good_health :=
    ifelse(rev_health %in% c(4, 5), 1, 0)]
long_imp_qr_relative_mob[, q_relative_mob := as.numeric(as.character(q_relative_mob))]
long_imp_qr_relative_mob[, q_gini := as.numeric(as.character(q_gini))]
long_imp_qr_relative_mob[, rev_health := factor(rev_health)]

# absolute
long_imp_qr_absolute_mob = data.table(
    mice::complete(imp_qr_absolute_mob, "long", include = FALSE)
    )
setnames(long_imp_qr_absolute_mob, c(".imp", ".id"), c("imp_num", "row_num"))
long_imp_qr_absolute_mob[, age_interview_est :=
        as.numeric(as.character(age_interview_est))]
long_imp_qr_absolute_mob[, good_health :=
    ifelse(rev_health %in% c(4, 5), 1, 0)]
long_imp_qr_absolute_mob[, rev_health := factor(rev_health)]

# number of observations
N = length(unique(long_imp_qr_relative_mob$id))

# outcomes
outcomes = c("rev_health", "bmi", "depression", "smoking", "smoking_30")
model_types = c("ordinal", "gaussian", "gaussian", "binomial", "poisson")

# unadjusted models

# z_relative_mob models
unadjusted_qr_relative_mob_results = unadjustedRegression(
    imputations = long_imp_qr_relative_mob,
    exposure_variable = "q_relative_mob_resid",
    exposure_type = "ordinal",
    id_var = "id",
    time_var = "stime",
    max_time_exposure = 8,
    outcomes = outcomes,
    final_model_types = model_types
)

# z_absolute_mob models
unadjusted_qr_absolute_mob_results = unadjustedRegression(
    imputations = long_imp_qr_absolute_mob,
    exposure_variable = "q_absolute_mob_resid",
    exposure_type = "ordinal",
    id_var = "id",
    time_var = "stime",
    max_time_exposure = 8,
    outcomes = outcomes,
    final_model_type = model_types
)

# z_gini models
unadjusted_qr_gini_results = unadjustedRegression(
    imputations = long_imp_qr_relative_mob,
    exposure_variable = "q_gini_resid",
    exposure_type = "ordinal",
    id_var = "id",
    time_var = "stime",
    max_time_exposure = 8,
    outcomes = outcomes,
    final_model_type = model_types
)

# create table unadjusted models

# put model outputs in a list
list_rows = list(
    unadjusted_qr_relative_mob_results,
    unadjusted_qr_absolute_mob_results,
    unadjusted_qr_gini_results
)

row_names = c("average_q_relative_mob_resid",
              "average_q_absolute_mob_resid",
              "average_q_gini_resid")

row_labels = c("Rank-rank",
               "Upward mobility $\\times$ -1",
               "Gini"
               )

column_names = c("Health status",
                 "BMI",
                 "Depression",
                 "Smoking",
                 "Days smoking last month")

comment = longText("Each coefficient represents a model. Coefficients and standard errors are combined estimates from 20 multiple imputed datasets.
Analyses based on exposure from 12 to 20 years old. We estimate different models depending on the outcome: Ordinal regression (self-reported health), Generalized linear model (BMI, depression), Logistic regression (smoking), Poisson regression (number of days smoking last month). $^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$"
)

caption = "Unadjusted estimates of average residual exposure (categorical) \\newline on health indicators, NLSY97"
label = "tab:nlsy97_unadjusted_qr_models"

# groups = list("Relative" = 1:2,
#               "Absolute" = 3:4,
#               "Inequality" = 5:6)

createModelTables(
    list_rows,
    caption = caption,
    label = label,
    column_names = column_names,
    row_names = row_names,
    row_labels = row_labels,
    filename = "output/tables/nlsy97_unadjusted_qr_models.tex",
    comment = comment,
    # groups = groups,
    observations = N
)

# residuals

# lagged and baseline variables
lag_vars = c("imp_parent_employed", "imp_parent_married", "hhsize",
             "bmi", "rev_health", "smoking", "smoking_30", "log_population",
             "log_county_income", "z_prop_black",
             "q_relative_mob_resid", "q_gini_resid", "q_absolute_mob_resid")

baseline_vars = c("z_relative_mob", "z_gini", "z_absolute_mob",
                  "relative_mob_resid", "absolute_mob_resid", "gini_resid",
                  "q_relative_mob", "q_absolute_mob", "q_gini",
                  "q_relative_mob_resid", "q_absolute_mob_resid", "q_gini_resid",
                  "imp_parent_employed",
                  "imp_parent_married", "hhsize", "bmi", "rev_health",
                  "smoking", "smoking_30", "log_population",
                  "log_county_income", "z_prop_black", "log_income_adj")

# create outcome models

# z_relative_mob
denominator_time1 = longText("
    male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 + log_income_adj
")

numerator = longText("
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj + baseline_hhsize +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_rev_health + baseline_bmi + baseline_smoking +
    baseline_q_relative_mob_resid + baseline_q_gini_resid + baseline_log_population +
    baseline_log_county_income + baseline_z_prop_black +
    lag_q_relative_mob_resid) + as.factor(stime)
")

denominator = longText("
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj + baseline_hhsize +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_rev_health + baseline_smoking + baseline_bmi +
    baseline_q_relative_mob_resid + baseline_q_gini_resid + baseline_log_population +
    baseline_log_county_income + baseline_z_prop_black +
    nmoves + lag_imp_parent_employed + lag_imp_parent_married + lag_hhsize +
    log_income_adj + lag_rev_health + lag_bmi + lag_smoking +
    lag_q_relative_mob_resid + lag_q_gini_resid + lag_log_county_income +
    lag_log_population + lag_z_prop_black) + as.factor(stime)
")

predictors = longText(
    "average_q_relative_mob_resid + male + ethnicity +
     as.factor(max_age_interview_est) +
     parent_education + asvab_score + mother_age_at_birth +
     residential_moves_by_12 +
     baseline_log_income_adj + baseline_hhsize +
     baseline_imp_parent_employed + baseline_imp_parent_married +
     baseline_q_gini_resid + baseline_log_population +
     baseline_log_county_income + baseline_z_prop_black
")

qr_relative_mob_results = ipwExposure(
    imputations = long_imp_qr_relative_mob,
    lag_variables = lag_vars,
    baseline_variables = baseline_vars,
    denominator_time1 = denominator_time1,
    numerator = numerator,
    denominator = denominator,
    exposure_variable = "q_relative_mob_resid",
    id_var = "id",
    time_var = "stime",
    max_time_exposure = 8,
    trim_p = 0.01,
    exposure_type = "ordinal",
    outcomes = outcomes,
    predictors = predictors,
    final_model_types = model_types,
    factor_columns = "rev_health"
)

summary(qr_relative_mob_results[["weights"]])

# z_absolute_mob
denominator_time1 = longText("
    male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 + log_income_adj
")

numerator = longText("
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj + baseline_hhsize +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_rev_health + baseline_bmi + baseline_smoking +
    baseline_q_absolute_mob_resid + baseline_q_gini_resid + baseline_log_population +
    baseline_log_county_income + baseline_z_prop_black +
    lag_q_absolute_mob_resid) + as.factor(stime)
")

denominator = longText("
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj + baseline_hhsize +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_rev_health + baseline_smoking + baseline_bmi +
    baseline_q_absolute_mob_resid + baseline_q_gini_resid + baseline_log_population +
    baseline_log_county_income + baseline_z_prop_black +
    nmoves + lag_imp_parent_employed + lag_imp_parent_married + lag_hhsize +
    log_income_adj + lag_rev_health + lag_bmi + lag_smoking +
    lag_q_absolute_mob_resid + lag_q_gini_resid + lag_log_county_income +
    lag_log_population + lag_z_prop_black) + as.factor(stime)
")

predictors = longText(
    "average_q_absolute_mob_resid + male + ethnicity +
     as.factor(max_age_interview_est) +
     parent_education + asvab_score + mother_age_at_birth +
     residential_moves_by_12 +
     baseline_log_income_adj + baseline_hhsize +
     baseline_imp_parent_employed + baseline_imp_parent_married +
     baseline_q_gini_resid + baseline_log_population +
     baseline_log_county_income + baseline_z_prop_black
")

qr_absolute_mob_results = ipwExposure(
    imputations = long_imp_qr_absolute_mob,
    lag_variables = lag_vars,
    baseline_variables = baseline_vars,
    denominator_time1 = denominator_time1,
    numerator = numerator,
    denominator = denominator,
    exposure_variable = "q_absolute_mob_resid",
    id_var = "id",
    time_var = "stime",
    max_time_exposure = 8,
    trim_p = 0.01,
    exposure_type = "ordinal",
    outcomes = outcomes,
    predictors = predictors,
    final_model_types = model_types,
    factor_columns = "rev_health"
)

summary(qr_absolute_mob_results[["weights"]])

# z_gini
denominator_time1 = longText("
    male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 + log_income_adj
")

numerator = longText("
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj + baseline_hhsize +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_rev_health + baseline_bmi + baseline_smoking +
    baseline_q_relative_mob_resid + baseline_q_gini_resid + baseline_log_population +
    baseline_log_county_income + baseline_z_prop_black +
    lag_q_gini_resid) + as.factor(stime)
")

denominator = longText("
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj + baseline_hhsize +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_rev_health + baseline_smoking + baseline_bmi +
    baseline_q_relative_mob_resid + baseline_q_gini_resid + baseline_log_population +
    baseline_log_county_income + baseline_z_prop_black +
    nmoves + lag_imp_parent_employed + lag_imp_parent_married + lag_hhsize +
    log_income_adj + lag_rev_health + lag_bmi + lag_smoking +
    lag_q_relative_mob_resid + lag_q_gini_resid + lag_log_county_income +
    lag_log_population + lag_z_prop_black) + as.factor(stime)
")

predictors = longText(
    "average_q_gini_resid + male + ethnicity +
     as.factor(max_age_interview_est) +
     parent_education + asvab_score + mother_age_at_birth +
     residential_moves_by_12 +
     baseline_log_income_adj + baseline_hhsize +
     baseline_imp_parent_employed + baseline_imp_parent_married +
     baseline_q_relative_mob_resid + baseline_log_population +
     baseline_log_county_income + baseline_z_prop_black
")

qr_gini_results = ipwExposure(
    imputations = long_imp_qr_relative_mob,
    lag_variables = lag_vars,
    baseline_variables = baseline_vars,
    denominator_time1 = denominator_time1,
    numerator = numerator,
    denominator = denominator,
    exposure_variable = "q_gini_resid",
    id_var = "id",
    time_var = "stime",
    max_time_exposure = 8,
    trim_p = 0.01,
    exposure_type = "ordinal",
    outcomes = outcomes,
    predictors = predictors,
    final_model_types = model_types,
    factor_columns = "rev_health"
)

summary(qr_gini_results[["weights"]])
# create tables adjusted results

# list of results
list_rows = list(qr_relative_mob_results,
                 qr_absolute_mob_results,
                 qr_gini_results)

row_names = c("average_q_relative_mob_resid",
              "average_q_absolute_mob_resid",
              "average_q_gini_resid"
              )

row_labels = c("Rank-rank",
               "Upward mobility $\\times$ -1",
               "Gini"
               )

column_names = c("Health status", "BMI", "Depression", "Smoking", "Days smoking last month")
caption = "Adjusted estimates of average residual exposure (categorical) \\newline on health indicators, NLSY97"
label = "tab:nlsy97_adjusted_qr_models"

comment = longText("Each coefficient represents a model. Coefficients and standard errors are combined estimates from 30 multiple imputed datasets.
Analyses based on exposure from 12 to 20 years old. We estimate different models depending on the outcome: Ordinal regression (self-reported health), General linear model (BMI, depression), Logistic regression (smoking), Poisson regression (days smoking last month). $^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$"
)

# groups = list("Relative" = 1:2,
#               "Absolute" = 3:4,
#               "Inequality" = 5:6)

createModelTables(
    list_rows,
    caption = caption,
    label = label,
    column_names = column_names,
    row_names = row_names,
    row_labels = row_labels,
    filename = "output/tables/nlsy97_adjusted_qr_models.tex",
    comment = comment,
    # groups = groups,
    observations = N
)


# create weight tables
list_weights = list(
    qr_relative_mob_results[["weights"]],
    qr_absolute_mob_results[["weights"]],
    qr_gini_results[["weights"]]
)

tableWeights(list_weights,
             model_names = c("Rank-rank correlation",
                             "Upward mobility",
                             "Gini"),
             caption  = "NLSY97 Stabilized treatment weights, categorical residual exposure",
             label = "tab:ipt_weigths_qr",
             comment = "Analyses based on exposure from 12 to 20 years old. ",
             filename = "output/tables/nlsy97_ipt_weights_qr.tex",
             tabcolsep = 10,
             arraystretch = 1)
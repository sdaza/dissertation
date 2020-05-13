##############################
# county income mobility and individual health
# MSM with PSID data
# author: sebastian daza
##############################


library(survey)
library(data.table)
library(texreg)
library(mitools)
library(MASS)
library(forcats)
source("src/utils.R")

# read imputed data
imp_z_relative_mob = readRDS('output/data/psid_z_relative_mob_imputations.rds')
imp_z_absolute_mob = readRDS('output/data/psid_z_absolute_mob_imputations.rds')

# transform imputations

# relative
long_imp_z_relative_mob = data.table(mice::complete(imp_z_relative_mob, "long", include = FALSE))
setnames(long_imp_z_relative_mob, c(".imp", ".id"), c("imp_num", "row_num"))
# add sampling weigths
long_imp_z_relative_mob = merge(long_imp_z_relative_mob, sw, on = "pid")
# adjusting variables
long_imp_z_relative_mob[, last_wave_rev_health := factor(last_wave_rev_health)]
long_imp_z_relative_mob[, last_wave_rev_health_c := fct_collapse(
    last_wave_rev_health, "1" = c("1", "2"), "2" = "3", "3" = "4", "4" = "5")]
long_imp_z_relative_mob[, last_wave_good_health := ifelse(last_wave_rev_health %in% c(4, 5), 1, 0)]
table(long_imp_z_relative_mob$last_wave_good_health)
long_imp_z_relative_mob[, last_wave_smoking_number := as.integer(last_wave_smoking_number)]

# absolute
long_imp_z_absolute_mob = data.table(mice::complete(imp_z_absolute_mob, "long", include = FALSE))
setnames(long_imp_z_absolute_mob, c(".imp", ".id"), c("imp_num", "row_num"))
# add sampling weigths
long_imp_z_absolute_mob = merge(long_imp_z_absolute_mob, sw, on = "pid")
# adjusting variables
long_imp_z_absolute_mob[, last_wave_rev_health := factor(last_wave_rev_health)]
long_imp_z_absolute_mob[, last_wave_rev_health_c :=
   fct_collapse(last_wave_rev_health, "1" = c("1", "2"), "2" = "3", "3" = "4", "4" = "5")]
long_imp_z_absolute_mob[, last_wave_good_health := ifelse(last_wave_rev_health %in% c(4, 5), 1, 0)]
table(long_imp_z_relative_mob$last_wave_good_health)
long_imp_z_absolute_mob[, last_wave_smoking_number := as.integer(last_wave_smoking_number)]

# number of observations
N = length(unique(long_imp_z_relative_mob$pid))

outcomes = c("last_wave_rev_health_c", "last_wave_bmi", "last_wave_depression",
             "last_wave_smoking", "last_wave_smoking_number")
model_types = c("ordinal", "gaussian", "gaussian", "binomial", "poisson")

# loop of unadjusted models

# z_relative_mob models
unadjusted_z_relative_mob_results = unadjustedRegression(
    imputations = long_imp_z_relative_mob,
    exposure_variable = "z_relative_mob",
    exposure_type = "gaussian",
    id_var = "pid",
    time_var = "time",
    max_time_exposure = 20,
    outcome = outcomes,
    final_model_types = model_types,
    sampling_weight = "sweight",
    strata = "stratum",
    cluster = "cluster"
)

# z_absolute_mob models
unadjusted_z_absolute_mob_results = unadjustedRegression(
    imputations = long_imp_z_absolute_mob,
    exposure_variable = "z_absolute_mob",
    exposure_type = "gaussian",
    id_var = "pid",
    time_var = "time",
    max_time_exposure = 20,
    outcome = outcomes,
    final_model_types = model_types,
    sampling_weight = "sweight",
    strata = "stratum",
    cluster = "cluster"
)


# z_gini models
unadjusted_z_gini_results = unadjustedRegression(
    imputations = long_imp_z_relative_mob,
    exposure_variable = "z_gini",
    exposure_type = "gaussian",
    id_var = "pid",
    time_var = "time",
    max_time_exposure = 20,
    outcome = outcomes,
    final_model_types = model_types,
    sampling_weight = "sweight",
    strata = "stratum",
    cluster = "cluster"
)


# create table unadjusted models

# put model outputs in a list
list_rows = list(
    unadjusted_z_relative_mob_results,
    unadjusted_z_absolute_mob_results,
    unadjusted_z_gini_results
)

row_names = c("average_z_relative_mob",
              "average_z_absolute_mob",
              "average_z_gini")

row_labels = c("Rank-rank",
               "Upward mobility $\\times$ -1",
               "Gini"
)

column_names = c("Health status",
                 "BMI",
                 "Depression",
                 "Smoking",
                 "Number cigarettes smoked last month")

comment = longText("Each coefficient represents a model. Coefficients and standard errors are combined estimates from 20 multiple imputed datasets.
                   Analyses based on exposure from 1 to 20 years old.
                   We estimate different models depending on the outcome:
                   Ordinal regression (self-reported health), Generalized linear model (BMI, depression),
                   Logistic regression (smoking), Poisson regression (number of cigarettes smoked last month).
                   $^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$"
)

caption = "Unadjusted estimates of average exposure (continuous) \\newline on health indicators, PSID"
label = "tab:psid_unadjusted_z_models"

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
    filename = "output/tables/psid_unadjusted_z_models.tex",
    comment = comment,
    # groups = groups,
    observations = N
)

# adjusted models

# lagged and baseline variables
lag_vars = c("z_relative_mob", "z_gini", "z_absolute_mob", "log_population",
             "log_county_income", "z_prop_black", "famsize",
             "head_marital_status", "head_education",
             "head_owns_house", "head_working_binary")

baseline_vars = c("z_relative_mob", "z_gini", "z_absolute_mob",
                  "log_population", "log_county_income", "z_prop_black",
                  "log_income_adj",
                  "famsize", "head_marital_status",
                  "head_education", "head_owns_house", "head_working_binary")

# create outcome models

# z_relative_mob

denominator_time1 = longText("
    male + white + as.factor(first_year) +
    weight_less_55 + mother_marital_status + mother_age + sweight
")

numerator = longText("
    (male + white + as.factor(first_year) +
     weight_less_55 + mother_marital_status + mother_age +
     baseline_log_income_adj + baseline_famsize +
     lag_z_relative_mob) + as.factor(time) + sweight
")

denominator = longText("
    (male + white + as.factor(first_year) +
     weight_less_55 + mother_marital_status + mother_age +
     baseline_log_income_adj + baseline_famsize +
     nmoves +
     lag_z_gini + lag_log_population + lag_log_county_income +
     lag_z_prop_black + lag_famsize + lag_head_marital_status +
     lag_head_education + lag_head_owns_house + lag_head_working_binary +
     lag_z_relative_mob) + as.factor(time) + sweight
")

predictors = longText(
    "average_z_relative_mob + male + white + as.factor(first_year) +
     weight_less_55 + mother_marital_status + mother_age +
     baseline_log_income_adj + baseline_famsize
")

z_relative_mob_results = ipwExposure(
        imputations = long_imp_z_relative_mob,
        lag_variables = lag_vars,
        baseline_variables = baseline_vars,
        denominator_time1 = denominator_time1,
        numerator = numerator,
        denominator = denominator,
        exposure_variable = "z_relative_mob",
        id_var = "pid",
        time_var = "time",
        max_time_exposure = 20,
        trim_p = 0.01,
        exposure_type = "gaussian",
        outcomes = outcomes,
        predictors = predictors,
        final_model_types = model_types,
        sampling_weight = "sweight",
        strata = "stratum",
        cluster = "cluster"
)

z_relative_mob_results[[4]]


# z_absolute_mob
denominator_time1 = longText("
    male + white + as.factor(first_year) +
    weight_less_55 + mother_marital_status + mother_age + sweight
")

numerator = longText("
    (male + white + as.factor(first_year) +
     weight_less_55 + mother_marital_status + mother_age +
     baseline_log_income_adj + baseline_famsize +
     lag_z_absolute_mob) + as.factor(time) + sweight
")

denominator = longText("
    (male + white + as.factor(first_year) +
     weight_less_55 + mother_marital_status + mother_age +
     baseline_log_income_adj + baseline_famsize +
     nmoves +
     lag_z_gini + lag_log_population + lag_log_county_income +
     lag_z_prop_black + lag_famsize + lag_head_marital_status +
     lag_head_education + lag_head_owns_house + lag_head_working_binary +
     lag_z_absolute_mob) + as.factor(time) + sweight
")

predictors = longText(
    "average_z_absolute_mob + male + white + as.factor(first_year) +
     weight_less_55 + mother_marital_status + mother_age +
     baseline_log_income_adj + baseline_famsize
")

z_absolute_mob_results = ipwExposure(
    imputations = long_imp_z_absolute_mob,
    lag_variables = lag_vars,
    baseline_variables = baseline_vars,
    denominator_time1 = denominator_time1,
    numerator = numerator,
    denominator = denominator,
    exposure_variable = "z_absolute_mob",
    id_var = "pid",
    time_var = "time",
    max_time_exposure = 20,
    trim_p = 0.01,
    exposure_type = "gaussian",
    outcomes = outcomes,
    predictors = predictors,
    final_model_types =  model_types,
    sampling_weight = "sweight",
    strata = "stratum",
    cluster = "cluster"

)

# z_gini
denominator_time1 = longText("
    male + white + as.factor(first_year) +
    weight_less_55 + mother_marital_status + mother_age + sweight
")

numerator = longText("
    (male + white + as.factor(first_year)  +
     weight_less_55 + mother_marital_status + mother_age +
     baseline_log_income_adj + baseline_famsize +
     lag_z_gini) + as.factor(time) + sweight
")

denominator = longText("
    (male + white + as.factor(first_year) +
     weight_less_55 + mother_marital_status + mother_age +
     baseline_log_income_adj + baseline_famsize +
     nmoves +
     lag_z_relative_mob + lag_log_population + lag_log_county_income +
     lag_z_prop_black + lag_famsize + lag_head_marital_status +
     lag_head_education + lag_head_owns_house + lag_head_working_binary +
     lag_z_gini) + as.factor(time) + sweight
")

predictors = longText(
    "average_z_gini + male + white + as.factor(first_year) +
     weight_less_55 + mother_marital_status + mother_age +
     baseline_log_income_adj + baseline_famsize
")

z_gini_results = ipwExposure(
    imputations = long_imp_z_relative_mob,
    lag_variables = lag_vars,
    baseline_variables = baseline_vars,
    denominator_time1 = denominator_time1,
    numerator = numerator,
    denominator = denominator,
    exposure_variable = "z_gini",
    id_var = "pid",
    time_var = "time",
    max_time_exposure = 20,
    trim_p = 0.01,
    exposure_type = "gaussian",
    outcomes = outcomes,
    predictors = predictors,
    final_model_types =  model_types,
    sampling_weight = "sweight",
    strata = "stratum",
    cluster = "cluster"
)

# create table adjusted models
list_rows = list(z_relative_mob_results,
                 z_absolute_mob_results,
                 z_gini_results
                 )

row_names = c("average_z_relative_mob",
              "average_z_absolute_mob",
              "average_z_gini"
              )

column_names = c("Health status",
                 "BMI",
                 "Depression",
                 "Smoking",
                 "Number cigarettes smoked last month")

comment = longText("Each coefficient represents a model. Coefficients and standard errors are combined estimates from 20 multiple imputed datasets.
                   Analyses based on exposure from 1 to 20 years old.
                   We estimate different models depending on the outcome:
                   Ordinal regression (self-reported health), Generalized linear model (BMI, depression),
                   Logistic regression (smoking), Poisson regression (number of cigarettes smoked last month).
                   $^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$"
)

caption = "Adjusted estimates of average exposure (continuous) \\newline on health indicators, PSID"
label = "tab:psid_adjusted_z_models"

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
    filename = "output/tables/psid_adjusted_z_models.tex",
    comment = comment,
    # groups = groups,
    observations = N
)

# create weight tables
list_weights = list(
    z_relative_mob_results[["weights"]],
    z_absolute_mob_results[["weights"]],
    z_gini_results[["weights"]]
)

tableWeights(list_weights,
             model_names = c("Rank-rank correlation",
                             "Upward mobility",
                             "Gini"),
             caption  = "Stabilized treatment weights, continuous exposure",
             label = "tab:psid_ipt_weigths_z",
             comment = "Analyses based on exposure from 1 to 20 years old. ",
             filename = "output/tables/psid_ipt_weights_z.tex",
             tabcolsep = 10,
             arraystretch = 1)


# residuals

# read imputed data
imp_zr_relative_mob = readRDS('output/data/psid_zr_relative_mob_imputations.rds')
imp_zr_absolute_mob = readRDS('output/data/psid_zr_absolute_mob_imputations.rds')

# transform imputations

# relative
long_imp_zr_relative_mob = data.table(mice::complete(imp_zr_relative_mob, "long", include = FALSE))
setnames(long_imp_zr_relative_mob, c(".imp", ".id"), c("imp_num", "row_num"))
# add sampling weigths
long_imp_zr_relative_mob = merge(long_imp_zr_relative_mob, sw, on = "pid")
# adjusting variables
long_imp_zr_relative_mob[, last_wave_rev_health := factor(last_wave_rev_health)]
long_imp_zr_relative_mob[, last_wave_rev_health_c := fct_collapse(
    last_wave_rev_health, "1" = c("1", "2"), "2" = "3", "3" = "4", "4" = "5")]
long_imp_zr_relative_mob[, last_wave_good_health := ifelse(last_wave_rev_health %in% c(4, 5), 1, 0)]
long_imp_zr_relative_mob[, last_wave_rev_health := factor(last_wave_rev_health)]
long_imp_zr_relative_mob[, last_wave_smoking_number := as.integer(last_wave_smoking_number)]

# absolute
long_imp_zr_absolute_mob = data.table(mice::complete(imp_zr_absolute_mob, "long", include = FALSE))
setnames(long_imp_zr_absolute_mob, c(".imp", ".id"), c("imp_num", "row_num"))
# add sampling weigths
long_imp_zr_absolute_mob = merge(long_imp_zr_absolute_mob, sw, on = "pid")
# adjusting variables
long_imp_zr_absolute_mob[, last_wave_rev_health := factor(last_wave_rev_health)]
long_imp_zr_absolute_mob[, last_wave_rev_health_c := fct_collapse(
    last_wave_rev_health, "1" = c("1", "2"), "2" = "3", "3" = "4", "4" = "5")]
long_imp_zr_absolute_mob[, last_wave_good_health := ifelse(last_wave_rev_health %in% c(4, 5), 1, 0)]
long_imp_zr_absolute_mob[, last_wave_rev_health := factor(last_wave_rev_health)]
long_imp_zr_absolute_mob[, last_wave_smoking_number := as.integer(last_wave_smoking_number)]

# number of observations
N = length(unique(long_imp_zr_relative_mob$pid))

outcomes = c("last_wave_rev_health_c", "last_wave_bmi", "last_wave_depression",
             "last_wave_smoking", "last_wave_smoking_number")
model_types = c("ordinal", "gaussian", "gaussian", "binomial", "poisson")


# loop of unadjusted models

# z_relative_mob models
unadjusted_zr_relative_mob_results = unadjustedRegression(
    imputations = long_imp_zr_relative_mob,
    exposure_variable = "relative_mob_resid",
    exposure_type = "gaussian",
    id_var = "pid",
    time_var = "time",
    max_time_exposure = 20,
    outcome = outcomes,
    final_model_types = model_types,
    sampling_weight = "sweight",
    strata = "stratum",
    cluster = "cluster"
)

# z_absolute_mob models
unadjusted_zr_absolute_mob_results = unadjustedRegression(
    imputations = long_imp_zr_absolute_mob,
    exposure_variable = "absolute_mob_resid",
    exposure_type = "gaussian",
    id_var = "pid",
    time_var = "time",
    max_time_exposure = 20,
    outcome = outcomes,
    final_model_types = model_types,
    sampling_weight = "sweight",
    strata = "stratum",
    cluster = "cluster"
)

# z_gini models
unadjusted_zr_gini_results = unadjustedRegression(
    imputations = long_imp_zr_relative_mob,
    exposure_variable = "gini_resid",
    exposure_type = "gaussian",
    id_var = "pid",
    time_var = "time",
    max_time_exposure = 20,
    outcome = outcomes,
    final_model_types = model_types,
    sampling_weight = "sweight",
    strata = "stratum",
    cluster = "cluster"
)

# create table unadjusted models

# put model outputs in a list
list_rows = list(
    unadjusted_zr_relative_mob_results,
    unadjusted_zr_absolute_mob_results,
    unadjusted_zr_gini_results
)

row_names = c("average_relative_mob_resid",
              "average_absolute_mob_resid",
              "average_gini_resid")

row_labels = c("Rank-rank",
               "Upward mobility $\\times$ -1",
               "Gini"
)

column_names = c("Health status",
                 "BMI",
                 "Depression",
                 "Smoking",
                 "Number cigarettes smoked last month")

comment = longText("Each coefficient represents a model. Coefficients and standard errors are combined estimates from 20 multiple imputed datasets.
                   Analyses based on exposure from 1 to 20 years old.
                   We estimate different models depending on the outcome:
                   Ordinal regression (self-reported health), Generalized linear model (BMI, depression),
                   Logistic regression (smoking), Poisson regression (number of cigarettes smoked last month).
                   $^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$")

caption = "Unadjusted estimates of average residual exposure (continuous) \\newline on health indicators, PSID"
label = "tab:psid_unadjusted_zr_models"

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
    filename = "output/tables/psid_unadjusted_zr_models.tex",
    comment = comment,
    # groups = groups,
    observations = N
)

# adjusted models

# lagged and baseline variables
lag_vars = c("z_relative_mob", "z_gini", "z_absolute_mob",
             "relative_mob_resid", "gini_resid", "absolute_mob_resid",
             "log_population",
             "log_county_income", "z_prop_black", "famsize",
             "head_marital_status", "head_education",
             "head_owns_house", "head_working_binary")

baseline_vars = c("z_relative_mob", "z_gini", "z_absolute_mob",
                  "relative_mob_resid", "gini_resid", "absolute_mob_resid",
                  "log_population", "log_county_income", "z_prop_black",
                  "log_income_adj",
                  "famsize", "head_marital_status",
                  "head_education", "head_owns_house", "head_working_binary")

# create outcome models

# z_relative_mob

denominator_time1 = longText("
    male + white + as.factor(first_year) +
    weight_less_55 + mother_marital_status + mother_age + sweight
")

numerator = longText("
    (male + white +  as.factor(first_year) +
     weight_less_55 + mother_marital_status + mother_age +
     baseline_log_income_adj + baseline_famsize +
     lag_relative_mob_resid) + as.factor(time) + sweight
")

denominator = longText("
    (male + white + as.factor(first_year) +
     weight_less_55 + mother_marital_status + mother_age +
     baseline_log_income_adj + baseline_famsize +
     nmoves +
     lag_gini_resid + lag_log_population + lag_log_county_income +
     lag_z_prop_black + lag_famsize + lag_head_marital_status +
     lag_head_education + lag_head_owns_house + lag_head_working_binary +
     lag_relative_mob_resid) + as.factor(time) + sweight
")

predictors = longText(
    "average_relative_mob_resid + male + white + as.factor(first_year) +
     weight_less_55 + mother_marital_status + mother_age +
     baseline_log_income_adj + baseline_famsize
")


zr_relative_mob_results = ipwExposure(
    imputations = long_imp_zr_relative_mob,
    lag_variables = lag_vars,
    baseline_variables = baseline_vars,
    denominator_time1 = denominator_time1,
    numerator = numerator,
    denominator = denominator,
    exposure_variable = "relative_mob_resid",
    id_var = "pid",
    time_var = "time",
    max_time_exposure = 20,
    trim_p = 0.01,
    exposure_type = "gaussian",
    outcomes = outcomes,
    predictors = predictors,
    final_model_types = model_types,
    sampling_weight = "sweight",
    strata = "stratum",
    cluster = "cluster"
)

zr_relative_mob_results[[5]]

# z_absolute_mob
denominator_time1 = longText("
    male + white + as.factor(first_year) +
    weight_less_55 + mother_marital_status + mother_age + sweight
")

numerator = longText("
    (male + white + as.factor(first_year) +
     weight_less_55 + mother_marital_status + mother_age +
     baseline_log_income_adj + baseline_famsize +
     lag_absolute_mob_resid) + as.factor(time) + sweight
")

denominator = longText("
    (male + white + as.factor(first_year) +
     weight_less_55 + mother_marital_status + mother_age +
     baseline_log_income_adj + baseline_famsize +
     nmoves +
     lag_gini_resid + lag_log_population + lag_log_county_income +
     lag_z_prop_black + lag_famsize + lag_head_marital_status +
     lag_head_education + lag_head_owns_house + lag_head_working_binary +
     lag_absolute_mob_resid) + as.factor(time) + sweight
")

predictors = longText(
    "average_absolute_mob_resid + male + white + as.factor(first_year) +
     weight_less_55 + mother_marital_status + mother_age +
     baseline_log_income_adj + baseline_famsize
")


zr_absolute_mob_results = ipwExposure(
    imputations = long_imp_zr_absolute_mob,
    lag_variables = lag_vars,
    baseline_variables = baseline_vars,
    denominator_time1 = denominator_time1,
    numerator = numerator,
    denominator = denominator,
    exposure_variable = "absolute_mob_resid",
    id_var = "pid",
    time_var = "time",
    max_time_exposure = 20,
    trim_p = 0.01,
    exposure_type = "gaussian",
    outcomes = outcomes,
    predictors = predictors,
    final_model_types = model_types,
    sampling_weight = "sweight",
    strata = "stratum",
    cluster = "cluster"
)

# z_gini
denominator_time1 = longText("
    male + white + as.factor(first_year) +
    weight_less_55 + mother_marital_status + mother_age + sweight
")

numerator = longText("
    (male + white + as.factor(first_year) +
     weight_less_55 + mother_marital_status + mother_age +
     baseline_log_income_adj + baseline_famsize +
     lag_gini_resid) + as.factor(time) + sweight
")

denominator = longText("
    (male + white + as.factor(first_year) +
     weight_less_55 + mother_marital_status + mother_age +
     baseline_log_income_adj + baseline_famsize +
     nmoves +
     lag_relative_mob_resid + lag_log_population + lag_log_county_income +
     lag_z_prop_black + lag_famsize + lag_head_marital_status +
     lag_head_education + lag_head_owns_house + lag_head_working_binary +
     lag_gini_resid) + as.factor(time) + sweight
")

predictors = longText(
    "average_gini_resid + male + white + as.factor(first_year) +
     weight_less_55 + mother_marital_status + mother_age +
     baseline_log_income_adj + baseline_famsize
")

zr_gini_results = ipwExposure(
    imputations = long_imp_zr_relative_mob,
    lag_variables = lag_vars,
    baseline_variables = baseline_vars,
    denominator_time1 = denominator_time1,
    numerator = numerator,
    denominator = denominator,
    exposure_variable = "gini_resid",
    id_var = "pid",
    time_var = "time",
    max_time_exposure = 20,
    trim_p = 0.01,
    exposure_type = "gaussian",
    outcomes = outcomes,
    predictors = predictors,
    final_model_types = model_types,
    sampling_weight = "sweight",
    strata = "stratum",
    cluster = "cluster"
)

# create table adjusted models
list_rows = list(zr_relative_mob_results,
                 zr_absolute_mob_results,
                 zr_gini_results
)

row_names = c("average_relative_mob_resid",
              "average_absolute_mob_resid",
              "average_gini_resid"
)

column_names = c("Health status",
                 "BMI",
                 "Depression",
                 "Smoking",
                 "Number cigarettes smoked last month")

comment = longText("Each coefficient represents a model. Coefficients and standard errors are combined estimates from 20 multiple imputed datasets.
                   Analyses based on exposure from 1 to 20 years old.
                   We estimate different models depending on the outcome:
                   Ordinal regression (self-reported health), Generalized linear model (BMI, depression),
                   Logistic regression (smoking), Poisson regression (number of cigarettes smoked last month).
                   $^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$")

caption = "Adjusted estimates of average residual exposure (continuous) \\newline on health indicators, PSID"
label = "tab:psid_adjusted_zr_models"

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
    filename = "output/tables/psid_adjusted_zr_models.tex",
    comment = comment,
    # groups = groups,
    observations = N
)

# create weight tables
list_weights = list(
    zr_relative_mob_results[["weights"]],
    zr_absolute_mob_results[["weights"]],
    zr_gini_results[["weights"]]
)

tableWeights(list_weights,
             model_names = c("Rank-rank correlation",
                             "Upward mobility",
                             "Gini"),
             caption  = "Stabilized treatment weights, continuous residual exposure",
             label = "tab:psid_ipt_weigths_z",
             comment = "Analyses based on exposure from 1 to 20 years old. ",
             filename = "output/tables/psid_ipt_weights_zr.tex",
             tabcolsep = 10,
             arraystretch = 1)

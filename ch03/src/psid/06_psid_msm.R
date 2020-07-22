##############################
# county income mobility and individual health
# MSM with PSID data
# author: sebastian daza
##############################


library(survey)
options(survey.lonely.psu="adjust")
library(data.table)
library(hash)
library(texreg)
library(mice)
library(mitools)
library(MASS)
source("src/utils.R")

# slackr::slackr_setup(config_file = ".slackr")

# z = z-score
# r = residuals

# read imputed data
datasets = list(
    imp_z_relative_mob = readRDS('output/data/psid_z_relative_mob_imp.rds'),
    imp_z_absolute_mob = readRDS('output/data/psid_z_absolute_mob_imp.rds'),
    imp_zr_relative_mob = readRDS('output/data/psid_zr_relative_mob_imp.rds'),
    imp_zr_absolute_mob = readRDS('output/data/psid_zr_absolute_mob_imp.rds'),
    imp_q_relative_mob = readRDS('output/data/psid_q_relative_mob_imp.rds'),
    imp_q_absolute_mob = readRDS('output/data/psid_q_absolute_mob_imp.rds'),
    imp_qr_relative_mob = readRDS('output/data/psid_qr_relative_mob_imp.rds'),
    imp_qr_absolute_mob = readRDS('output/data/psid_qr_absolute_mob_imp.rds')
)

# transform imputation data
for (i in seq_along(datasets)) {
    print(paste0(":::: transforming dataset ", i, " ::::"))
    temp = data.table(mice::complete(datasets[[i]], "long", include = FALSE))
    setnames(temp, c(".imp", ".id"), c("imp_num", "row_num"))
    # other variables
    mean_bmi = attr(temp$last_wave_bmi,"scaled:center")
    temp[, obesity := ifelse((last_wave_bmi + mean_bmi) >= 30, 1, 0)]
    temp[, good_health := ifelse(last_wave_rev_health %in% c(4, 5), 1, 0)]
    temp[, last_wave_rev_health := factor(last_wave_rev_health)]
    # save long format
    datasets[[i]] = temp
    rm(temp)
}

# outcomes
exposure_variables = hash(
    "z_relative_mob" = "imp_z_relative_mob",
    "z_absolute_mob" = "imp_z_absolute_mob",
    "z_gini" = "imp_z_relative_mob",
    "relative_mob_resid" = "imp_zr_relative_mob",
    "absolute_mob_resid" = "imp_zr_absolute_mob",
    "gini_resid" = "imp_zr_relative_mob" ,
    "q_relative_mob" = "imp_q_relative_mob",
    "q_absolute_mob" = "imp_q_absolute_mob",
    "q_gini" = "imp_q_relative_mob",
    "q_relative_mob_resid" = "imp_qr_relative_mob",
    "q_absolute_mob_resid" = "imp_qr_absolute_mob",
    "q_gini_resid" = "imp_qr_relative_mob"
)

exposure_types = hash(
    "z_relative_mob" = "gaussian",
    "z_absolute_mob" = "gaussian",
    "z_gini" = "gaussian",
    "relative_mob_resid" = "gaussian",
    "absolute_mob_resid" = "gaussian",
    "gini_resid" = "gaussian" ,
    "q_relative_mob" = "ordinal",
    "q_absolute_mob" = "ordinal",
    "q_gini" = "ordinal",
    "q_relative_mob_resid" = "ordinal",
    "q_absolute_mob_resid" = "ordinal",
    "q_gini_resid" = "ordinal"
)

outcomes = c("last_wave_rev_health", "last_wave_bmi", "last_wave_depression",
     "last_wave_smoking", "last_wave_smoking_number")
model_types = c("ordinal", "gaussian", "gaussian", "binomial", "poisson")

# unadjusted models
unadjusted_models = list()
for (i in seq_along(exposure_variables)) {
    exposure_name = keys(exposure_variables)[i]
    print(paste0(":::: Running model for ", exposure_name))
    name = paste0("unadjusted_", exposure_name , "_results")
    unadjusted_models[[name]] = unadjustedRegression(
        imputations = datasets[[values(exposure_variables[exposure_name])]],
        exposure_variable = exposure_name,
        exposure_type = values(exposure_types[exposure_name]),
        id_var = "pid",
        time_var = "time",
        max_time_exposure = 20,
        outcomes = outcomes,
        final_model_types = model_types,
        sampling_weight = "sweight",
        cluster = "cluster",
        strata = "stratum"
    )
}

saveRDS(unadjusted_models, "output/data/psid_unadjusted_models.rds")

# adjusted models

# lagged and baseline variables
lag_vars = c("z_relative_mob", "z_absolute_mob", "z_gini",
    "relative_mob_resid", "absolute_mob_resid",
    "gini_resid", "q_relative_mob_resid",
    "q_absolute_mob_resid", "q_gini_resid",
    "q_relative_mob", "q_absolute_mob", "q_gini", 
    "log_population", "log_county_income", "z_prop_black", 
    "log_income_adj", "famsize", "head_marital_status", "head_education",
    "head_owns_house", "head_working_binary")

baseline_vars = c("z_relative_mob", "z_absolute_mob", "z_gini",
    "relative_mob_resid", "absolute_mob_resid",
    "gini_resid", "q_relative_mob_resid",
    "q_absolute_mob_resid", "q_gini_resid",
    "q_relative_mob", "q_absolute_mob", "q_gini",
    "log_population", "log_county_income", "z_prop_black", 
    "log_income_adj", "famsize", "head_marital_status", 
    "head_education", "head_owns_house", "head_working_binary")

denominator_time1 = longText("
    male + as.factor(race) + as.factor(first_year) +
    weight_less_55 + mother_marital_status + mother_age + log_income_adj + sweight
")

gini_variables = hash(
    "z_relative_mob" = "z_gini",
    "z_absolute_mob" = "z_gini",
    "z_gini" = "z_relative_mob",
    "relative_mob_resid" = "gini_resid",
    "absolute_mob_resid" = "gini_resid",
    "gini_resid" = "relative_mob_resid",
    "q_relative_mob" = "q_gini",
    "q_absolute_mob" = "q_gini",
    "q_gini" = "q_relative_mob",
    "q_relative_mob_resid" = "q_gini_resid",
    "q_absolute_mob_resid" = "q_gini_resid",
    "q_gini_resid" = "q_relative_mob_resid"
)

adjusted_models = list()
for (i in seq_along(exposure_variables)) {

    exposure_name = keys(exposure_variables)[i]
    print(paste0(":::: Running model for ", exposure_name))
    gini_name = values(gini_variables[exposure_name])
    name = paste0("adjusted_", exposure_name, "_results")

    numerator = longText(paste0("
        (male + as.factor(race) + as.factor(first_year) +
        weight_less_55 + mother_marital_status + mother_age +
        baseline_log_population + baseline_log_county_income +
        baseline_z_prop_black + baseline_log_income_adj +
        baseline_famsize + baseline_head_marital_status + baseline_head_education +
        baseline_head_owns_house + baseline_head_working_binary +
        baseline_", exposure_name, " + baseline_", gini_name, " + 
        lag_", exposure_name, ") + as.factor(time) + sweight")
    )

    denominator = longText(paste0("
        (male + as.factor(race) + as.factor(first_year) +
        weight_less_55 + mother_marital_status + mother_age +
        baseline_log_population + baseline_log_county_income +
        baseline_z_prop_black + baseline_log_income_adj +
        baseline_famsize + baseline_head_marital_status + baseline_head_education +
        baseline_head_owns_house + baseline_head_working_binary +
        baseline_", exposure_name, " + baseline_", gini_name, " +
        nmoves +  lag_log_population + lag_log_county_income +
        lag_z_prop_black + lag_log_income_adj +
        lag_famsize + lag_head_marital_status + lag_head_education +
        lag_head_owns_house + lag_head_working_binary +
        lag_", exposure_name, " + lag_", gini_name, ")
        + as.factor(time) + sweight")
    )

    predictors = longText(paste0("
        average_", exposure_name, " + male + as.factor(race) +
        as.factor(first_year) + weight_less_55 + mother_marital_status + mother_age +
        baseline_log_population + baseline_log_county_income +
        baseline_z_prop_black + baseline_log_income_adj +
        baseline_famsize + baseline_head_marital_status +
        baseline_head_education + baseline_head_owns_house +
        baseline_head_working_binary + baseline_", gini_name)
    )

    adjusted_models[[name]] = ipwExposure(
        imputations = datasets[[values(exposure_variables[exposure_name])]],
        lag_variables = lag_vars,
        baseline_variables = baseline_vars,
        denominator_time1 = denominator_time1,
        numerator = numerator,
        denominator = denominator,
        exposure_variable = exposure_name,
        exposure_type = values(exposure_types[exposure_name]),
        id_var = "pid",
        time_var = "time",
        max_time_exposure = 20,
        trim_p = 0.01,
        outcomes = outcomes,
        predictors = predictors,
        final_model_types = model_types,
        sampling_weight = "sweight",
        cluster = "cluster",
        strata = "stratum"
    )
}

saveRDS(adjusted_models, "output/data/psid_adjusted_models.rds")
##############################
# county income mobility and individual health
# MSM with NLSY97 data
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

slackr::slackr_setup(config_file = ".slackr")

# z = z-score
# r = residuals

# read imputed data
datasets = list(
    imp_z_relative_mob = readRDS('output/data/nlsy97_z_relative_mob_imp.rds'),
    imp_z_absolute_mob = readRDS('output/data/nlsy97_z_absolute_mob_imp.rds'),
    imp_zr_relative_mob = readRDS('output/data/nlsy97_zr_relative_mob_imp.rds'),
    imp_zr_absolute_mob = readRDS('output/data/nlsy97_zr_absolute_mob_imp.rds'),
    imp_q_relative_mob = readRDS('output/data/nlsy97_q_relative_mob_imp.rds'),
    imp_q_absolute_mob = readRDS('output/data/nlsy97_q_absolute_mob_imp.rds'),
    imp_qr_relative_mob = readRDS('output/data/nlsy97_qr_relative_mob_imp.rds'),
    imp_qr_absolute_mob = readRDS('output/data/nlsy97_qr_absolute_mob_imp.rds')
)

# transform imputation data
for (i in seq_along(datasets)) {
    print(paste0(":::: transforming dataset ", i, " ::::"))
    temp = data.table(mice::complete(datasets[[i]], "long", include = FALSE))
    setnames(temp, c(".imp", ".id"), c("imp_num", "row_num"))
    mean_bmi = attr(temp$bmi,"scaled:center")
    temp[, obesity := ifelse((bmi + mean_bmi) >= 30, 1, 0)]
    # other variables
    temp[, age_interview_est := as.numeric(as.character(age_interview_est))]
    temp[, good_health := ifelse(rev_health %in% c(4, 5), 1, 0)]
    temp[, rev_health := factor(rev_health)]
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

outcomes = c("rev_health", "bmi", "depression", "smoking", "smoking_days")
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
        id_var = "id",
        time_var = "stime",
        max_time_exposure = 8,
        outcomes = outcomes,
        final_model_types = model_types,
        sampling_weight = "sweight",
        cluster = "cluster",
        strata = "stratum"
    )
}

slackr::text_slackr(paste0("NSLY97 unadjusted models finished at ", Sys.time()))
saveRDS(unadjusted_models, "output/data/nlsy97_unadjusted_models.rds")

# adjusted models

# lagged and baseline variables
lag_vars = c("imp_parent_employed", "imp_parent_married", "hhsize",
    "bmi", "rev_health", "smoking", "smoking_days", "log_population",
    "log_county_income", "z_prop_black",
    "z_relative_mob", "z_gini", "z_absolute_mob",
    "relative_mob_resid", "absolute_mob_resid", "gini_resid",
    "q_relative_mob", "q_absolute_mob", "q_gini",
    "q_relative_mob_resid", "q_absolute_mob_resid", "q_gini_resid"
)

baseline_vars = c("z_relative_mob", "z_gini", "z_absolute_mob",
    "relative_mob_resid", "absolute_mob_resid", "gini_resid",
    "q_relative_mob", "q_absolute_mob", "q_gini",
    "q_relative_mob_resid", "q_absolute_mob_resid", "q_gini_resid",
    "imp_parent_employed",
    "imp_parent_married", "hhsize", "bmi", "rev_health",
    "smoking", "smoking_days", "log_population",
    "log_county_income", "z_prop_black", "log_income_adj")


denominator_time1 = longText("
    male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 + log_income_adj + sweight
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
        (male + ethnicity + as.factor(max_age_interview_est) +
        parent_education + asvab_score + mother_age_at_birth +
        residential_moves_by_12 +
        baseline_log_income_adj + baseline_hhsize +
        baseline_imp_parent_employed + baseline_imp_parent_employed +
        baseline_rev_health + baseline_bmi + baseline_smoking +
        baseline_", exposure_name, " + baseline_", gini_name, " + baseline_log_population +
        baseline_log_county_income + baseline_z_prop_black +
        lag_", exposure_name, ") + as.factor(stime) + sweight")
    )

    denominator = longText(paste0("
        (male + ethnicity + as.factor(max_age_interview_est) +
        parent_education + asvab_score + mother_age_at_birth +
        residential_moves_by_12 +
        baseline_log_income_adj + baseline_hhsize +
        baseline_imp_parent_employed + baseline_imp_parent_employed +
        baseline_rev_health + baseline_smoking + baseline_bmi +
        baseline_", exposure_name, " + baseline_", gini_name, " + baseline_log_population +
        baseline_log_county_income + baseline_z_prop_black +
        nmoves + lag_imp_parent_employed + lag_imp_parent_married + lag_hhsize +
        log_income_adj + lag_rev_health + lag_bmi + lag_smoking +
        lag_", exposure_name, " + lag_", gini_name, " + lag_log_county_income +
        lag_log_population + lag_z_prop_black) + as.factor(stime) + sweight")
    )

    predictors = longText(paste0(
        "average_", exposure_name, " + male + ethnicity +
        as.factor(max_age_interview_est) +
        parent_education + asvab_score + mother_age_at_birth +
        residential_moves_by_12 +
        baseline_log_income_adj + baseline_hhsize +
        baseline_imp_parent_employed + baseline_imp_parent_married +
        baseline_", gini_name, " + baseline_log_population +
        baseline_log_county_income + baseline_z_prop_black")
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
        id_var = "id",
        time_var = "stime",
        max_time_exposure = 8,
        trim_p = 0.01,
        outcomes = outcomes,
        predictors = predictors,
        final_model_types = model_types,
        sampling_weight = "sweight",
        cluster = "cluster",
        strata = "stratum"
    )
}

saveRDS(adjusted_models, "output/data/nlsy97_adjusted_models.rds")
slackr::text_slackr(paste0("NSLY97 adjusted models finished at ", Sys.time()))

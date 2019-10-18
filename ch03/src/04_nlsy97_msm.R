###################################
# individual income mobility paper
# nlsy97 marginal structural models
# author: sebastian daza
# version: 0.01
###################################

library(survey)
library(mitools)

imp = readRDS('ch03/output/data/nlsy97_imputation_individual.rds')

rel_mob_coeff_bmi = list()
rel_mob_vcov_bmi = list()
rel_mob_coeff_depression = list()
rel_mob_vcov_depression = list()
rel_mob_coeff_health = list()
rel_mob_vcov_health = list()
rel_mob_coeff_smoking = list()
rel_mob_vcov_smoking = list()
rel_mob_coeff_smoking30 = list()
rel_mob_vcov_smoking30 = list()

gini_coeff_bmi = list()
gini_vcov_bmi = list()
gini_coeff_depression = list()
gini_vcov_depression = list()
gini_coeff_health = list()
gini_vcov_health = list()
gini_coeff_smoking = list()
gini_vcov_smoking = list()
gini_coeff_smoking30 = list()
gini_vcov_smoking30 = list()

# relative mobility
for (i in 1:imp$m) {

    print(paste0("::::: imputation: ", i, " :::::"))
    dat = data.table(complete(imp, i))
    setorder(dat, id, year)
    dat[, cum_smoking := cumsum(smoking_ever), id][,
        fsmoking_ever := ifelse(cum_smoking > 0, 1, 0)]

    # create lag variables
    lag_vars = c("imp_parent_employed", "imp_parent_married", "hhsize",
                 "bmi", "health", "smoking_ever", "smoking_30", "log_population",
                 "log_county_income", "z_relative_mob", "z_gini")
    dat[, paste0("lag_", lag_vars) := lapply(.SD, shift), id,
        .SDcol = lag_vars]

    dat[id == 1, .(id, bmi, lag_bmi)]

    # impute first value backwards
    dat[, paste0("lag_", lag_vars) := lapply(.SD, impute_locf), id,
        .SDcol = paste0("lag_", lag_vars)]

    # exposure variables
    dat[age_interview_est <= 20, total_exposure_time := sum(exposure_time), id]
    dat[age_interview_est <= 20,
        z_relative_mob_exposure := sum(z_relative_mob * exposure_time) / sum(exposure_time), id]
    dat[age_interview_est <= 20,
        z_gini_exposure := sum(z_gini * exposure_time) / sum(exposure_time), id]

    dat[, z_relative_mob_exposure := getMax(z_relative_mob_exposure), id]
    dat[, z_gini_exposure := getMax(z_gini_exposure), id]
    dat[, max_age_interview_est := factor(max_age_interview_est)]
    dat[, health := factor(health)]
    dat[, lag_health := factor(lag_health)]

    last_wave = dat[year == 2015]
    dat = dat[age_interview_est <= 20]
    setorder(dat, id, year)
    dat[, time := 1:.N, id]
    dat[, cyear := year - mean(year)]

    temp_relative_mob = ipwtm(
        exposure = z_relative_mob,
        family = "gaussian",
        numerator   = ~ male + ethnicity + max_age_interview_est +
                        parent_education + asvab_score + mother_age_at_birth +
                        residential_moves_by_12,
        denominator = ~ male + ethnicity + max_age_interview_est +
                        parent_education + asvab_score + mother_age_at_birth +
                        residential_moves_by_12 +
                        lag_imp_parent_employed + lag_imp_parent_married + lag_hhsize +
                        lag_bmi + lag_health + lag_smoking_ever + lag_smoking_30 +
                        log_income_adj + cyear +
                        lag_z_gini + lag_log_population + lag_log_county_income,
        timevar = time,
        type = "all",
        corstr = "ar1",
        id = id,
        data = dat
    )

    temp_gini = ipwtm(
        exposure = z_gini,
        family = "gaussian",
        numerator   = ~ male + ethnicity + max_age_interview_est +
                        parent_education + asvab_score + mother_age_at_birth +
                        residential_moves_by_12,
        denominator = ~ male + ethnicity + max_age_interview_est +
                        parent_education + asvab_score + mother_age_at_birth +
                        residential_moves_by_12 +
                        lag_imp_parent_employed + lag_imp_parent_married + lag_hhsize +
                        lag_bmi + lag_health + lag_smoking_ever + lag_smoking_30 +
                        log_income_adj + cyear +
                        lag_z_gini + lag_log_population + lag_log_county_income,
        timevar = time,
        type = "all",
        corstr = "ar1",
        id = id,
        data = dat
    )

    dat[, ipw_rel_mob := temp_relative_mob$ipw.weights]
    dat[, ipw_gini := temp_gini$ipw.weights]
    dat[, max_year := getMax(year), id]

    sdat = dat[year == max_year, .SD, .SDcol = names(dat) %like% "^id|^ipw"]
    fwave = merge(last_wave, sdat, on = "id")

    svy_design_rel_mob = svydesign(ids = ~ 1, weights = ~ ipw_rel_mob, data = fwave)
    svy_design_gini = svydesign(ids = ~ 1, weights = ~ ipw_gini, data = fwave)

    # bmi
    msm_rel_mob_bmi = svyglm(bmi ~ z_relative_mob_exposure +
                             male + ethnicity + max_age_interview_est +
                             parent_education + asvab_score + mother_age_at_birth +
                             residential_moves_by_12,
                             design = svy_design_rel_mob)

    rel_mob_coeff_bmi = c(rel_mob_coeff_bmi, list(coefficients(msm_rel_mob_bmi)))
    rel_mob_vcov_bmi = c(rel_mob_vcov_bmi, list(vcov(msm_rel_mob_bmi)))

    msm_gini_bmi = svyglm(bmi ~ z_gini_exposure +
                          male + ethnicity + max_age_interview_est +
                          parent_education + asvab_score + mother_age_at_birth +
                          residential_moves_by_12,
                          design = svy_design_gini)

    gini_coeff_bmi = c(gini_coeff_bmi, list(coefficients(msm_gini_bmi)))
    gini_vcov_bmi = c(gini_vcov_bmi, list(vcov(msm_gini_bmi)))

    # depression
    msm_rel_mob_depression = svyglm(depression ~ z_relative_mob_exposure +
                                    male + ethnicity + max_age_interview_est +
                                    parent_education + asvab_score + mother_age_at_birth +
                                    residential_moves_by_12,
                                    design = svy_design_rel_mob)

    rel_mob_coeff_depression = c(rel_mob_coeff_depression, list(coefficients(msm_rel_mob_depression)))
    rel_mob_vcov_depression = c(rel_mob_vcov_depression, list(vcov(msm_rel_mob_depression)))

    msm_gini_depression = svyglm(depression  ~ z_gini_exposure +
                                 male + ethnicity + max_age_interview_est +
                                 parent_education + asvab_score + mother_age_at_birth +
                                 residential_moves_by_12,
                                 design = svy_design_gini)

    gini_coeff_depression = c(gini_coeff_depression, list(coefficients(msm_gini_depression)))
    gini_vcov_depression = c(gini_vcov_depression, list(vcov(msm_gini_depression)))

    msm_rel_mob_health = svyolr(health ~ z_relative_mob_exposure +
                        male + ethnicity + max_age_interview_est +
                        parent_education + asvab_score + mother_age_at_birth +
                        residential_moves_by_12,
                        design = svy_design_rel_mob)

    rel_mob_coeff_health = c(rel_mob_coeff_health, list(coefficients(msm_rel_mob_health)))
    rel_mob_vcov_health = c(rel_mob_vcov_health, list(vcov(msm_rel_mob_health)))


    msm_gini_health = svyolr(health ~ z_relative_mob_exposure +
                        male + ethnicity + max_age_interview_est +
                        parent_education + asvab_score + mother_age_at_birth +
                        residential_moves_by_12,
                        design = svy_design_gini)

    gini_coeff_health = c(gini_coeff_health, list(coefficients(msm_gini_health)))
    gini_vcov_health = c(gini_vcov_health, list(vcov(msm_gini_health)))

    msm_smoking = svyglm(smoking_ever ~  z_relative_mob_exposure +
                     male + ethnicity + max_age_interview_est +
                     parent_education + asvab_score + mother_age_at_birth +
                     residential_moves_by_12,
                     design = sdesign,
                     family = quasibinomial
                     )

    rel_mob_coeff_smoking = c(rel_mob_coeff_smoking, list(coefficients(msm_smoking)))
    rel_mob_vcov_smoking = c(rel_mob_vcov_smoking, list(vcov(msm_smoking)))

    msm_smoking30 = svyglm(smoking_30 ~  z_relative_mob_exposure +
                     male + ethnicity + max_age_interview_est +
                     parent_education + asvab_score + mother_age_at_birth +
                     residential_moves_by_12,
                     design = sdesign,
                     family = poisson
                     )

    rel_mob_coeff_smoking30 = c(rel_mob_coeff_smoking30, list(coefficients(msm_smoking30)))
    rel_mob_vcov_smoking30 = c(rel_mob_vcov_smoking30, list(vcov(msm_smoking30)))

}

rel_mob_results_bmi =  MIcombine(rel_mob_coeff_bmi, rel_mob_vcov_bmi)

rel_mob_results_depression =  MIcombine(rel_mob_coeff_depression, rel_mob_vcov_depression)
rel_mob_results_health =  MIcombine(rel_mob_coeff_health, rel_mob_vcov_health)
rel_mob_results_smoking =  MIcombine(rel_mob_coeff_smoking, rel_mob_vcov_smoking)
rel_mob_results_smoking30 =  MIcombine(rel_mob_coeff_smoking30, rel_mob_vcov_smoking30)

rel_mob_results_bmi
rel_mob_results_depression
rel_mob_results_health
rel_mob_results_smoking
rel_mob_results_smoking30
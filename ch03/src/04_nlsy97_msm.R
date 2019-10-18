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

for (i in 1:imp$m) {

    print(paste0("::::: imputation: ", i, " :::::"))
    dat = data.table(complete(imp, i))
    setorder(dat, id, year)
    dat[, cum_smoking := cumsum(smoking_ever), id][,
        fsmoking_ever := ifelse(cum_smoking > 0, 1, 0)]

    dat[age_interview_est <= 20, total_exposure_time := sum(exposure_time), id]
    dat[age_interview_est <= 20,
        z_relative_mob_exposure := sum(z_relative_mob * exposure_time) / sum(exposure_time), id]
    dat[, z_relative_mob_exposure := getMax(z_relative_mob_exposure), id]
    dat[, max_age_interview_est := factor(max_age_interview_est)]
    dat[, health := factor(health)]

    last_wave = dat[year == 2015]
    dat = dat[age_interview_est <= 20]
    setorder(dat, id, year)
    dat[, time := 1:.N, id]
    dat[, cyear := year - mean(year)]

    temp = ipwtm(exposure = z_relative_mob,
                 family = "gaussian",
                 numerator   = ~ male + ethnicity + max_age_interview_est +
                                 parent_education + asvab_score + mother_age_at_birth +
                                 residential_moves_by_12,
                 denominator = ~ male + ethnicity + + max_age_interview_est +
                                 parent_education + asvab_score + mother_age_at_birth +
                                 residential_moves_by_12 +
                                 imp_parent_employed + imp_parent_married + hhsize +
                                 bmi + health +  smoking_ever + smoking_30 +
                                 log_income_adj + cyear,
                 timevar = time,
                 type = "all",
                 corstr = "ar1",
                 id = id,
                 data = dat)

    summary(temp$ipw.weights)
    dat[, ipw := temp$ipw.weights]
    dat[, max_year := getMax(year), id]
    sdat = dat[year == max_year, .(id, ipw)]
    fwave = merge(last_wave, sdat, on = "id")

    sdesign = svydesign(ids = ~ 1, weights = ~ ipw, data = fwave)

    msm_bmi = svyglm(bmi ~  z_relative_mob_exposure +
                     male + ethnicity + max_age_interview_est +
                     parent_education + asvab_score + mother_age_at_birth +
                     residential_moves_by_12,
                     design = sdesign)

    rel_mob_coeff_bmi = c(rel_mob_coeff_bmi, list(coefficients(msm_bmi)))
    rel_mob_vcov_bmi = c(rel_mob_vcov_bmi, list(vcov(msm_bmi)))

    msm_depression = svyglm(depression ~  z_relative_mob_exposure +
                     male + ethnicity + max_age_interview_est +
                     parent_education + asvab_score + mother_age_at_birth +
                     residential_moves_by_12,
                     design = sdesign)

    rel_mob_coeff_depression = c(rel_mob_coeff_depression, list(coefficients(msm_depression)))
    rel_mob_vcov_depression = c(rel_mob_vcov_depression, list(vcov(msm_depression)))

    msm_health = svyolr(health ~ z_relative_mob_exposure +
                     male + ethnicity + max_age_interview_est +
                     parent_education + asvab_score + mother_age_at_birth +
                     residential_moves_by_12,
                     design = sdesign)

    rel_mob_coeff_health = c(rel_mob_coeff_health, list(coefficients(msm_health)))
    rel_mob_vcov_health = c(rel_mob_vcov_health, list(vcov(msm_health)))

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

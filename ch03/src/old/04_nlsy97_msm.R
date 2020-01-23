###################################
# county income mobility and individual health
# nlsy97 marginal structural models
# author: sebastian daza
###################################

library(survey)
library(mice)
library(mitools)
library(ipw)
library(texreg)
library(data.table)
library(forcats)
library(foreach)
library(doParallel)

source("ch03/src/utils.R")

# relative mobility (continuous)

imp = readRDS('ch03/output/data/nlsy97_relative_mob_imputation.rds')

# examine solution
dat = data.table(mice::complete(imp, 1))

# age distribution looks fine
table(dat$stime)
table(dat[stime == 8, age_interview_est])

names(dat)

sdazar::countmis(dat)

setorder(dat, id, stime)

lag_vars = c("imp_parent_employed", "imp_parent_married", "hhsize",
             "bmi", "health", "smoking_ever", "smoking_30", "log_population",
             "log_county_income", "z_relative_mob", "z_gini")

dat[, paste0("lag_", lag_vars) := lapply(.SD, shift), id,
    .SDcol = lag_vars]


stopCluster(cl)
cl = makeCluster(10)
registerDoParallel(cl)

results = foreach(i = 1:imp$m,
                  .combine = append,
                  .packages = c("data.table", "ipw",
                                "survey", "forcats")) %dopar% {

    dat = data.table(mice::complete(imp, i))
    setorder(dat, id, year)

    # create lag variables
    lag_vars = c("imp_parent_employed", "imp_parent_married", "hhsize",
                 "bmi", "health", "smoking_ever", "smoking_30", "log_population",
                 "log_county_income", "z_relative_mob", "z_gini")
    dat[, paste0("lag_", lag_vars) := lapply(.SD, shift), id,
        .SDcol = lag_vars]

    # impute first value backwards
    dat[, paste0("lag_", lag_vars) := lapply(.SD, impute_locf), id,
        .SDcol = paste0("lag_", lag_vars)]

    # exposure variables
    dat[as.numeric(age_interview_est) <= 20, total_exposure_time :=
        sum(exposure_time), id]
    dat[as.numeric(age_interview_est) <= 20,
        z_relative_mob_exposure :=
        sum(z_relative_mob * exposure_time) / sum(exposure_time), id]
    dat[as.numeric(age_interview_est) <= 20,
        z_gini_exposure := sum(z_gini * exposure_time) / sum(exposure_time), id]

    # dat[as.numeric(age_interview_est) < as.numeric(max_age_interview_est), total_exposure_time := sum(exposure_time), id]
    # dat[as.numeric(age_interview_est) < as.numeric(max_age_interview_est),
    #     z_relative_mob_exposure := sum(z_relative_mob * exposure_time) / sum(exposure_time), id]
    # dat[as.numeric(age_interview_est) < as.numeric(max_age_interview_est),
    #     z_gini_exposure := sum(z_gini * exposure_time) / sum(exposure_time), id]

    dat[, z_relative_mob_exposure := getMax(z_relative_mob_exposure), id]
    dat[, z_gini_exposure := getMax(z_gini_exposure), id]
    dat[, max_age_interview_est := factor(max_age_interview_est)]
    dat[, health := factor(health)]
    dat[, lag_health := factor(lag_health)]

    # last wave and exposure datasets
    last_wave = dat[year == 2015]
    dat = dat[as.numeric(age_interview_est) <= 20]
    # dat = dat[as.numeric(age_interview_est) < as.numeric(max_age_interview_est)]

    dat[, time := 1:.N, id]
    dat[, cyear := year - mean(year)]
    dat[, max_age_interview_est := fct_drop(max_age_interview_est)]

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
                        log_income_adj + cyear,
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
                        log_income_adj + cyear,
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

    rel_mob_bmi = list(
                       coeff = coefficients(msm_rel_mob_bmi),
                       vcov = vcov(msm_rel_mob_bmi)
                       )

    msm_gini_bmi = svyglm(bmi ~ z_gini_exposure +
                          male + ethnicity + max_age_interview_est +
                          parent_education + asvab_score + mother_age_at_birth +
                          residential_moves_by_12,
                          design = svy_design_gini)

    gini_bmi = list(
                    coeff = coefficients(msm_gini_bmi),
                    vcov = vcov(msm_gini_bmi)
                    )

    # depression
    msm_rel_mob_depression = svyglm(depression ~ z_relative_mob_exposure +
                                    male + ethnicity + max_age_interview_est +
                                    parent_education + asvab_score + mother_age_at_birth +
                                    residential_moves_by_12,
                                    design = svy_design_rel_mob)

    rel_mob_depression = list(
                              coeff = coefficients(msm_rel_mob_depression),
                              vcov = vcov(msm_rel_mob_depression)
                              )

    msm_gini_depression = svyglm(depression  ~ z_gini_exposure +
                                 male + ethnicity + max_age_interview_est +
                                 parent_education + asvab_score + mother_age_at_birth +
                                 residential_moves_by_12,
                                 design = svy_design_gini)

    gini_depression = list(
                           coeff = coefficients(msm_gini_depression),
                           vcov = vcov(msm_gini_depression)
                           )

    # health
    msm_rel_mob_health = svyolr(health ~ z_relative_mob_exposure +
                                male + ethnicity + max_age_interview_est +
                                parent_education + asvab_score + mother_age_at_birth +
                                residential_moves_by_12,
                                design = svy_design_rel_mob)

    rel_mob_health =  list(
                           coeff = coefficients(msm_rel_mob_health),
                           vcov = vcov(msm_rel_mob_health)
                           )

    msm_gini_health = svyolr(health ~ z_gini_exposure +
                             male + ethnicity + max_age_interview_est +
                             parent_education + asvab_score + mother_age_at_birth +
                             residential_moves_by_12,
                             design = svy_design_gini)

    gini_health = list(
                       coeff = coefficients(msm_gini_health),
                       vcov = vcov(msm_gini_health)
                       )


    # smoking
    msm_rel_mob_smoking = svyglm(smoking_ever ~ z_relative_mob_exposure +
                                 male + ethnicity + max_age_interview_est +
                                 parent_education + asvab_score + mother_age_at_birth +
                                 residential_moves_by_12,
                                 design = svy_design_rel_mob,
                                 family = quasibinomial
                                 )

    rel_mob_smoking = list(
                           coeff = coefficients(msm_rel_mob_smoking),
                           vcov = vcov(msm_rel_mob_smoking)
                           )

    msm_gini_smoking = svyglm(smoking_ever ~ z_gini_exposure +
                              male + ethnicity + max_age_interview_est +
                              parent_education + asvab_score + mother_age_at_birth +
                              residential_moves_by_12,
                              design = svy_design_gini,
                              family = quasibinomial
                              )

    gini_smoking = list(
                        coeff = coefficients(msm_gini_smoking),
                        vcov = vcov(msm_gini_smoking)
                        )

    # smoking 30
    msm_rel_mob_smoking30 = svyglm(smoking_30 ~ z_relative_mob_exposure +
                                   male + ethnicity + max_age_interview_est +
                                   parent_education + asvab_score + mother_age_at_birth +
                                   residential_moves_by_12,
                                   design = svy_design_rel_mob,
                                   family = poisson
                                   )

    rel_mob_smoking30 = list(
                             coeff = coefficients(msm_rel_mob_smoking30),
                             vcov = vcov(msm_rel_mob_smoking30)
                             )

    msm_gini_smoking30 = svyglm(smoking_30 ~ z_gini_exposure +
                                male + ethnicity + max_age_interview_est +
                                parent_education + asvab_score + mother_age_at_birth +
                                residential_moves_by_12,
                                design = svy_design_gini,
                                family = poisson
                                )


    gini_smoking30  = list(
                           coeff = coefficients(msm_gini_smoking30),
                           vcov = vcov(msm_gini_smoking30)
                           )

    par_results = list(
                       "rel_mob_bmi"  = rel_mob_bmi,
                       "rel_mob_depression" = rel_mob_depression,
                       "rel_mob_health" = rel_mob_health,
                       "rel_mob_smoking" = rel_mob_smoking,
                       "rel_mob_smoking30" = rel_mob_smoking30,
                       "gini_bmi" = gini_bmi,
                       "gini_depression" = gini_depression,
                       "gini_health" = gini_health,
                       "gini_smoking" = gini_smoking,
                       "gini_smoking30" = gini_smoking30
                       )

    par_results = list(par_results)
    names(par_results) = paste0("imp_", i)
    return(par_results)
}

# combine results from models
outputs = c("bmi", "depression", "health", "smoking", "smoking30")
type = c("rel_mob", "gini")

for (i in outputs) {
    for (h in type) {
        tname = paste0(h, "_", i)
        assign(paste0(h, "_results_", i),
               MIcombine(getSublist(getSublist(results, tname), "coeff"),
                         getSublist(getSublist(results, tname), "vcov")
                         )
               )
    }
}

# create table in latex
N = nrow(fwave)

# list with all the results
list_results = list(
                    health = list(rel_mob_results_health, gini_results_health),
                    bmi = list(rel_mob_results_bmi, gini_results_bmi),
                    depression = list(rel_mob_results_depression, gini_results_depression),
                    smoking = list(rel_mob_results_smoking, gini_results_smoking),
                    smoking30 = list(rel_mob_results_smoking30, gini_results_smoking30)
                    )

# loop to create texreg objects
for (i in names(list_results)) {
    sublist = list_results[[i]]
    vnames = names(sublist[[1]]$coefficients)
    position = grep("exposure", vnames)
    coeff = c(sublist[[1]]$coefficients[position], sublist[[2]]$coefficients[position])
    assign(paste0("tr_", i),
        createTexreg(
            coef.names =  names(coeff),
            coef = coeff,
            se = c(getCoefficients(sublist[[1]], position = position, coeff = FALSE),
                   getCoefficients(sublist[[2]], position = position, coeff = FALSE)),
            gof.names = 'Observations',
            gof = N,
            gof.decimal = FALSE
    ))
}

# create summary table
models = list(tr_health, tr_bmi, tr_depression,
              tr_smoking, tr_smoking30)

cmodels = c("Self-reported healh", "BMI", "Depression", "Smoking", "Day smoking last 30 days")

cnames = list(
              z_relative_mob_exposure = 'Income relative mobility average exposure',
              z_gini_exposure = 'Gini average exposure'
              )

texreg(
    models,
    float.pos = "htp",
    caption = "Income mobility and inequality exposure models (NLSY 97)",
    booktabs = TRUE,
    use.packages = FALSE,
    dcolumn = TRUE,
    caption.above = TRUE,
    scalebox = 0.65,
    label = "tbl:nlsy97_rel_mob_exposure_models",
    # sideways = TRUE,
    digits = 2,
    custom.model.names = cmodels,
    custom.coef.map = cnames,
    # groups = list("Random Effects" = c(4:9)),
    custom.note = "Note: Each row represents a model. ",
    file = "ch03/manuscript/tables/summary_relative_mob_nlsy97_exposure_20.tex"
)

# absolute mobility

imp = readRDS('ch03/output/data/nlsy97_absolute_mob_imputation.rds')

stopCluster(cl)
cl = makeCluster(10)
registerDoParallel(cl)

results = foreach(i = 1:imp$m,
                  .combine = append,
                  .packages = c("data.table", "ipw",
                                "survey", "forcats")) %dopar% {
    dat = data.table(mice::complete(imp, i))
     setorder(dat, id, year)

    # create lag variables
    lag_vars = c("imp_parent_employed", "imp_parent_married", "hhsize",
                 "bmi", "health", "smoking_ever", "smoking_30", "log_population",
                 "log_county_income", "z_absolute_mob", "z_gini")
    dat[, paste0("lag_", lag_vars) := lapply(.SD, shift), id,
        .SDcol = lag_vars]

    # impute first value backwards
    dat[, paste0("lag_", lag_vars) := lapply(.SD, impute_locf), id,
        .SDcol = paste0("lag_", lag_vars)]

    # exposure variables
    dat[as.numeric(age_interview_est) <= 20, total_exposure_time :=
        sum(exposure_time), id]
    dat[as.numeric(age_interview_est) <= 20,
        z_absolute_mob_exposure :=
        sum(z_absolute_mob * exposure_time) / sum(exposure_time), id]
    dat[as.numeric(age_interview_est) <= 20,
        z_gini_exposure := sum(z_gini * exposure_time) / sum(exposure_time), id]

    # dat[as.numeric(age_interview_est) < as.numeric(max_age_interview_est), total_exposure_time := sum(exposure_time), id]
    # dat[as.numeric(age_interview_est) < as.numeric(max_age_interview_est),
    #     z_absolute_mob_exposure := sum(z_absolute_mob * exposure_time) / sum(exposure_time), id]
    # dat[as.numeric(age_interview_est) < as.numeric(max_age_interview_est),
    #     z_gini_exposure := sum(z_gini * exposure_time) / sum(exposure_time), id]

    dat[, z_absolute_mob_exposure := getMax(z_absolute_mob_exposure), id]
    dat[, z_gini_exposure := getMax(z_gini_exposure), id]
    dat[, max_age_interview_est := factor(max_age_interview_est)]
    dat[, health := factor(health)]
    dat[, lag_health := factor(lag_health)]

    # last wave and exposure datasets
    last_wave = dat[year == 2015]
    dat = dat[as.numeric(age_interview_est) <= 20]
    # dat = dat[as.numeric(age_interview_est) < as.numeric(max_age_interview_est)]

    dat[, time := 1:.N, id]
    dat[, cyear := year - mean(year)]
    dat[, max_age_interview_est := fct_drop(max_age_interview_est)]

    temp_absolute_mob = ipwtm(
        exposure = z_absolute_mob,
        family = "gaussian",
        numerator   = ~ male + ethnicity + max_age_interview_est +
                        parent_education + asvab_score + mother_age_at_birth +
                        residential_moves_by_12,
        denominator = ~ male + ethnicity + max_age_interview_est +
                        parent_education + asvab_score + mother_age_at_birth +
                        residential_moves_by_12 +
                        lag_imp_parent_employed + lag_imp_parent_married + lag_hhsize +
                        lag_bmi + lag_health + lag_smoking_ever + lag_smoking_30 +
                        log_income_adj + cyear,
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
                        log_income_adj + cyear,
        timevar = time,
        type = "all",
        corstr = "ar1",
        id = id,
        data = dat
    )

    dat[, ipw_abs_mob := temp_absolute_mob$ipw.weights]
    dat[, ipw_gini := temp_gini$ipw.weights]
    dat[, max_year := getMax(year), id]

    sdat = dat[year == max_year, .SD, .SDcol = names(dat) %like% "^id|^ipw"]
    fwave = merge(last_wave, sdat, on = "id")

    svy_design_abs_mob = svydesign(ids = ~ 1, weights = ~ ipw_abs_mob, data = fwave)
    svy_design_gini = svydesign(ids = ~ 1, weights = ~ ipw_gini, data = fwave)

    # bmi
    msm_abs_mob_bmi = svyglm(bmi ~ z_absolute_mob_exposure +
                             male + ethnicity + max_age_interview_est +
                             parent_education + asvab_score + mother_age_at_birth +
                             residential_moves_by_12,
                             design = svy_design_abs_mob)

    abs_mob_bmi = list(
                       coeff = coefficients(msm_abs_mob_bmi),
                       vcov = vcov(msm_abs_mob_bmi)
                  )

    msm_gini_bmi = svyglm(bmi ~ z_gini_exposure +
                          male + ethnicity + max_age_interview_est +
                          parent_education + asvab_score + mother_age_at_birth +
                          residential_moves_by_12,
                          design = svy_design_gini)

    gini_bmi = list(
                    coeff = coefficients(msm_gini_bmi),
                    vcov = vcov(msm_gini_bmi)
                    )

    # depression
    msm_abs_mob_depression = svyglm(depression ~ z_absolute_mob_exposure +
                                    male + ethnicity + max_age_interview_est +
                                    parent_education + asvab_score + mother_age_at_birth +
                                    residential_moves_by_12,
                                    design = svy_design_abs_mob)

    abs_mob_depression = list(
                              coeff = coefficients(msm_abs_mob_depression),
                              vcov = vcov(msm_abs_mob_depression)
                              )

    msm_gini_depression = svyglm(depression  ~ z_gini_exposure +
                                 male + ethnicity + max_age_interview_est +
                                 parent_education + asvab_score + mother_age_at_birth +
                                 residential_moves_by_12,
                                 design = svy_design_gini)

    gini_depression = list(
                           coeff = coefficients(msm_gini_depression),
                           vcov = vcov(msm_gini_depression)
                           )

    # health
    msm_abs_mob_health = svyolr(health ~ z_absolute_mob_exposure +
                                male + ethnicity + max_age_interview_est +
                                parent_education + asvab_score + mother_age_at_birth +
                                residential_moves_by_12,
                                design = svy_design_abs_mob)

    abs_mob_health =  list(
                           coeff = coefficients(msm_abs_mob_health),
                           vcov = vcov(msm_abs_mob_health)
                           )

    msm_gini_health = svyolr(health ~ z_gini_exposure +
                             male + ethnicity + max_age_interview_est +
                             parent_education + asvab_score + mother_age_at_birth +
                             residential_moves_by_12,
                             design = svy_design_gini)

    gini_health = list(
                       coeff = coefficients(msm_gini_health),
                       vcov = vcov(msm_gini_health)
                       )


    # smoking
    msm_abs_mob_smoking = svyglm(smoking_ever ~ z_absolute_mob_exposure +
                                 male + ethnicity + max_age_interview_est +
                                 parent_education + asvab_score + mother_age_at_birth +
                                 residential_moves_by_12,
                                 design = svy_design_abs_mob,
                                 family = quasibinomial
                                 )

    abs_mob_smoking = list(
                           coeff = coefficients(msm_abs_mob_smoking),
                           vcov = vcov(msm_abs_mob_smoking)
                           )

    msm_gini_smoking = svyglm(smoking_ever ~ z_gini_exposure +
                              male + ethnicity + max_age_interview_est +
                              parent_education + asvab_score + mother_age_at_birth +
                              residential_moves_by_12,
                              design = svy_design_gini,
                              family = quasibinomial
                              )

    gini_smoking = list(
                        coeff = coefficients(msm_gini_smoking),
                        vcov = vcov(msm_gini_smoking)
                        )

    # smoking 30
    msm_abs_mob_smoking30 = svyglm(smoking_30 ~ z_absolute_mob_exposure +
                                   male + ethnicity + max_age_interview_est +
                                   parent_education + asvab_score + mother_age_at_birth +
                                   residential_moves_by_12,
                                   design = svy_design_abs_mob,
                                   family = poisson
                                   )

    abs_mob_smoking30 = list(
                             coeff = coefficients(msm_abs_mob_smoking30),
                             vcov = vcov(msm_abs_mob_smoking30)
                             )

    msm_gini_smoking30 = svyglm(smoking_30 ~ z_gini_exposure +
                                male + ethnicity + max_age_interview_est +
                                parent_education + asvab_score + mother_age_at_birth +
                                residential_moves_by_12,
                                design = svy_design_gini,
                                family = poisson
                                )

    gini_smoking30  = list(
                           coeff = coefficients(msm_gini_smoking30),
                           vcov = vcov(msm_gini_smoking30)
                           )

    par_results = list(
                       "abs_mob_bmi"  = abs_mob_bmi,
                       "abs_mob_depression" = abs_mob_depression,
                       "abs_mob_health" = abs_mob_health,
                       "abs_mob_smoking" = abs_mob_smoking,
                       "abs_mob_smoking30" = abs_mob_smoking30,
                       "gini_bmi" = gini_bmi,
                       "gini_depression" = gini_depression,
                       "gini_health" = gini_health,
                       "gini_smoking" = gini_smoking,
                       "gini_smoking30" = gini_smoking30
                       )

    par_results = list(par_results)
    names(par_results) = paste0("imp_", i)
    return(par_results)
}

# combine results from models
outputs = c("bmi", "depression", "health", "smoking", "smoking30")
type = c("abs_mob", "gini")

for (i in outputs) {
    for (h in type) {
        tname = paste0(h, "_", i)
        assign(paste0(h, "_results_", i),
               MIcombine(getSublist(getSublist(results, tname), "coeff"),
                         getSublist(getSublist(results, tname), "vcov")
                         )
               )
    }
}

# create table in latex
N = nrow(fwave)

# list with all the results
list_results = list(
                    health = list(abs_mob_results_health, gini_results_health),
                    bmi = list(abs_mob_results_bmi, gini_results_bmi),
                    depression = list(abs_mob_results_depression, gini_results_depression),
                    smoking = list(abs_mob_results_smoking, gini_results_smoking),
                    smoking30 = list(abs_mob_results_smoking30, gini_results_smoking30)
                    )

# loop to create texreg objects
for (i in names(list_results)) {
    sublist = list_results[[i]]
    vnames = names(sublist[[1]]$coefficients)
    position = grep("exposure", vnames)
    coeff = c(sublist[[1]]$coefficients[position], sublist[[2]]$coefficients[position])
    assign(paste0("tr_", i), createTexreg(
        coef.names =  names(coeff),
        coef = coeff,
        se = c(getCoefficients(sublist[[1]], position = position, coeff = FALSE),
               getCoefficients(sublist[[2]], position = position, coeff = FALSE)),
        gof.names = 'Observations',
        gof = N,
        gof.decimal = FALSE
    ))
}

# create summary table
models = list(tr_health, tr_bmi, tr_depression,
              tr_smoking, tr_smoking30)

cmodels = c("Self-reported healh", "BMI", "Depression", "Smoking", "Day smoking last 30 days")

cnames = list(
              z_absolute_mob_exposure = 'Income absolute mobility average exposure',
              z_gini_exposure = 'Gini average exposure'
              )

texreg(
    models,
    float.pos = "htp",
    caption = "Income mobility and inequality exposure models (NLSY 97)",
    booktabs = TRUE,
    use.packages = FALSE,
    dcolumn = TRUE,
    caption.above = TRUE,
    scalebox = 0.65,
    label = "tbl:nlsy97_abs_mob_exposure_models",
    # sideways = TRUE,
    digits = 2,
    custom.model.names = cmodels,
    custom.coef.map = cnames,
    # groups = list("Random Effects" = c(4:9)),
    custom.note = "Note: Each row represents a model. ",
    file = "ch03/manuscript/tables/summary_absolute_mob_nlsy97_exposure_20.tex"
)


# relative mobility categorical

imp = readRDS('ch03/output/data/nlsy97_qrelative_mob_imputation.rds')

stopCluster(cl)
cl = makeCluster(10)
registerDoParallel(cl)

results = foreach(i = 1:imp$m,
                  .combine = append,
                  .packages = c("data.table", "ipw",
                                "survey", "forcats")) %dopar% {

     dat = data.table(mice::complete(imp, i))
     setorder(dat, id, year)

    # create lag variables
    lag_vars = c("imp_parent_employed", "imp_parent_married", "hhsize",
                 "bmi", "health", "smoking_ever", "smoking_30", "log_population",
                 "log_county_income", "q_relative_mob", "q_gini")
    dat[, paste0("lag_", lag_vars) := lapply(.SD, shift), id,
        .SDcol = lag_vars]

    # impute first value backwards
    dat[, paste0("lag_", lag_vars) := lapply(.SD, impute_locf), id,
        .SDcol = paste0("lag_", lag_vars)]

    # exposure variables
    dat[as.numeric(age_interview_est) <= 20, total_exposure_time :=
        sum(exposure_time), id]
    dat[as.numeric(age_interview_est) <= 20,
        z_absolute_mob_exposure :=
        sum(z_absolute_mob * exposure_time) / sum(exposure_time), id]
    dat[as.numeric(age_interview_est) <= 20,
        z_gini_exposure := sum(z_gini * exposure_time) / sum(exposure_time), id]

    # dat[as.numeric(age_interview_est) < as.numeric(max_age_interview_est), total_exposure_time := sum(exposure_time), id]
    # dat[as.numeric(age_interview_est) < as.numeric(max_age_interview_est),
    #     z_absolute_mob_exposure := sum(z_absolute_mob * exposure_time) / sum(exposure_time), id]
    # dat[as.numeric(age_interview_est) < as.numeric(max_age_interview_est),
    #     z_gini_exposure := sum(z_gini * exposure_time) / sum(exposure_time), id]

    dat[, z_absolute_mob_exposure := getMax(z_absolute_mob_exposure), id]
    dat[, z_gini_exposure := getMax(z_gini_exposure), id]
    dat[, max_age_interview_est := factor(max_age_interview_est)]
    dat[, health := factor(health)]
    dat[, lag_health := factor(lag_health)]

    # last wave and exposure datasets
    last_wave = dat[year == 2015]
    dat = dat[as.numeric(age_interview_est) <= 20]
    # dat = dat[as.numeric(age_interview_est) < as.numeric(max_age_interview_est)]

    dat[, time := 1:.N, id]
    dat[, cyear := year - mean(year)]
    dat[, max_age_interview_est := fct_drop(max_age_interview_est)]

    temp_absolute_mob = ipwtm(
        exposure = q_relative_mob,
        family = "gaussian",
        numerator   = ~ male + ethnicity + max_age_interview_est +
                        parent_education + asvab_score + mother_age_at_birth +
                        residential_moves_by_12,
        denominator = ~ male + ethnicity + max_age_interview_est +
                        parent_education + asvab_score + mother_age_at_birth +
                        residential_moves_by_12 +
                        lag_imp_parent_employed + lag_imp_parent_married + lag_hhsize +
                        lag_bmi + lag_health + lag_smoking_ever + lag_smoking_30 +
                        log_income_adj + cyear,
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
                        log_income_adj + cyear,
        timevar = time,
        type = "all",
        corstr = "ar1",
        id = id,
        data = dat
    )

    dat[, ipw_abs_mob := temp_absolute_mob$ipw.weights]
    dat[, ipw_gini := temp_gini$ipw.weights]
    dat[, max_year := getMax(year), id]

    sdat = dat[year == max_year, .SD, .SDcol = names(dat) %like% "^id|^ipw"]
    fwave = merge(last_wave, sdat, on = "id")

    svy_design_abs_mob = svydesign(ids = ~ 1, weights = ~ ipw_abs_mob, data = fwave)
    svy_design_gini = svydesign(ids = ~ 1, weights = ~ ipw_gini, data = fwave)

    # bmi
    msm_abs_mob_bmi = svyglm(bmi ~ z_absolute_mob_exposure +
                             male + ethnicity + max_age_interview_est +
                             parent_education + asvab_score + mother_age_at_birth +
                             residential_moves_by_12,
                             design = svy_design_abs_mob)

    abs_mob_bmi = list(
                       coeff = coefficients(msm_abs_mob_bmi),
                       vcov = vcov(msm_abs_mob_bmi)
                  )

    msm_gini_bmi = svyglm(bmi ~ z_gini_exposure +
                          male + ethnicity + max_age_interview_est +
                          parent_education + asvab_score + mother_age_at_birth +
                          residential_moves_by_12,
                          design = svy_design_gini)

    gini_bmi = list(
                    coeff = coefficients(msm_gini_bmi),
                    vcov = vcov(msm_gini_bmi)
                    )

    # depression
    msm_abs_mob_depression = svyglm(depression ~ z_absolute_mob_exposure +
                                    male + ethnicity + max_age_interview_est +
                                    parent_education + asvab_score + mother_age_at_birth +
                                    residential_moves_by_12,
                                    design = svy_design_abs_mob)

    abs_mob_depression = list(
                              coeff = coefficients(msm_abs_mob_depression),
                              vcov = vcov(msm_abs_mob_depression)
                              )

    msm_gini_depression = svyglm(depression  ~ z_gini_exposure +
                                 male + ethnicity + max_age_interview_est +
                                 parent_education + asvab_score + mother_age_at_birth +
                                 residential_moves_by_12,
                                 design = svy_design_gini)

    gini_depression = list(
                           coeff = coefficients(msm_gini_depression),
                           vcov = vcov(msm_gini_depression)
                           )

    # health
    msm_abs_mob_health = svyolr(health ~ z_absolute_mob_exposure +
                                male + ethnicity + max_age_interview_est +
                                parent_education + asvab_score + mother_age_at_birth +
                                residential_moves_by_12,
                                design = svy_design_abs_mob)

    abs_mob_health =  list(
                           coeff = coefficients(msm_abs_mob_health),
                           vcov = vcov(msm_abs_mob_health)
                           )

    msm_gini_health = svyolr(health ~ z_gini_exposure +
                             male + ethnicity + max_age_interview_est +
                             parent_education + asvab_score + mother_age_at_birth +
                             residential_moves_by_12,
                             design = svy_design_gini)

    gini_health = list(
                       coeff = coefficients(msm_gini_health),
                       vcov = vcov(msm_gini_health)
                       )


    # smoking
    msm_abs_mob_smoking = svyglm(smoking_ever ~ z_absolute_mob_exposure +
                                 male + ethnicity + max_age_interview_est +
                                 parent_education + asvab_score + mother_age_at_birth +
                                 residential_moves_by_12,
                                 design = svy_design_abs_mob,
                                 family = quasibinomial
                                 )

    abs_mob_smoking = list(
                           coeff = coefficients(msm_abs_mob_smoking),
                           vcov = vcov(msm_abs_mob_smoking)
                           )

    msm_gini_smoking = svyglm(smoking_ever ~ z_gini_exposure +
                              male + ethnicity + max_age_interview_est +
                              parent_education + asvab_score + mother_age_at_birth +
                              residential_moves_by_12,
                              design = svy_design_gini,
                              family = quasibinomial
                              )

    gini_smoking = list(
                        coeff = coefficients(msm_gini_smoking),
                        vcov = vcov(msm_gini_smoking)
                        )

    # smoking 30
    msm_abs_mob_smoking30 = svyglm(smoking_30 ~ z_absolute_mob_exposure +
                                   male + ethnicity + max_age_interview_est +
                                   parent_education + asvab_score + mother_age_at_birth +
                                   residential_moves_by_12,
                                   design = svy_design_abs_mob,
                                   family = poisson
                                   )

    abs_mob_smoking30 = list(
                             coeff = coefficients(msm_abs_mob_smoking30),
                             vcov = vcov(msm_abs_mob_smoking30)
                             )

    msm_gini_smoking30 = svyglm(smoking_30 ~ z_gini_exposure +
                                male + ethnicity + max_age_interview_est +
                                parent_education + asvab_score + mother_age_at_birth +
                                residential_moves_by_12,
                                design = svy_design_gini,
                                family = poisson
                                )

    gini_smoking30  = list(
                           coeff = coefficients(msm_gini_smoking30),
                           vcov = vcov(msm_gini_smoking30)
                           )

    par_results = list(
                       "abs_mob_bmi"  = abs_mob_bmi,
                       "abs_mob_depression" = abs_mob_depression,
                       "abs_mob_health" = abs_mob_health,
                       "abs_mob_smoking" = abs_mob_smoking,
                       "abs_mob_smoking30" = abs_mob_smoking30,
                       "gini_bmi" = gini_bmi,
                       "gini_depression" = gini_depression,
                       "gini_health" = gini_health,
                       "gini_smoking" = gini_smoking,
                       "gini_smoking30" = gini_smoking30
                       )

    par_results = list(par_results)
    names(par_results) = paste0("imp_", i)
    return(par_results)
}

# combine results from models
outputs = c("bmi", "depression", "health", "smoking", "smoking30")
type = c("abs_mob", "gini")

for (i in outputs) {
    for (h in type) {
        tname = paste0(h, "_", i)
        assign(paste0(h, "_results_", i),
               MIcombine(getSublist(getSublist(results, tname), "coeff"),
                         getSublist(getSublist(results, tname), "vcov")
                         )
               )
    }
}

# create table in latex
N = nrow(fwave)

# list with all the results
list_results = list(
                    health = list(abs_mob_results_health, gini_results_health),
                    bmi = list(abs_mob_results_bmi, gini_results_bmi),
                    depression = list(abs_mob_results_depression, gini_results_depression),
                    smoking = list(abs_mob_results_smoking, gini_results_smoking),
                    smoking30 = list(abs_mob_results_smoking30, gini_results_smoking30)
                    )

# loop to create texreg objects
for (i in names(list_results)) {
    sublist = list_results[[i]]
    vnames = names(sublist[[1]]$coefficients)
    position = grep("exposure", vnames)
    coeff = c(sublist[[1]]$coefficients[position], sublist[[2]]$coefficients[position])
    assign(paste0("tr_", i), createTexreg(
        coef.names =  names(coeff),
        coef = coeff,
        se = c(getCoefficients(sublist[[1]], position = position, coeff = FALSE),
               getCoefficients(sublist[[2]], position = position, coeff = FALSE)),
        gof.names = 'Observations',
        gof = N,
        gof.decimal = FALSE
    ))
}

# create summary table
models = list(tr_health, tr_bmi, tr_depression,
              tr_smoking, tr_smoking30)

cmodels = c("Self-reported healh", "BMI", "Depression", "Smoking", "Day smoking last 30 days")

cnames = list(
              z_absolute_mob_exposure = 'Income absolute mobility average exposure',
              z_gini_exposure = 'Gini average exposure'
              )

texreg(
    models,
    float.pos = "htp",
    caption = "Income mobility and inequality exposure models (NLSY 97)",
    booktabs = TRUE,
    use.packages = FALSE,
    dcolumn = TRUE,
    caption.above = TRUE,
    scalebox = 0.65,
    label = "tbl:nlsy97_abs_mob_exposure_models",
    # sideways = TRUE,
    digits = 2,
    custom.model.names = cmodels,
    custom.coef.map = cnames,
    # groups = list("Random Effects" = c(4:9)),
    custom.note = "Note: Each row represents a model. ",
    file = "ch03/manuscript/tables/summary_absolute_mob_nlsy97_exposure_20.tex"
)


library(MASS)

dat = data.table(haven::read_stata("https://stats.idre.ucla.edu/stat/data/ologit.dta"))
dat[, apply := factor(apply)]
m <- polr(apply ~ pared + public + gpa, data = dat, Hess=TRUE)

predict(m,  type = "probs")

table(dat$apply)
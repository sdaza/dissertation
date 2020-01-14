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
             "log_county_income", "z_relative_mob", "z_gini")

baseline_vars = c("z_relative_mob", "z_gini", "imp_parent_employed",
                  "imp_parent_married", "hhsize", "bmi", "health",
                  "smoking_ever", "smoking_30", "log_population",
                  "log_county_income", "log_income_adj")

denominator_time1 = "
    male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj +
    baseline_hhsize + baseline_bmi + baseline_z_gini +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_health + baseline_smoking_30
"

numerator = "
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj +
    baseline_hhsize + baseline_bmi +
    baseline_z_relative_mob + baseline_z_gini +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_health + baseline_smoking_30) * as.factor(stime)
"

denominator = "
    (male + ethnicity + as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj +
    baseline_hhsize + baseline_bmi +
    baseline_z_relative_mob + baseline_z_gini +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_health + baseline_smoking_30 +
    lag_imp_parent_employed + lag_imp_parent_married + lag_hhsize +
    log_income_adj + lag_z_relative_mob) * as.factor(stime)
    "

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


rr_bmi = ipwExposure(
    imputations = imp,
    lag_variables = lag_vars,
    baseline_variables = baseline_vars,
    denominator_time1 = denominator_time1 ,
    numerator = numerator,
    denominator = denominator,
    exposure_variable = "z_relative_mob",
    id_var = "id",
    time_var = "stime",
    max_time_exposure = 8,
    final_model = "model_bmi",
    trim_p = 0.01,
    exposure_type = "gaussian",
    final_model_type = "gaussian"
)


model_smoking = formula(
    smoking_ever ~
    average_z_relative_mob + male + ethnicity +
    as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj +
    baseline_hhsize + baseline_bmi +
    baseline_z_relative_mob + baseline_z_gini +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_health + baseline_smoking_30
)

model_smoking_30 = formula(
    smoking_30 ~
    average_z_relative_mob + male + ethnicity +
    as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj +
    baseline_hhsize + baseline_bmi +
    baseline_z_relative_mob + baseline_z_gini +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_health + baseline_smoking_30
)

model_health = formula(
    health ~
    average_z_relative_mob + male + ethnicity +
    as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj +
    baseline_hhsize + baseline_bmi +
    baseline_z_relative_mob + baseline_z_gini +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_health + baseline_smoking_30
)

model_depression = formula(
    depression ~
    average_z_relative_mob + male + ethnicity +
    as.factor(max_age_interview_est) +
    parent_education + asvab_score + mother_age_at_birth +
    residential_moves_by_12 +
    baseline_log_income_adj +
    baseline_hhsize + baseline_bmi +
    baseline_z_relative_mob + baseline_z_gini +
    baseline_imp_parent_employed + baseline_imp_parent_employed +
    baseline_health + baseline_smoking_30
)



rr_bmi = ipwContinousExposure(
    imputations = imp,
    lag_variables = lag_vars,
    baseline_variables = baseline_vars,
    time1_numerator = numerator_1,
    time1_denominator = denominator_1,
    numerator = numerator ,
    denominator = denominator,
    treatment  = "z_relative_mob",
    id_var = "id",
    time_var = "stime",
    max_time  = 8,
    final_model = model_bmi,
    trim_p = 0.01,
    model_type = "gaussian"
)

rr_depression = ipwContinousExposure(
    imputations = imp,
    lag_variables = lag_vars,
    baseline_variables = baseline_vars,
    time1_numerator = numerator_1,
    time1_denominator = denominator_1,
    numerator = numerator ,
    denominator = denominator,
    treatment  = "z_relative_mob",
    id_var = "id",
    time_var = "stime",
    max_time  = 8,
    final_model = model_depression,
    trim_p = 0.01,
    model_type = "gaussian"
)


summary(rr_bmi)

rr_smoking = ipwContinousExposure(
    imputations = imp,
    lag_variables = lag_vars,
    baseline_variables = baseline_vars,
    time1_numerator = numerator_1,
    time1_denominator = denominator_1,
    numerator = numerator ,
    denominator = denominator,
    treatment  = "z_relative_mob",
    id_var = "id",
    time_var = "stime",
    max_time  = 8,
    final_model = model_smoking,
    trim_p = 0.01,
    model_type = "binomial"
)

rr_smoking_30 = ipwContinousExposure(
    imputations = imp,
    lag_variables = lag_vars,
    baseline_variables = baseline_vars,
    time1_numerator = numerator_1,
    time1_denominator = denominator_1,
    numerator = numerator ,
    denominator = denominator,
    treatment  = "z_relative_mob",
    id_var = "id",
    time_var = "stime",
    max_time  = 8,
    final_model = model_smoking_30,
    trim_p = 0.01,
    model_type = "poisson"
)

rr_health = ipwContinousExposure(
    imputations = imp,
    lag_variables = lag_vars,
    baseline_variables = baseline_vars,
    time1_numerator = numerator_1,
    time1_denominator = denominator_1,
    numerator = numerator ,
    denominator = denominator,
    treatment  = "z_relative_mob",
    id_var = "id",
    time_var = "stime",
    max_time  = 8,
    final_model = model_health,
    trim_p = 0.01,
    model_type = "ordinal"
)

summary(rr_bmi)
summary(rr_depression)
summary(rr_smoking)
summary(rr_smoking_30)
summary(rr_health)

# relative mobility (categorical version)


bmi_coeff = list()
bmi_vcov = list()

for (i in 1:imp$m) {

print(paste0("imputation ", i))

dat = data.table(mice::complete(imp, i))
dat[, age_interview_est := as.numeric(as.character(age_interview_est))]

# define variables for models
setorder(dat, cols = c(id_var, time_var))

names(dat)


dat[, paste0("lag_", lag_vars) := lapply(.SD, shift), id,
    .SDcol = lag_vars]

dat[, health := factor(health)]
dat[, lag_health := factor(lag_health)]

baseline_vars = c("z_relative_mob", "z_gini", "imp_parent_employed", "imp_parent_married",
                  "hhsize", "bmi", "health", "smoking_ever", "smoking_30", "log_population",
                  "log_county_income", "log_income_adj",
                  "q_relative_mob", "q_gini")

dat[, paste0("baseline_", baseline_vars) := lapply(.SD, getFirst), id,
    .SDcol = baseline_vars]

# define working dataset
dat[, max_time := max(stime), id]
last_obs = dat[stime == max_time]

# last_obs = dat[stime == 9]
sdat = dat[stime <= 8]

table(sdat$stime)
table(sdat$age_interview_est)
table(sdat$max_age_interview_est)
table(sdat$q_relative_mob)

# estimate weights for time 1 and the time > 1 (until 8)
test1 = sdat[stime == 1]

model1a = polr(q_relative_mob ~ 1, data = test1)
model1b = polr(q_relative_mob ~
               male + ethnicity + as.factor(max_age_interview_est) +
               parent_education + asvab_score + mother_age_at_birth +
               residential_moves_by_12 +
               baseline_log_income_adj +
               baseline_hhsize + baseline_bmi + baseline_q_gini +
               baseline_imp_parent_employed + baseline_imp_parent_employed +
               baseline_health + baseline_smoking_30,  data = test1)

probs1 = as.data.frame(predict(model1a, type = "probs"))
probs2 = as.data.frame(predict(model1b, type = "probs"))

numerator = rep(NA, nrow(test1))
denominator = rep(NA, nrow(test1))

for (j in unique(test1$q_relative_mob)) {
    flag = test1$q_relative_mob == j
    denominator[flag] = probs2[flag, j]
    numerator[flag] = probs1[flag, j]
}

weights_time_1 = numerator / denominator
rm(numerator, denominator)

summary(weights_time_1)

test1[, ipw := weights_time_1]

test2 = sdat[stime > 1]

model2a = polr(q_relative_mob ~ male + ethnicity + as.factor(max_age_interview_est) +
         parent_education + asvab_score + mother_age_at_birth +
         residential_moves_by_12 +
         baseline_log_income_adj +
         baseline_hhsize + baseline_bmi +
         baseline_q_relative_mob + baseline_q_gini +
         baseline_imp_parent_employed + baseline_imp_parent_employed +
         baseline_health + baseline_smoking_30 + lag_q_relative_mob
          + as.factor(stime),
         data = test2)

model2b = polr(q_relative_mob ~ male + ethnicity + as.factor(max_age_interview_est) +
             parent_education + asvab_score + mother_age_at_birth +
             residential_moves_by_12 +
             baseline_log_income_adj +
             baseline_hhsize + baseline_bmi +
             baseline_q_relative_mob + baseline_q_gini +
             baseline_imp_parent_employed + baseline_imp_parent_employed +
             baseline_health + baseline_smoking_30 +
             lag_imp_parent_employed + lag_imp_parent_married + lag_hhsize +
             log_income_adj + lag_q_relative_mob +  as.factor(stime),
             data = test2)

probs1 = as.data.frame(predict(model2a, type = "probs"))
probs2 = as.data.frame(predict(model2b, type = "probs"))

numerator = rep(NA, nrow(test2))
denominator = rep(NA, nrow(test2))

for (j in unique(test2$q_relative_mob)) {
    flag = test2$q_relative_mob == j
    denominator[flag] = probs2[flag, j]
    numerator[flag] = probs1[flag, j]
}

weights_time_2 = numerator / denominator
rm(numerator, denominator)
summary(weights_time_2)
test2[, ipw := weights_time_2]

gdata = rbind(test1, test2)
gdata[, average_q_relative_mob := mean(as.numeric(as.character(q_relative_mob))), id]
table(gdata$stime)

# explore some examples
# ids = unique(gdata$id)
# gdata[id == sample(ids, 1), .(id, stime, z_relative_mob, average_z_relative_mob)]
# gdata[, .(id, stime, z_relative_mob, average_z_relative_mob)]

setorder(gdata, id, stime)
gdata[, cipw := cumprod(ipw), id]
gdata = gdata[stime == 8]
gdata[, tcipw := truncateWeights(cipw, 0.01)]

gdata = gdata[, .(id, cipw, tcipw, average_q_relative_mob)]
fdata = merge(last_obs, gdata, on = "id")
dim(fdata)

print(paste0("Mean of weights: ", round(mean(fdata$cipw), 2)))
print(paste0("Mean of weights (trunc): ", round(mean(fdata$tcipw), 2)))

svy_design = svydesign(ids = ~ 1, weights = ~ tcipw, data = fdata)
bmi = svyglm(bmi ~
             average_q_relative_mob + male + ethnicity +
             as.factor(max_age_interview_est) +
             parent_education + asvab_score + mother_age_at_birth +
             residential_moves_by_12 +
             baseline_log_income_adj +
             baseline_hhsize + baseline_bmi +
             baseline_q_relative_mob + baseline_q_gini +
             baseline_imp_parent_employed + baseline_imp_parent_employed +
             baseline_health + baseline_smoking_30, design = svy_design
             )


bmi_coeff[[i]] = coefficients(bmi)
bmi_vcov[[i]] = vcov(bmi)

}

results = MIcombine(bmi_coeff, bmi_vcov)
summary(results)


# continous variable

for (i in 1:imp$m) {

print(paste0("imputation ", i))

dat = data.table(mice::complete(imp, i))
dat[, age_interview_est := as.numeric(as.character(age_interview_est))]

# define variables for models
setorder(dat, id, stime)

lag_vars = c("imp_parent_employed", "imp_parent_married", "hhsize",
             "bmi", "health", "smoking_ever", "smoking_30", "log_population",
             "log_county_income", "z_relative_mob", "z_gini")

dat[, paste0("lag_", lag_vars) := lapply(.SD, shift), id,
    .SDcol = lag_vars]

dat[, health := factor(health)]
dat[, lag_health := factor(lag_health)]

baseline_vars = c("z_relative_mob", "z_gini", "imp_parent_employed", "imp_parent_married",
                  "hhsize", "bmi", "health", "smoking_ever", "smoking_30", "log_population",
                  "log_county_income", "log_income_adj")

dat[, paste0("baseline_", baseline_vars) := lapply(.SD, getFirst), id,
    .SDcol = baseline_vars]

# define working dataset
dat[, max_time := max(stime), id]
last_obs = dat[stime == max_time]

# last_obs = dat[stime == 9]
sdat = dat[stime <= 8]

table(sdat$stime)
table(sdat$age_interview_est)
table(sdat$max_age_interview_est)

# estimate weights for time 1 and the time > 1 (until 8)
test1 = sdat[stime == 1]

names(test1)
model1a = lm(z_relative_mob ~ 1, data = test1)
model1b = lm(z_relative_mob ~
             male + ethnicity + as.factor(max_age_interview_est) +
             parent_education + asvab_score + mother_age_at_birth +
             residential_moves_by_12 +
             baseline_log_income_adj +
             baseline_hhsize + baseline_bmi + baseline_z_gini +
             baseline_imp_parent_employed + baseline_imp_parent_employed +
             baseline_health + baseline_smoking_30,  data = test1)

kdens1 = dnorm(test1[["z_relative_mob"]],
           predict(model1a),
           as.numeric(sd(model1a$residuals)))

kdens2 = dnorm(test1[["z_relative_mob"]],
           predict(model1b),
           as.numeric(sd(model1b$residuals)))

weights_time_1 = kdens1 / kdens2
rm(kdens1, kdens2)

summary(weights_time_1)
test1[, ipw := weights_time_1]

test2 = sdat[stime > 1]

model2a = lm(z_relative_mob ~ (male + ethnicity + as.factor(max_age_interview_est) +
         parent_education + asvab_score + mother_age_at_birth +
         residential_moves_by_12 +
         baseline_log_income_adj +
         baseline_hhsize + baseline_bmi +
         baseline_z_relative_mob + baseline_z_gini +
         baseline_imp_parent_employed + baseline_imp_parent_employed +
         baseline_health + baseline_smoking_30 + lag_z_relative_mob
         ) * as.factor(stime),
         data = test2)

model2b = lm(z_relative_mob ~ (male + ethnicity + as.factor(max_age_interview_est) +
             parent_education + asvab_score + mother_age_at_birth +
             residential_moves_by_12 +
             baseline_log_income_adj +
             baseline_hhsize + baseline_bmi +
             baseline_z_relative_mob + baseline_z_gini +
             baseline_imp_parent_employed + baseline_imp_parent_employed +
             baseline_health + baseline_smoking_30 +
             lag_imp_parent_employed + lag_imp_parent_married + lag_hhsize +
             log_income_adj + lag_z_relative_mob) * as.factor(stime),
             data = test2)

kdens1 = dnorm(test2[["z_relative_mob"]],
           predict(model2a),
           as.numeric(sd(model2a$residuals)))

kdens2 = dnorm(test2[["z_relative_mob"]],
           predict(model2b),
           as.numeric(sd(model2b$residuals)))

weights_time_2 = kdens1 / kdens2
rm(kdens1, kdens2)

summary(weights_time_2)
test2[, ipw := weights_time_2]

gdata = rbind(test1, test2)
gdata[, average_z_relative_mob := mean(z_relative_mob), id]
table(gdata$stime)

# explore some examples
# ids = unique(gdata$id)
# gdata[id == sample(ids, 1), .(id, stime, z_relative_mob, average_z_relative_mob)]
# gdata[, .(id, stime, z_relative_mob, average_z_relative_mob)]

setorder(gdata, id, stime)
gdata[, cipw := cumprod(ipw), id]
gdata = gdata[stime == 8]
gdata[, tcipw := truncateWeights(cipw, 0.01)]

sd(gdata$cipw)
summary(gdata$tcipw)

gdata = gdata[, .(id, cipw, tcipw, average_z_relative_mob)]
fdata = merge(last_obs, gdata, on = "id")
dim(fdata)

print(paste0("Mean of weights: ", round(mean(fdata$cipw), 2)))
print(paste0("Mean of weights (trunc): ", round(mean(fdata$tcipw), 2)))

names(fdata)
svy_design = svydesign(ids = ~ 1, weights = ~ tcipw, data = fdata)
bmi = svyglm(bmi ~
             average_z_relative_mob + male + ethnicity +
             as.factor(max_age_interview_est) +
             parent_education + asvab_score + mother_age_at_birth +
             residential_moves_by_12 +
             baseline_log_income_adj +
             baseline_hhsize + baseline_bmi +
             baseline_z_relative_mob + baseline_z_gini +
             baseline_imp_parent_employed + baseline_imp_parent_employed +
             baseline_health + baseline_smoking_30, design = svy_design
             )

bmi_coeff[[i]] =  coefficients(bmi)
bmi_vcov[[i]] = vcov(bmi)

}

results = MIcombine(bmi_coeff, bmi_vcov)
summary(results)
summary(glm(bmi ~ average_z_relative_mob, data = fdata))
summary(glm(depression ~ average_z_relative_mob, data = fdata))
summary(glm(health ~ average_z_relative_mob, data = fdata))

length(bmi_vcov)

temp_mob = ipwtm(
    exposure = z_relative_mob,
    family = "gaussian",
    numerator   = ~ (male + ethnicity + max_age_interview_est +
                    parent_education + asvab_score + mother_age_at_birth +
                    residential_moves_by_12 +
                    baseline_z_relative_mob + baseline_z_gini) * as.factor(stime),
    denominator = ~ (male + ethnicity + max_age_interview_est +
                    parent_education + asvab_score + mother_age_at_birth +
                    residential_moves_by_12 +
                    baseline_z_relative_mob + baseline_z_gini +
                    lag_imp_parent_employed + lag_imp_parent_married + lag_hhsize +
                    lag_bmi + lag_health + lag_smoking_ever + lag_smoking_30 +
                    log_income_adj) * as.factor(stime),
    timevar = stime,
    type = "all",
    corstr = "ar1",
    id = id,
    data = sdat[stime > 1],
    trunc = 0.01
)

summary(temp_mob$ipw.weights)
test = sdat[stime > 1 & stime <= 8]
test[, ipw := temp_mob$ipw.weights]
test[, average_z_relative_mob := mean(z_relative_mob), id]

test = test[stime == 8]

m1 = glm(bmi ~ average_z_relative_mob + male + ethnicity + max_age_interview_est +
             parent_education + asvab_score + mother_age_at_birth +
             residential_moves_by_12 +
             baseline_z_relative_mob + baseline_z_gini, data =  test, weights = test$ipw)

summary(m1)
ids = unique(test$id)
test[id == sample(ids, 1), .(id, stime, z_relative_mob, ipw)]

prop.table(xtabs(test$ipw ~ test$male + (test$z_relative_mob > mean(test$z_relative_mob))))
prop.table(table(test$male, (test$z_relative_mob > mean(test$z_relative_mob))))



test1 = sdat[stime == 1]
model1a = lm(z_relative_mob ~ 1, data = test1)
model1b = lm(z_relative_mob ~ male + ethnicity + max_age_interview_est +
                              parent_education + asvab_score + mother_age_at_birth +
                              residential_moves_by_12, data = test1)

kdens1 = dnorm(test1[["z_relative_mob"]],
               predict(model1a),
               as.numeric(sd(model1a$residuals)))

kdens2 = dnorm(test1[["z_relative_mob"]],
               predict(model1b),
               as.numeric(sd(model1b$residuals)))

weights_time_1 = kdens1 / kdens2
summary(weights_time_1)

test1[, ipw := weights_time_1]

test2 = sdat[stime > 1]

model2a = lm(z_relative_mob ~ (male + ethnicity + max_age_interview_est +
             parent_education + asvab_score + mother_age_at_birth +
             residential_moves_by_12 +
             baseline_z_relative_mob + baseline_z_gini) * as.factor(stime),
             data = test2)

model2b = lm(z_relative_mob ~ (male + ethnicity + max_age_interview_est +
             parent_education + asvab_score + mother_age_at_birth +
             residential_moves_by_12 +
             baseline_z_relative_mob + baseline_z_gini +
             lag_imp_parent_employed + lag_imp_parent_married + lag_hhsize +
             log_income_adj) * as.factor(stime),
             data = test2)

kdens1 = dnorm(test2[["z_relative_mob"]],
               predict(model2a),
               as.numeric(sd(model2a$residuals)))

kdens2 = dnorm(test2[["z_relative_mob"]],
               predict(model2b),
               as.numeric(sd(model2b$residuals)))

weights_time_2 = kdens1 / kdens2
summary(weights_time_2)
test2[, ipw := weights_time_2]

gdata = rbind(test1, test2)
dim(gdata)
gdata[, average_z_relative_mob := mean(z_relative_mob), id]
table(gdata$stime)

setorder(gdata, id, stime)
gdata[, cipw := cumprod(ipw), id]
gdata = gdata[stime == 8]
gdata[, tcipw := truncateWeights(cipw, 0.01)]

m1 = glm(bmi ~ average_z_relative_mob + male + ethnicity + max_age_interview_est +
             parent_education + asvab_score + mother_age_at_birth +
             residential_moves_by_12 +
             baseline_z_relative_mob + baseline_z_gini, data =  gdata, weights = gdata$tipw)

summary(m1)

m2 = glm(depression ~ average_z_relative_mob + male + ethnicity + max_age_interview_est +
             parent_education + asvab_score + mother_age_at_birth +
             residential_moves_by_12 +
             baseline_z_relative_mob + baseline_z_gini, data =  gdata, weights = gdata$tipw)

summary(m2)


m2 = glm(depression ~ average_z_relative_mob, data =  gdata, weights = gdata$ipw)




summary(m1)
gdata[, ipwt1 := truncateWeights(tipw, 0.01), id]
summary(gdata$cipw)
summary(gdata$tcipw)


    gdata[ttime == 1, tipw  := weights_time_1]

    gdata[ttime > 1, tipw := weights_time_2]

    setorder(gdata, id, time)

    gdata[, ipw := cumprod(tipw), id]
    gdata[, ipwt1 := truncateWeights(tipw, 0.01), id]
    gdata[, ipwt5 := truncateWeights(tipw, 0.05), id]
    return(gdata[, c(id, time, "ipw", "ipwt1", "ipwt5"), with = FALSE])




# create weights with function
tt = ipwC(sdat,
     exposure = "z_relative_mob",
     time_var = "stime",
     time_invariant = time_invariant,
     time_variant = time_variant
     )

summary(tt)
sdat[get("stime") > 1]

summary(tt)

tt[, max_time := max(as.numeric(as.character(time))), id]
summary(tt)

tt = tt[max_time == time]
fwave = merge(last_wave, tt, on = "id")


# create simple weights first






#set seed to replicate results
set.seed(12345)
#define sample size
n <- 2000
#define confounder c
c <- rnorm(n,0,1)
#define treatment at time 1 as function of confounder
t1 <- .1*c + rnorm(n,0, sqrt(.99))
#define depression at time 1 as function of confounder and treat1
d1 <- .1*c  + .4*t1 +  rnorm(n,0, sqrt(.822))
#define treatment at time 2 as function of confounder and dep1
t2 <- .1*c + .4*d1 + .4*t1 + rnorm(n,0, sqrt(.5196))
#define outcome depression at time 2 as function of confounder, treat1, and dep1
d2 <- .1*c + .4*t2 + .4*d1 + rnorm(n,0, sqrt(.4582))
#add ID variable to do mixed effects models later
id <- rep(1: length(c))
d
#put all in a dataframe and write data to harddrive to use later in e.g. SPSS
df1 <- data.frame(id, c, t1, d1, t2, d2)
dt1 = data.table(df1)
o
mt1 = melt(dt1, id.vars = c("id", "c"), measure = patterns("^t", "^d"), value.name = c("t", "d"),
     variable.name = "time")

setorder(mt1, id, time)
mt1[, lag_t := shift(t), id]
mt1[, lag_d := shift(d), id]

hw1n = glm(t ~ 1, data = mt1[time == 1])
hw1d = glm(t ~ c, data = mt1[time == 1])
hw2n = glm(t ~ c, data = mt1[time == 2])
hw2d = glm(t ~ lag_d +  lag_t +  c, data = mt1[time == 2])

w1 = (dnorm(mt1[time == 1, t], predict(hw1n), as.numeric(sd(hw1n$residuals)))) /
     (dnorm(mt1[time == 1, t], predict(hw1d), as.numeric(sd(hw1d$residuals))))

w2 = (dnorm(mt1[time == 2, t], predict(hw2n), as.numeric(sd(hw2n$residuals)))) /
     (dnorm(mt1[time == 2, t], predict(hw2d), as.numeric(sd(hw2d$residuals))))

summary(w2)
summary(w1)


# kdens1 = dnorm(mt1$t, predict(mnum), as.numeric(sd(mnum$residuals)))

mt1[time == 1, ww := w1]
mt1[time == 2, ww := w2]

setorder(mt1, time, id)
mt1[, ww := cumprod(ww), id]
summary(mt1$ww)

mt1[id == 2000]
iptw1 = ipwtm(
    exposure = t,
    family = "gaussian",
    numerator   = ~ c,
    denominator = ~ c + d,
    timevar = time,
    type = "all",
    corstr = "ar1",
    id = id,
    data = mt1)

summary(iptw1$ipw.weights)
mt1[, iptw := iptw1$ipw.weights]
cor(mt1[, .(iptw, ww)])


# estimation

m1 = glm(d ~ t + lag_t, data = mt1[time == 2])
m2 = glm(d ~ t + lag_t + lag_d + c, data = mt1[time == 2])
m3 = glm(d ~ t + lag_t + c, data = mt1[time == 2], weights = mt1[time == 2, ww])
m4 = glm(d ~ t + lag_t + c, data = mt1[time == 2], weights = mt1[time == 2, iptw])

screenreg(list(m1, m2, m3, m4))

library(tableone)

data = mt1
exposure = "t"
time = "time"
baseline = c("lag_d", "lag_t")
time_invariant = "c"
time_variant = ""
id = "id"

ipwC = function(data,
                exposure,
                time,
                baseline,
                time_invariant,
                time_variant,
                id = "id") {

    setorder(data, id, time)

    # weights time == 1
    tdata1 = data[time == 1]
    time_1_formula_1 = formula(paste0(exposure, " ~ 1"))
    time_1_formula_2 = formula(paste0(
                                      paste0(exposure, " ~ "),
                                      paste0(time_invariant, collapse = " + ")
                                      )
                               )
    time_1_model_1 = glm(formula = time_1_formula_1, data = tdata1)
    time_1_model_2 = glm(formula = time_1_formula_2, data = tdata1)

    kdens1 = dnorm(tdata1[[exposure]],
                   predict(time_1_model_1),
                   as.numeric(sd(time_1_model_1$residuals)))

    kdens2 = dnorm(tdata1[[exposure]],
                   predict(time_1_model_2),
                   as.numeric(sd(time_1_model_2$residuals)))

    weights_time_1 = kdens1 / kdens2
    remove(kdens1, kdens2)
    weights_time_1_trunc_1 = truncateWeights(weights_time_1, 0.01)
    weights_time_1_trunc_5 = truncateWeights(weights_time_1, 0.05)

    # time > 1 weights, add time invariant covs
    tdata = data[as.numeric(time) > 1]
    time_2_formula_1 = formula(paste0(
                                  paste0(exposure, " ~ "),
                                  paste0(c(baseline[2]),
                                         collapse = " + ")
                                  )
                               )

    # TODO: add time variant covs and time
    time_2_formula_2 = formula(paste0(
                                  paste0(exposure, " ~ "),
                                  paste0(c(time_invariant, baseline),
                                         collapse = " + ")
                                  )
                               )

    time_2_model_1 = glm(formula = time_2_formula_1, data = tdata)
    time_2_model_2 = glm(formula = time_2_formula_2, data = tdata)

    kdens1 = dnorm(tdata[[exposure]],
                   predict(time_2_model_1),
                   as.numeric(sd(time_2_model_1$residuals)))

    kdens2 = dnorm(tdata[[exposure]],
                   predict(time_2_model_2),
                   as.numeric(sd(time_2_model_2$residuals)))

    weights_time_2 = kdens1 / kdens2
    weights_time_2_trunc_1 = truncateWeights(weights_time_2, 0.01)
    weights_time_2_trunc_5 = truncateWeights(weights_time_2, 0.05)

    data[time == 1, `:=` (
                           tipw = weights_time_1,
                           tipwtrc1 = weights_time_1_trunc_1,
                           tipwtrc5 = weights_time_1_trunc_5
                           )
    ]

    data[as.numeric(time) > 1, `:=` (
                                     tipw = weights_time_2,
                                     tipwtrc1 = weights_time_2_trunc_1,
                                     tipwtrc5 = weights_time_2_trunc_5
                                     )
    ]

    setorder(data, id, time)

    data[, `:=`(
                ipw = cumprod(tipw),
                ipwtrc1 = cumprod(tipwtrc1),
                ipwtrc5 = cumprod(tipwtrc5)
                ),
    id]

    return(data[, c(id, time, "ipw", "ipwtrc1", "ipwtrc5"), with = FALSE])
}

tt = ipwC(mt1,
     exposure = "t",
     time = "time",
     baseline = c("lag_d", "lag_t"),
     time_invariant = "c",
     time_variant = ""
     )

tt
tt[, test := cumprod(tipw), id]
mt1[, c("ipw", "ipwtrc1", "ipwtrc5") := NULL]

mt1 = merge(mt1, tt, on = c("time", "id"))
summary(mt1)

m1 = glm(d ~ t + lag_t, data = mt1[time == 2])
m2 = glm(d ~ t + lag_t + lag_d + c, data = mt1[time == 2])
m3 = glm(d ~ t + lag_t + c, data = mt1[time == 2], weights = mt1[time == 2, ipw])
m4 = glm(d ~ t + lag_t + c, data = mt1[time == 2], weights = mt1[time == 2, ipwtrc1])
m5 = glm(d ~ t + lag_t + c, data = mt1[time == 2], weights = mt1[time == 2, ipwtrc5])

screenreg(list(m1, m2, m3, m4, m5))


# residual balance ipw
library(rbw)
library(data.table)
library(survey)

data(campaign_long)

summary(w2)
summary(truncateWeights(w2, 0.01))
data = data.table(campaign_long)

test_wide = data.table(campaign_wide)

test_wide
data

table(data$d.gone.neg)


m1 <- lm(dem.polls ~ (d.gone.neg.l1 + dem.polls.l1 + undother.l1) *  factor(week), data = campaign_long)

summary(m1)
m2 <- lm(undother ~ (d.gone.neg.l1 + dem.polls.l1 + undother.l1) * factor(week), data = campaign_long)
xmodels <- list(m1, m2)
summary(m1)

(data)
setorder(data, id, week)

ids = unique(data$id)
head(data[id == sample(ids, 1),
    .(id, week, dem.polls, undother, d.gone.neg.l1, neg.rep, d.gone.neg, dem.polls.l1, undother.l1)])

table(data$office)

# residual balancing weights
fit <- rbwPanel(exposure = d.gone.neg,
                xmodels = xmodels, id = id,
                time = week,
                data = campaign_long,
                max_iter = 500,
                tol = 0.001)

hist(fit$weights$rbw)

campaign_wide <- merge(campaign_wide, fit$weights, by = "id")

hist(fit$weights)

# fitting a marginal structural model
rbw_design <- svydesign(ids = ~ 1,
                        weights = ~ rbw,
                        data = campaign_wide)

msm_rbw <- svyglm(demprcnt ~ cum_neg * deminc + camp.length + factor(year) + office,
                  design = rbw_design)

summary(msm_rbw)

# library CBPS

library(CBPS)
data(Blackwell)

## Quickly fit a short model to test
form0 <- "d.gone.neg ~ d.gone.neg.l1 + camp.length"

fit0 <- CBMSM(formula = form0, time = Blackwell$time, id = Blackwell$demName,
            data = Blackwell, type="MSM",  iterations = NULL, twostep = TRUE,
            msm.variance = "approx", time.vary = TRUE)


# Fitting the models in Imai and Ratkovic (2014)
# Warning: may take a few mintues; setting time.vary to FALSE
# Results in a quicker fit but with poorer balance
# Usually, it is best to use time.vary TRUE
form1 <- "d.gone.neg ~ d.gone.neg.l1 + d.gone.neg.l2 + d.neg.frac.l3 +
         camp.length + deminc + base.poll + year.2002 +
          year.2004 + year.2006 + base.und + office"

## Note that  init="glm" gives the published results but the default is now init="opt"
fit1 <- CBMSM(formula = form1, time=Blackwell$time,id=Blackwell$demName,
            data=Blackwell, type="MSM",  iterations = NULL, twostep = TRUE,
            msm.variance = "full", time.vary = TRUE, init = "glm")

fit2 <- CBMSM(formula = form1, time=Blackwell$time,id=Blackwell$demName,
            data=Blackwell, type="MSM",  iterations = NULL, twostep = TRUE,
            msm.variance = "approx", time.vary = TRUE, init="glm")


## Assessing balance
bal1 <- balance.CBMSM(fit1)
bal2 <- balance.CBMSM(fit2)

length(fit2$weights)
##Effect estimation: Replicating Effect Estimates in
##Table 3 of Imai and Ratkovic (2014)

lm1 <-lm(demprcnt[time==1]~fit1$treat.hist,data=Blackwell,
         weights=fit1$glm.weights)
lm2 <- lm(demprcnt[time==1]~fit1$treat.hist,data=Blackwell,
          weights=fit1$weights)
lm3<-lm(demprcnt[time==1]~fit1$treat.hist,data=Blackwell,
        weights=fit2$weights)

lm4<-lm(demprcnt[time==1]~fit1$treat.cum,data=Blackwell,
weights=fit1$glm.weights)

lm5<-lm(demprcnt[time==1]~fit1$treat.cum,data=Blackwell,
weights=fit1$weights)

lm6<-lm(demprcnt[time==1]~fit1$treat.cum,data=Blackwell,
weights=fit$weights$rbw)


### Example: Multiple Binary Treatments Administered at the Same Time
n<-200
k<-4
set.seed(1040)
X1<-cbind(1,matrix(rnorm(n*k),ncol=k))

betas.1<-betas.2<-betas.3<-c(2,4,4,-4,3)/5
probs.1<-probs.2<-probs.3<-(1+exp(-X1 %*% betas.1))^-1

treat.1<-rbinom(n=length(probs.1),size=1,probs.1)
treat.2<-rbinom(n=length(probs.2),size=1,probs.2)
treat.3<-rbinom(n=length(probs.3),size=1,probs.3)
treat<-c(treat.1,treat.2,treat.3)
X<-rbind(X1,X1,X1)
time<-c(rep(1,nrow(X1)),rep(2,nrow(X1)),rep(3,nrow(X1)))
id<-c(rep(1:nrow(X1),3))
y<-cbind(treat.1,treat.2,treat.3) %*% c(2,2,2) +
X1 %*% c(-2,8,7,6,2) + rnorm(n,sd=5)

multibin1<-CBMSM(treat~X,id=id,time=time,type="MultiBin",twostep=TRUE)
summary(lm(y~-1+treat.1+treat.2+treat.3+X1, weights=multibin1$w))


library(data.table)

dt1 <- data.table(x = c("a", "b", "c", "d"), dt1_y = c(11.9, 21.4, 5.7, 18))
dt2 <- data.table(dt2_y = c(10, 15, 20), z = c("one", "two", "three"))

# add row ids and duplicate y
dt1[, `:=` (dt1_row_id = .I, joint_y = dt1_y)]
dt2[, `:=` (dt2_row_id = .I, joint_y = dt2_y)]

dt1
dt2

dt2[dt1, on = .(joint_y), roll = "nearest"]
dt2[dt1, on = .(joint_y), roll = T]

 data(LaLonde)
## Estimate CBPS
fit <- CBPS(treat ~ age + educ + re75 + re74 +
      I(re75==0) + I(re74==0),
    data = LaLonde, ATT = TRUE)
balance(fit)

head(LaLonde)


# testing ipwC function with real data

library(survey)
source("ch03/src/utils.R")

# relative mobility (continuous)

imp = readRDS('ch03/output/data/nlsy97_relative_mob_imputation.rds')

dat = data.table(mice::complete(imp, 1))
setorder(dat, id, year)

dat[, age_interview_est := as.numeric(as.character(age_interview_est))]

str(dat)
# create lag variables
lag_vars = c("imp_parent_employed", "imp_parent_married", "hhsize",
             "bmi", "health", "smoking_ever", "smoking_30", "log_population",
             "log_county_income", "z_relative_mob", "z_gini")


dat[, paste0("lag_", lag_vars) := lapply(.SD, shift), id,
    .SDcol = lag_vars]

# baseline vars
baseline_vars = c("log_income_adj")
dat[, paste0("baseline_", baseline_vars) := lapply(.SD, getFirst), id,
    .SDcol = baseline_vars]

# # impute first value backwards
# dat[, paste0("lag_", lag_vars) := lapply(.SD, impute_locf), id,
#     .SDcol = paste0("lag_", lag_vars)]

str(dat)

# exposure variables
dat[as.numeric(age_interview_est) <= 20, total_exposure_time :=
    sum(exposure_time), id]
dat[as.numeric(age_interview_est) <= 20,
    z_relative_mob_exposure :=
    sum(z_relative_mob * exposure_time) / sum(exposure_time), id]
dat[as.numeric(age_interview_est) <= 20,
    z_gini_exposure := sum(z_gini * exposure_time) / sum(exposure_time), id]

dat[, z_relative_mob_exposure := getMax(z_relative_mob_exposure), id]
dat[, z_gini_exposure := getMax(z_gini_exposure), id]
dat[, max_age_interview_est := factor(max_age_interview_est)]
dat[, health := factor(health)]
dat[, lag_health := factor(lag_health)]

# last wave and exposure datasets
last_wave = dat[year == 2015]
dat = dat[as.numeric(age_interview_est) <= 20]

table(dat$age_interview_est)
table(dat$time)

# dat = dat[as.numeric(age_interview_est) < as.numeric(max_age_interview_est)]

dat[, time := 1:.N, id]
dat[, cyear := year - mean(year)]
dat[, max_age_interview_est := fct_drop(max_age_interview_est)]

str(dat)

dat[, time := factor(time)]
table(dat$time)

time = "time"
time_invariant = c("male", "ethnicity", "max_age_interview_est", "parent_education",
                   "asvab_score", "mother_age_at_birth", "residential_moves_by_12")
baseline = c("baseline_log_income_adj")
time_variant = c("lag_imp_parent_employed", "log_income_adj", "lag_imp_parent_married",
                 "lag_hhsize", "lag_bmi", "lag_health", "lag_smoking_ever",
                 "lag_smoking_30", "lag_z_relative_mob")

tt = ipwC(dat,
     exposure = "z_relative_mob",
     time = "time",
     time_invariant = time_invariant,
     time_variant = time_variant
     )

summary(tt)

tt[, max_time := max(as.numeric(as.character(time))), id]
summary(tt)

tt = tt[max_time == time]
fwave = merge(last_wave, tt, on = "id")
fwave

svy_design_rel_mob = svydesign(ids = ~ 1, weights = ~ ipwt, data = fwave)
msm_rel_mob_bmi = svyglm(bmi ~ z_relative_mob_exposure +
                             male + ethnicity + max_age_interview_est +
                             parent_education + asvab_score + mother_age_at_birth +
                             residential_moves_by_12,
                             design = svy_design_rel_mob)

summary(msm_rel_mob_bmi)

# svy_design_gini = svydesign(ids = ~ 1, weights = ~ ipw_gini, data = fwave)

msm_rel_mob_depression = svyglm(depression ~ z_relative_mob_exposure +
                                male + ethnicity + max_age_interview_est +
                                parent_education + asvab_score + mother_age_at_birth +
                                residential_moves_by_12,
                                design = svy_design_rel_mob)

summary(msm_rel_mob_depression)

msm_rel_mob_health = svyolr(health ~ z_relative_mob_exposure +
                            male + ethnicity + max_age_interview_est +
                            parent_education + asvab_score + mother_age_at_birth +
                            residential_moves_by_12,
                            design = svy_design_rel_mob)

summary(msm_rel_mob_health)

summary(msm_rel_mob_depression)

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



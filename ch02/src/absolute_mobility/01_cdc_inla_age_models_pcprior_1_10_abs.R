# CDC mortality - income mobility
# author: Sebastina Daza

# R < 00dissertation/ch02/src/01_cdc_inla_age_models_pcprior_1_10_abs.R > 00dissertation/ch02/src/01_cdc_inla_age_models_pcprior_1_10_abs.log  --no-save  &

# load libraries
library(here)
library(sdazar)
library(INLA)
library(brinla)
library(ggplot2)
library(patchwork)
library(fmsb) # life tables
library(USAboundaries)
library(maptools)
library(spdep)
library(sp)
library(fmsb)
library(texreg)
# library(sf)

# load functions
setwd('00dissertation/ch02')
source('src/utils/functions.R')
county.adj = 'data/counties.graph' # for spatial models

# plot options
options(repr.plot.width = 5, repr.plot.height = 4) # plot options

# INLA node  options
(nodes = parallel::detectCores())
INLA:::inla.dynload.workaround()
inla.setOption("num.threads", (nodes/2) - 10)

# read data
data = readRDS('data/cdc_chetty.rds')
# fwrite(data, 'data/cdc_chetty.csv')

dim(data)
countmis(data)

# remove missing records
variables = c('z_relative_mob', 'z_absolute_mob', 'z_gini',
              'z_medicare_expenses', 'log_unemployment', 'z_uninsured',
              'log_pct_black', 'log_pct_hispanic')

data = data[complete.cases(data[, variables, with=FALSE])]
countmis(data)

data[, z_absolute_mob := z_absolute_mob * -1]

# get counties, US 2000
counties = us_counties("2000-07-01")
length(unique(data$fips)) / length(counties$fips)
counties = counties[counties$fips %in% unique(data$fips),]

all(as.character(counties$fips) %in% unique(data$fips))
all(unique(data$fips) %in% as.character(counties$fips))

ordered_fips = as.character(unique(counties$fips))
length(ordered_fips)

# order data based on counties
data = data[order(match(fips, ordered_fips))]

# create indicators for county and state
data[, county_i := .GRP, by = fips]
data[, state_i := .GRP, by = state]

length(unique(data[, county_i]))

############################
# aggregate data by gender
############################

male = data[sex==1]
female = data[sex==2]

amale = male[,
            .(state = first(state),
              deaths = sum(deaths),
              deaths1 = sum(deaths1),
              deaths2 = sum(deaths2),
              deaths3 = sum(deaths3),
              deaths4 = sum(deaths4),
              pop = sum(pop)),
            by = .(age, county)]

afemale = female[,
                .(state = first(state),
                  deaths = sum(deaths),
                  deaths1 = sum(deaths1),
                  deaths2 = sum(deaths2),
                  deaths3 = sum(deaths3),
                  deaths4 = sum(deaths4),
                  pop = sum(pop)),
                by = .(age, county)]

male_cov = male[, s := 1:.N, by = .(age, county)][s==1,][
    , c('pop','deaths', 'deaths1', 'deaths2', 'deaths3',
        'deaths4', 'state', 'race', 's', 'sex') := NULL]

female_cov = female[, s := 1:.N, by = .(age, county)][s==1,][
    , c('pop','deaths', 'deaths1', 'deaths2', 'deaths3',
        'deaths4', 'state', 'race', 's', 'sex') := NULL]

men = merge(amale, male_cov, by = c('age', 'county'))
women = merge(afemale, female_cov, by = c('age', 'county'))

# create indexes for random effects
men[, id := 1:.N]
men[, mob_age := age]
men[, gini_age := age]
men[, gini_county := county_i]
men[, mob_county:= county_i]

women[, id := 1:.N]
women[, mob_age := age]
women[, gini_age := age]
women[, gini_county := county_i]
women[, mob_county:= county_i]

# dimension of data
dim(men)
dim(women)

# labels for plots

age_groups = c('0-4','5-9','10-14','15-19','20-24',
              '25-39','30-34','35-39','40-44','45-49',
              '50-54','55-59','60-64','65-69','70-74',
              '75-79','80-84','85+')

e_groups =  c('E(0)','E(5)','E(10)','E(15)','E(20)',
              'E(25)','E(30)','E(35)','E(40)','E(45)',
              'E(50)','E(55)','E(60)','E(65)','E(70)',
              'E(75)','E(80)','E(85)')


##############################
# models
##############################

pcprior = list(prec = list(prior="pc.prec",
  param = c(1, 0.1)))

# baseline (0)

formula = deaths ~  1 + log_population + log_income + z_absolute_mob +
          z_gini +
        f(state_i, model = 'iid', hyper=pcprior) +
        f(county_i, model = 'iid', hyper=pcprior) +
        f(age, model = 'iid', hyper=pcprior) +
        f(id, model = 'iid', hyper=pcprior) # overdisperssion

m0 = inla(formula, data = men,
                family = 'poisson', E = pop,
                control.compute = list(dic=TRUE, waic = TRUE, cpo = TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

w0 = inla(formula, data = women,
                family = 'poisson', E = pop,
                control.compute = list(dic=TRUE, waic = TRUE, cpo = TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

round(m0$summary.fixed, 4)
bri.hyperpar.summary(m0)

round(w0$summary.fixed, 4)
bri.hyperpar.summary(w0)

# m0.resid = bri.Pois.resid(m0, plot = FALSE)
#
# savepdf('output/m0_resid_pcprior_1_10_abs')
#   qqnorm(m0.resid$resid)
#   qqline(m0.resid$resid)
# dev.off()
#
# savepdf('output/m0_pred_pvalues_pcprior_1_10_abs')
#   hist_pred_pvalues(m0, men$deaths) # pretty bad
# dev.off()
#
# savepdf('output/m0_cpo_pcprior_1_10_abs')
#   plot_loo(m0)
# dev.off()
#
# w0.resid = bri.Pois.resid(w0, plot = FALSE)
#
# savepdf('output/w0_resid_pcprior_1_10_abs')
#   qqnorm(w0.resid$resid)
#   qqline(w0.resid$resid)
# dev.off()
#
# savepdf('output/w0_pred_pvalues_pcprior_1_10_abs')
#   hist_pred_pvalues(w0, women$deaths) # pretty bad
# dev.off()
#
# savepdf('output/w0_cpo_pcprior_1_10_abs')
#   plot_loo(w0)
# dev.off()

# varying gini and mobility by age (1)

formula = deaths ~  1 + log_population + log_income + z_absolute_mob +
          z_gini +
        f(state_i, model = 'iid', hyper=pcprior) +
        f(county_i, model = 'iid', hyper=pcprior) +
        f(age, model = 'iid', hyper=pcprior) +
        f(id, model = 'iid', hyper=pcprior) +
        f(mob_age, z_absolute_mob, model = 'iid', hyper=pcprior) +
        f(gini_age, z_gini, model = 'iid', hyper=pcprior)

m1 = inla(formula, data = men,
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo= TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

w1 = inla(formula, data = women,
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo= TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

round(m1$summary.fixed, 4)
bri.hyperpar.summary(m1)

round(w1$summary.fixed, 4)
bri.hyperpar.summary(w1)

# plots
# sim_m1 = inla.posterior.sample(n=5000, result=m1)
# sim_w1 = inla.posterior.sample(n=5000, result=w1)

# savepdf('output/m1_coefficients_pcprior_1_10_abs')
# plot_fixed_coeff(m1, coeff=c('z_absolute_mob', 'z_gini'),
#   coeff_labels=c('Absolute Mobility', 'Gini'), exponential=TRUE)
# dev.off()

# savepdf('output/m1_random_effects__pcprior_1_10_abs')
# plot_random_dist(m1, terms=c('mob_age', 'gini_age'),
#   term_labels=c('Absolute Mobility', 'Gini'), exponential=FALSE)
# dev.off()
#
# savepdf('output/m1_effects_age_pcprior_1_10_abs')
# plot_random_fixed_effects_sim(sim_m1, fixed=c('z_absolute_mob', 'z_gini'),
#                               random=c('mob_age', 'gini_age'),
#                               x_labels=age_groups,
#                               ylabel='Rate ratio\n',
#                               xlabel='\nAge group',
#                               effects_labels = c('Mobility', 'Gini'),
#                               sorted=FALSE,
#                               exponential=TRUE)
# dev.off()
#
# savepdf('output/w1_effects_age_pcprior_1_10_abs')
# plot_random_fixed_effects_sim(sim_w1, fixed=c('z_absolute_mob', 'z_gini'),
#                               random=c('mob_age', 'gini_age'),
#                               x_labels=age_groups,
#                               ylabel='Rate ratio\n',
#                               xlabel='\nAge group',
#                               effects_labels = c('Mobility', 'Gini'),
#                               sorted=FALSE,
#                               exponential=TRUE)
# dev.off()
#
# # LE predictions
#
# value_matrix = rbind(
#     c(1, 0, 0,   1, 1, 0),
#     c(1, 1, 0,   1, 1, 0),
#     c(1, 0, 0,   1, 0, 1),
#     c(1, 0, 1,   1, 0, 1))
#
# rownames(value_matrix) = c('mob_0', 'mob_1', 'gini_0', 'gini_1')
#
# les = estimate_le_counterfactuals(sim_m1,
#   fixed=c('z_absolute_mob', 'z_gini'),
#   random=c('age', 'mob_age', 'gini_age'),
#   value_matrix=value_matrix)
#
# head(les)
#
# savepdf('output/m1_le_differences_pcprior_1_10_abs')
#   plot_le_counterfactuals(les, x_labels=e_groups)
# dev.off()
#
# les = estimate_le_counterfactuals(sim_w1,
#   fixed=c('z_absolute_mob', 'z_gini'),
#   random=c('age', 'mob_age', 'gini_age'),
#   value_matrix=value_matrix)
#
# head(les)
#
# savepdf('output/w1_le_differences_pcprior_1_10_abs')
#   plot_le_counterfactuals(les, x_labels=e_groups)
# dev.off()
#
# m1.resid = bri.Pois.resid(m1, plot = FALSE)
#
# savepdf('output/m1_resid_pcprior_1_10_abs')
#   qqnorm(m1.resid$resid)
#   qqline(m1.resid$resid)
# dev.off()
#
# savepdf('output/m1_predvalues_pcprior_1_10_abs')
#   hist_pred_pvalues(m1, men$deaths) # pretty bad
# dev.off()
#
# savepdf('output/m1_loo_pcprior_1_10_abs')
#   plot_loo(m1)
# dev.off()
#
# w1.resid = bri.Pois.resid(w1, plot = FALSE)
#
# savepdf('output/w1_resid_pcprior_1_10_abs')
#   qqnorm(w1.resid$resid)
#   qqline(w1.resid$resid)
# dev.off()
#
# savepdf('output/w1_predvalues_pcprior_1_10_abs')
#   hist_pred_pvalues(w1, women$deaths) # pretty bad
# dev.off()
#
# savepdf('output/w1_loo_pcprior_1_10_abs')
#   plot_loo(w1)
# dev.off()
#
# # random effects
# savepdf('output/m1_county_pcprior_1_10_abs')
#   plot_random_effects(m1, 'county_i', exponential=FALSE, sorted=TRUE)
# dev.off()
#
# savepdf('output/m1_state_pcprior_1_10_abs')
#   plot_random_effects(m1, 'state_i', exponential=FALSE, sorted=TRUE)
# dev.off()
#
# savepdf('output/w1_county_pcprior_1_10_abs')
#   plot_random_effects(w1, 'county_i', exponential=FALSE, sorted=TRUE)
# dev.off()
#
# savepdf('output/w1_state_pcprior_1_10_abs')
#   plot_random_effects(w1, 'state_i', exponential=FALSE, sorted=TRUE)
# dev.off()

# interaction mob x gini (2)

formula = deaths ~  1 + log_population + log_income + z_absolute_mob  * z_gini +
        f(state_i, model = 'iid', hyper=pcprior) +
        f(county_i, model = 'iid', hyper=pcprior) +
        f(age, model = 'iid', hyper=pcprior) +
        f(id, model = 'iid', hyper=pcprior) +
        f(mob_age, z_absolute_mob, model = 'iid', hyper=pcprior) +
        f(gini_age, z_gini, model = 'iid', hyper=pcprior)

m2 = inla(formula, data = men,
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo= TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

w2 = inla(formula, data = women,
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo= TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

round(m2$summary.fixed, 4)
bri.hyperpar.summary(m2)

round(w2$summary.fixed, 4)
bri.hyperpar.summary(w2)


# interaction income x mob  (3)

formula = deaths ~  1 + log_population + log_income * z_absolute_mob +  z_gini +
        f(state_i, model = 'iid', hyper=pcprior) +
        f(county_i, model = 'iid', hyper=pcprior) +
        f(age, model = 'iid', hyper=pcprior) +
        f(id, model = 'iid', hyper=pcprior) +
        f(mob_age, z_absolute_mob, model = 'iid', hyper=pcprior) +
        f(gini_age, z_gini, model = 'iid', hyper=pcprior)

m3 = inla(formula, data = men,
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo= TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

w3 = inla(formula, data = women,
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo= TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

round(m3$summary.fixed, 4)
bri.hyperpar.summary(m3)

round(w3$summary.fixed, 4)
bri.hyperpar.summary(w3)


# covariate adjustment (4)

formula = deaths ~  1 + log_population + log_income + z_absolute_mob +
          z_gini + z_segregation_income + log_unemployment + log_pct_hispanic +
          log_pct_black + z_uninsured + z_medicare_expenses +
        f(state_i, model = 'iid', hyper=pcprior) +
        f(county_i, model = 'iid', hyper=pcprior) +
        f(age, model = 'iid', hyper=pcprior) +
        f(id, model = 'iid', hyper=pcprior) +
        f(mob_age, z_absolute_mob, model = 'iid', hyper=pcprior) +
        f(gini_age, z_gini, model = 'iid', hyper=pcprior)

m4 = inla(formula, data = men,
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo=TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

w4 = inla(formula, data = women,
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo=TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

round(m4$summary.fixed, 4)
bri.hyperpar.summary(m4)

round(w4$summary.fixed, 4)
bri.hyperpar.summary(w4)

# savepdf('output/m2_coefficients')
# plot_fixed_coeff(m2, coeff=c('z_absolute_mob', 'z_gini'),
#   coeff_labels=c('Absolute Mobility', 'Gini'), exponential=TRUE)
# dev.off()
#
# savepdf('output/m2_random_effects')
# plot_random_dist(m2, terms=c('mob_age', 'gini_age'),
#   term_labels=c('Absolute Mobility', 'Gini'), exponential=FALSE)
# dev.off()

m4.resid = bri.Pois.resid(m4, plot = FALSE)

savepdf('output/m4_resid_pcprior_1_10_abs')
  qqnorm(m4.resid$resid)
  qqline(m4.resid$resid)
dev.off()

# savepdf('output/m4_predvalues_pcprior_1_10_abs')
#   hist_pred_pvalues(m4, men$deaths) # pretty bad
# dev.off()

savepdf('output/m4_loo_pcprior_1_10_abs')
  plot_loo(m4)
dev.off()

w4.resid = bri.Pois.resid(w4, plot = FALSE)

savepdf('output/w4_resid_pcprior_1_10_abs')
  qqnorm(w4.resid$resid)
  qqline(w4.resid$resid)
dev.off()

# savepdf('output/w4_predvalues_pcprior_1_10_abs')
#   hist_pred_pvalues(w4, women$deaths) # pretty bad
# dev.off()

savepdf('output/w4_loo_pcprior_1_10_abs')
  plot_loo(w4)
dev.off()

# better loo

# loo

# improved.m4 = inla.cpo(m4)
# improved.w4 = inla.cpo(w4)
#
# savepdf('output/m4_loo_pcprior_1_10_abs')
#   plot_loo(improved.m4)
# dev.off()
#
#
# savepdf('output/w4_loo_pcprior_1_10_abs')
#   plot_loo(improved.w4)
# dev.off()


savepdf('output/m4_coefficients_age_pcprior_1_10_abs')
print(plot_fixed_coeff(m4, coeff=c('z_absolute_mob', 'z_gini'),
  coeff_labels=c('Absolute Mobility', 'Gini'), exponential=TRUE) + xlim(0.95,1.15) + ylim(NA, 80) + xlab('\nPosterior distribution'))
dev.off()

savepdf('output/w4_coefficients_age_pcprior_1_10_abs')
print(plot_fixed_coeff(w4, coeff=c('z_absolute_mob', 'z_gini'),
  coeff_labels=c('Absolute Mobility', 'Gini'), exponential=TRUE) + xlim(0.95,1.15) + ylim(NA, 80) + xlab('\nPosterior distribution'))
dev.off()

# simulate

sim_m4 = inla.posterior.sample(n=5000, result=m4)
sim_w4 = inla.posterior.sample(n=5000, result=w4)

savepdf('output/m4_effects_age_pcprior_1_10_abs')
plot_random_fixed_effects_sim(sim_m4, fixed=c('z_absolute_mob', 'z_gini'),
                              random=c('mob_age', 'gini_age'),
                              x_labels=age_groups,
                              ylabel='Rate ratio\n',
                              xlabel='\nAge group',
                              effects_labels = c('Absolute Mobility', 'Gini'),
                              sorted=FALSE,
                              exponential=TRUE) + ylim(0.8, 1.3)
dev.off()

savepdf('output/w4_effects_age_pcprior_1_10_abs')
plot_random_fixed_effects_sim(sim_w4, fixed=c('z_absolute_mob', 'z_gini'),
                              random=c('mob_age', 'gini_age'),
                              x_labels=age_groups,
                              ylabel='Rate ratio\n',
                              xlabel='\nAge group',
                              effects_labels = c('Absolute Mobility', 'Gini'),
                              sorted=FALSE,
                              exponential=TRUE) + ylim(0.8, 1.3)
dev.off()

# LE predictions

value_matrix = rbind(
    c(1, 0, 0,   1, 1, 0),
    c(1, 1, 0,   1, 1, 0),
    c(1, 0, 0,   1, 0, 1),
    c(1, 0, 1,   1, 0, 1))

rownames(value_matrix) = c('mob_0', 'mob_1', 'gini_0', 'gini_1')

les_m = estimate_le_counterfactuals(sim_m4,
  fixed=c('z_absolute_mob', 'z_gini'),
  random=c('age', 'mob_age', 'gini_age'),
  value_matrix=value_matrix)

head(les_m)

savepdf('output/m4_le_differences_pcprior_1_10_abs')
  plot_le_counterfactuals(les_m, x_labels=e_groups,
    name_groups=c('Absolute Mobility', 'Gini')) +
     ylim(-1.5, 0.5)
dev.off()

savepdf('output/m4_le_re_differences_pcprior_1_10_abs')
  plot_le_counterfactuals(les_m, relative=TRUE, x_labels=e_groups,
  name_groups=c('Absolute Mobility', 'Gini')) +
     ylim(-0.13, 0.05)
dev.off()

les_w = estimate_le_counterfactuals(sim_w4,
  fixed=c('z_absolute_mob', 'z_gini'),
  random=c('age', 'mob_age', 'gini_age'),
  value_matrix=value_matrix)

head(les_w)

savepdf('output/w4_le_differences_pcprior_1_10_abs')
  plot_le_counterfactuals(les_w, x_labels=e_groups,
    name_groups=c('Absolute Mobility', 'Gini')) +
     ylim(-1.5, 0.5)
dev.off()

savepdf('output/w4_le_re_differences_pcprior_1_10_abs')
  plot_le_counterfactuals(les_w, relative=TRUE, x_labels=e_groups,
  name_groups=c('Absolute Mobility', 'Gini')) +
     ylim(-0.13, 0.05)
dev.off()

# spatial (5)

formula = deaths ~  1 + log_population + log_income + z_absolute_mob +
          z_gini + z_segregation_income + log_unemployment + log_pct_hispanic +
          log_pct_black + z_uninsured + z_medicare_expenses +
        f(state_i, model = 'iid', hyper=pcprior) +
        f(county_i, model = 'bym2', graph = county.adj, hyper=pcprior) +
        f(age, model = 'iid', hyper=pcprior) +
        f(id, model = 'iid', hyper=pcprior) +
        f(mob_age, z_absolute_mob, model = 'iid', hyper=pcprior) +
        f(gini_age, z_gini, model = 'iid', hyper=pcprior)

m5 = inla(formula, data = men,
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo=TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

w5 = inla(formula, data = women,
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo=TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

round(m5$summary.fixed, 4)
bri.hyperpar.summary(m5)

round(w5$summary.fixed, 4)
bri.hyperpar.summary(w5)

# create table
source('src/utils/extract_inla.R')

cnames = list(
               '(Intercept)' = 'Constant',
               z_absolute_mob = 'Income absolute mobility',
               z_gini = 'Gini',
               log_income = 'Log income',
               'z_absolute_mob:z_gini' = 'Absolute mobility x Gini',
               'log_income:z_absolute_mob' = 'Absolute mobility x Log income',
               'sd for id' = 'SD observations',
               'sd for age' = 'SD age group',
               'sd for county_i' = 'SD counties',
               'Phi for county_i' = 'Phi counties',
               'sd for state_i' = 'SD states',
               'sd for mob_age' = 'SD mobility by age',
               'sd for gini_age' = 'SD gini by age')


cmodels = c('Baseline', 'Varying-Coefficient',
            'Absolute mobility x Gini', 'Absolute mobility x Income',
             'Covariates', 'Spatial')

# men

m_models = list(m0,m1,m2,m3,m4,m5)

texreg(m_models,
            include.dic = TRUE, include.waic = TRUE,
            ci.test = FALSE,
            float.pos = "htp",
            caption = "County Level Poisson Models Absolute Mobility \\newline PC Prior $= Pr(\\sigma > 1) < 0.10$, Men, CDC 2000-2014",
            booktabs = TRUE,
            use.packages = FALSE,
            dcolumn = TRUE,
            caption.above = TRUE,
            scalebox = 0.65,
            label = 'tbl:m_age_pcprior_1_10_abs',
            sideways = TRUE,
            digits = 2,
            custom.model.names = cmodels,
            custom.coef.map = cnames,
            groups = list("Random Effects" = c(7:13)),
            custom.note = "Note: Selected coefficients (mean of marginal posterior distribution).
            Poisson model with offset = \\texttt{log(population)}. 95\\% credibility intervals.",
             file = "output/m_age_pcprior_1_10_abs.tex")


# women
w_models = list(w0,w1,w2,w3,w4,w5)

texreg(w_models,
           include.dic = TRUE, include.waic = TRUE,
           ci.test = FALSE,
           float.pos = "htp",
           caption = "County Level Poisson Models Absolute Mobility \\newline PC prior $= Pr(\\sigma > 1) < 0.10$, Women, CDC 2000-2014",
           booktabs = TRUE,
           use.packages = FALSE,
           dcolumn = TRUE,
           caption.above = TRUE,
           scalebox = 0.65,
           label = 'tbl:w_age_pcprior_1_10_abs',
           sideways = TRUE,
           digits = 2,
           custom.model.names = cmodels,
           custom.coef.map = cnames,
           groups = list("Random Effects" = c(7:13)),
           custom.note = "Note: Selected coefficients (mean of marginal posterior distribution).
           Poisson model with offset = \\texttt{log(population)}. 95\\% credibility intervals.",
            file = "output/w_age_pcprior_1_10_abs.tex")

######################
# end models
######################

# CDC mortality - income mobility
# author: Sebastian Daza

# R < 00dissertation/ch02/src/03_cdc_inla_age_cause_modelsw.R > 00dissertation/ch02/src/03_cdc_inla_age_cause_models_pcprior_1_10.log --no-save  &

# load libraries
library(here)
library(sdazar)
library(INLA)
library(brinla)
library(ggplot2)
library(ggridges)
library(patchwork)
library(fmsb) # life table  s
library(USAboundaries)
library(maptools)
library(spdep)
library(sp)
library(fmsb)
library(stringr)
# library(sf)

# load functions
setwd('00dissertation/ch02')
source('src/utils/functions.R')

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

data = data[, z_absolute_mob := z_absolute_mob * -1]

# expand data by cause

dc = melt(data,
  measure=patterns('^deaths[0-9]'),
  value.name = c('deaths_by_cause'),
  variable.name = 'cause')

# create indicators for county and state
dc[, county_i := .GRP, by = fips]
dc[, state_i := .GRP, by = state]
dc[, cause_i := .GRP, by = cause]

table(dc$cause_i, useNA='ifany')

# aggregate data by gender

# that means not taking into account race differences for now
male = dc[sex==1]
female = dc[sex==2]

amale = male[,
            .(state = first(state),
              deaths = sum(deaths_by_cause),
              pop = sum(pop)),
            by = .(county_i, age, cause_i)]

afemale = female[,
            .(state = first(state),
              deaths = sum(deaths_by_cause),
              pop = sum(pop)),
            by = .(county_i, age, cause_i)]

male_cov = male[, s := 1:.N, by = .(cause_i, age, county_i)][s==1,][
    , c('pop','deaths', 'state', 'race', 's', 'sex') := NULL]

female_cov = female[, s := 1:.N, by = .(cause_i, age, county_i)][s==1,][
    , c('pop','deaths', 'state', 'race', 's', 'sex') := NULL]


dim(male_cov)
dim(female_cov)

men = merge(amale, male_cov, by = c('age', 'county_i', 'cause_i'))
women = merge(afemale, female_cov, by = c('age', 'county_i', 'cause_i'))

dim(men)
dim(women)

# create indexes for random effects
men[, id := 1:.N]
men[, mob_age := age]
men[, gini_age := age]
men[, gini_county := county_i]
men[, mob_county:= county_i]
men[, gini_cause := cause_i]
men[, mob_cause := cause_i]

women[, id := 1:.N]
women[, mob_age := age]
women[, gini_age := age]
women[, gini_county := county_i]
women[, mob_county:= county_i]
women[, gini_cause := cause_i]
women[, mob_cause := cause_i]

##############################
# models
##############################

pcprior = list(prec = list(prior="pc.prec",
  param = c(1, 0.10)))

age_groups = c('0-4','5-9','10-14','15-19','20-24',
                '25-39','30-34','35-39','40-44','45-49',
                '50-54','55-59','60-64','65-69','70-74',
                '75-79','80-84','85+')

cause_groups = c('Communicable', 'Non-communicable',
'Injury', 'Residuals')



######################
# analysis by cause
######################

formula = deaths ~  1 + log_population + log_income + z_absolute_mob +
          z_gini + z_segregation_income + log_unemployment + log_pct_hispanic +
          log_pct_black + z_uninsured + z_medicare_expenses +
        f(state_i, model = 'iid', hyper=pcprior) +
        f(county_i, model = 'iid', hyper=pcprior) +
        f(age, model = 'iid', hyper=pcprior) +
        f(id, model = 'iid', hyper=pcprior) +
        f(mob_age, z_absolute_mob, model = 'iid', hyper=pcprior) +
        f(gini_age, z_gini, model = 'iid', hyper=pcprior)

# cause 1

m2 = inla(formula, data = men[cause_i==1],
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo=TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

w2 = inla(formula, data = women[cause_i==1],
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo=TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

round(m2$summary.fixed, 4)
bri.hyperpar.summary(m2)

round(w2$summary.fixed, 4)
bri.hyperpar.summary(w2)

# savepdf('output/m_coeff_age_cause_pcprior_1_10_1')
# print(plot_fixed_coeff(m2, coeff=c('z_absolute_mob', 'z_gini'),
#   coeff_labels=c('Absolute Mobility', 'Gini'), exponential=TRUE) + xlim(0.95,1.15) + ylim(NA, 60) + xlab('\nPosterior distribution'))
# dev.off()
#
# savepdf('output/w_coeff_age_cause_pcprior_1_10_1')
# print(plot_fixed_coeff(w2, coeff=c('z_absolute_mob', 'z_gini'),
#   coeff_labels=c('Absolute Mobility', 'Gini'), exponential=TRUE) + xlim(0.95,1.15) + ylim(NA, 60) + xlab('\nPosterior distribution'))
# dev.off()

# cause 2

m3 = inla(formula, data = men[cause_i==2],
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo=TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

w3 = inla(formula, data = women[cause_i==2],
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo=TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

# round(m3$summary.fixed, 4)
# bri.hyperpar.summary(m2)
#
# round(w3$summary.fixed, 4)
# bri.hyperpar.summary(w2)
#
# savepdf('output/m_coeff_age_cause_pcprior_1_10_2')
# print(plot_fixed_coeff(m3, coeff=c('z_absolute_mob', 'z_gini'),
#   coeff_labels=c('Absolute Mobility', 'Gini'), exponential=TRUE) + xlim(0.95,1.15) + ylim(NA, 60) + xlab('\nPosterior distribution'))
# dev.off()
#
# savepdf('output/w_coeff_age_cause_pcprior_1_10_2')
# print(plot_fixed_coeff(w3, coeff=c('z_absolute_mob', 'z_gini'),
#   coeff_labels=c('Absolute Mobility', 'Gini'), exponential=TRUE) + xlim(0.95,1.15) + ylim(NA, 60) + xlab('\nPosterior distribution'))
# dev.off()

# cause 3

m4 = inla(formula, data = men[cause_i==3],
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo=TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

w4 = inla(formula, data = women[cause_i==3],
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo=TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

# round(m4$summary.fixed, 4)
# bri.hyperpar.summary(m4)

# round(w4$summary.fixed, 4)
# bri.hyperpar.summary(w4)

# savepdf('output/m_coeff_age_cause_pcprior_1_10_3')
# print(plot_fixed_coeff(m4, coeff=c('z_absolute_mob', 'z_gini'),
#   coeff_labels=c('Absolute Mobility', 'Gini'), exponential=TRUE) + xlim(0.95,1.15) + ylim(NA, 60) + xlab('\nPosterior distribution'))
# dev.off()
#
# savepdf('output/w_coeff_age_cause_pcprior_1_10_3')
# print(plot_fixed_coeff(w4, coeff=c('z_absolute_mob', 'z_gini'),
#   coeff_labels=c('Absolute Mobility', 'Gini'), exponential=TRUE) + xlim(0.95,1.15) + ylim(NA, 60) + xlab('\nPosterior distribution'))
# dev.off()

# cause 4

m5 = inla(formula, data = men[cause_i==4],
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo=TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

w5 = inla(formula, data = women[cause_i==4],
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo=TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

# round(m5$summary.fixed, 4)
# bri.hyperpar.summary(m5)
#
# round(w5$summary.fixed, 4)
# bri.hyperpar.summary(w5)

# savepdf('output/m_coeff_age_cause_pcprior_1_10_4')
# print(plot_fixed_coeff(m5, coeff=c('z_absolute_mob', 'z_gini'),
#   coeff_labels=c('Absolute Mobility', 'Gini'), exponential=TRUE) + xlim(0.95,1.15) + ylim(NA, 60) + xlab('\nPosterior distribution'))
# dev.off()
#
# savepdf('output/w_coeff_age_cause_pcprior_1_10_4')
# print(plot_fixed_coeff(w5, coeff=c('z_absolute_mob', 'z_gini'),
#   coeff_labels=c('Absolute Mobility', 'Gini'), exponential=TRUE) + xlim(0.95,1.15) + ylim(NA, 60) + xlab('\nPosterior distribution'))
# dev.off()
#

# create plots

# get simulated data
models = list(m2, m3, m4, m5)
m_sim_list = list()
for (i in 1:length(models)) {
  print(paste0('::: Model ', i, ' :::'))
  m_sim_list[[i]] = inla.posterior.sample(2000, models[[i]])
}

models = list(w2, w3, w4, w5)
w_sim_list = list()
for (i in 1:length(models)) {
  print(paste0('::: Model ', i, ' :::'))
  w_sim_list[[i]] = inla.posterior.sample(2000, models[[i]])
}


m_plot_data = get_fixed_sim(m_sim_list,
  coeff=c('z_absolute_mob', 'z_gini'),
  coeff_labels=c('Absolute Mobility', 'Gini'),
  groups_labels=cause_groups)

head(m_plot_data, 10)

w_plot_data = get_fixed_sim(w_sim_list,
  coeff=c('z_absolute_mob', 'z_gini'),
  coeff_labels=c('Absolute Mobility', 'Gini'),
  groups_labels=cause_groups)

head(w_plot_data, 10)

savepdf('output/m_cause_dist_pcprior_1_10_abs')
plot_fixed_sim(m_plot_data) + xlim(.95, 1.3)
dev.off()

savepdf('output/w_cause_dist_pcprior_1_10_abs')
plot_fixed_sim(w_plot_data) + xlim(.95, 1.3)
dev.off()

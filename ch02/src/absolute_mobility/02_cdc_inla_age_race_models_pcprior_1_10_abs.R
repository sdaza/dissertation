# CDC mortality - income mobility
# author: Sebastian Daza

# R < 00dissertation/ch02/src/02_cdc_inla_age_race_models_pcprior_1_10_abs.R > 00dissertation/ch02/src/02_cdc_inla_age_race_models_pcprior_1_10_abs.log  --no-save  &

# load libraries
library(here)
library(sdazar)
library(INLA)
library(brinla)
library(ggplot2)
library(ggridges)
library(patchwork)
library(fmsb) # life tables
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
(nodes <- parallel::detectCores())
INLA:::inla.dynload.workaround()
inla.setOption("num.threads", (nodes/2) - 7)

# read data

data = readRDS('data/cdc_chetty.rds')
# fwrite(data, 'data/cdc_chetty.csv')

dim(data)
countmis(data)

# remove missing records
variables = c('z_relative_mob', 'z_absolute_mob', 'z_gini',  'z_medicare_expenses', 'log_unemployment', 'z_uninsured',
              'log_pct_black', 'log_pct_hispanic')

data = data[complete.cases(data[, variables, with=FALSE])]
countmis(data)

data[, z_absolute_mob := z_absolute_mob * -1]

length(unique(data$county))
names(data)

# create indicators for county and state

data[, county_i := .GRP, by = fips]
data[, state_i := .GRP, by = state]
data[, race_i := .GRP, by = race]

table(data$race, useNA='ifany')

data[race==1, race_i := 1]
data[race==2, race_i := 4]
data[race==3, race_i := 2]
data[race==4, race_i := 3]

# compute rates just to check

table(data$race, data$race_i)

# get data by gender

men = data[sex==1]
women = data[sex==2]

dim(men)
dim(women)

# create indexes for random effects
men[, id := 1:.N]
men[, mob_age := age]
men[, gini_age := age]
men[, gini_county := county_i]
men[, mob_county:= county_i]
men[, gini_race := race_i]
men[, mob_race := race_i]

women[, id := 1:.N]
women[, mob_age := age]
women[, gini_age := age]
women[, gini_county := county_i]
women[, mob_county:= county_i]
women[, gini_race := race_i]
women[, mob_race := race_i]

# labels for plots

e_groups =  c('E(0)','E(5)','E(10)','E(15)','E(20)',
              'E(25)','E(30)','E(35)','E(40)','E(45)',
              'E(50)','E(55)','E(60)','E(65)','E(70)',
              'E(75)','E(80)','E(85)')

race_groups = c('White', 'African-American', 'Hispanic', 'Other')

##############################
# models
##############################

# set prior

pcprior = list(prec = list(prior="pc.prec",
  param = c(1, 0.10)))

# covariate adjustment (1) + varying coefficients

# formula = deaths ~  1 + log_population + log_income + z_absolute_mob +
#           z_gini + z_segregation_income + log_unemployment + log_pct_hispanic +
#           log_pct_black + z_uninsured + z_medicare_expenses +
#           f(state_i, model = 'iid', hyper=pcprior) +
#           f(county_i, model = 'iid', hyper=pcprior) +
#           f(race_i, model='iid') +
#           f(age, model = 'iid', hyper=pcprior) +
#           f(id, model = 'iid', hyper=pcprior) +
#           f(mob_age, z_absolute_mob, model = 'iid', hyper=pcprior) +
#           f(gini_age, z_gini, model = 'iid', hyper=pcprior) +
#           f(mob_race, z_absolute_mob, model = 'iid', hyper=pcprior) +
#           f(gini_race, z_gini, model = 'iid', hyper=pcprior)
#
#
# m1 = inla(formula, data = women,
#                 family = 'poisson', E = pop,
#                 control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
#                   cpo= TRUE),
#                 control.predictor = list(link = 1, compute = TRUE),
#                 control.inla = list(strategy = 'gaussian'))
#
# w1 = inla(formula, data = women,
#                 family = 'poisson', E = pop,
#                 control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
#                   cpo= TRUE),
#                 control.predictor = list(link = 1, compute = TRUE),
#                 control.inla = list(strategy = 'gaussian'))
#
# round(m1$summary.fixed, 4)
# bri.hyperpar.summary(m1)
#
# round(w1$summary.fixed, 4)
# bri.hyperpar.summary(w1)

# plots

# sim_m1 = inla.posterior.sample(n=5000, result=m1)
# sim_w1 = inla.posterior.sample(n=5000, result=w1)
#
# savepdf('output/m1_effects_age_race_dist_pcprior_1_10_abs')
# plot_dist_fixed_random_sim(sim_m1,
#   fixed=c('z_absolute_mob', 'z_gini'),
#   random=c('mob_race', 'gini_race'),
#   names_group=race_groups,
#   names_random=c('Absolute Mobility', 'Gini'),
# ) + xlab('\nPosterior distribution')
# dev.off()
#
# savepdf('output/w1_effects_age_race_dist_pcprior_1_10_abs')
# plot_dist_fixed_random_sim(sim_w1,
#   fixed=c('z_absolute_mob', 'z_gini'),
#   random=c('mob_race', 'gini_race'),
#   names_group=race_groups,
#   names_random=c('Absolute Mobility', 'Gini')
#     ) + xlab('\nPosterior distribution')
# dev.off()

######################
# analysis by race
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

# white

m2 = inla(formula, data = men[race_i==1],
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo=TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

w2 = inla(formula, data = women[race_i==1],
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo=TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

# round(m2$summary.fixed, 4)
# bri.hyperpar.summary(m2)
#
# round(w2$summary.fixed, 4)
# bri.hyperpar.summary(w2)

# savepdf('output/m_coeff_age_race_pcprior_1_10_1')
# print(plot_fixed_coeff(m2, coeff=c('z_absolute_mob', 'z_gini'),
#   coeff_labels=c('Absolute Mobility', 'Gini'), exponential=TRUE) + xlim(0.95,1.15) + ylim(NA, 60) + xlab('\nPosterior distribution'))
# dev.off()
#
# savepdf('output/w_coeff_age_race_pcprior_1_10_1')
# print(plot_fixed_coeff(w2, coeff=c('z_absolute_mob', 'z_gini'),
#   coeff_labels=c('Absolute Mobility', 'Gini'), exponential=TRUE) + xlim(0.95,1.15) + ylim(NA, 60) + xlab('\nPosterior distribution'))
# dev.off()

# black

m3 = inla(formula, data = men[race_i==2],
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo=TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

w3 = inla(formula, data = women[race_i==2],
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
# savepdf('output/m_coeff_age_race_pcprior_1_10_2')
# print(plot_fixed_coeff(m3, coeff=c('z_absolute_mob', 'z_gini'),
#   coeff_labels=c('Absolute Mobility', 'Gini'), exponential=TRUE) + xlim(0.95,1.15) + ylim(NA, 60) + xlab('\nPosterior distribution'))
# dev.off()
#
# savepdf('output/w_coeff_age_race_pcprior_1_10_2')
# print(plot_fixed_coeff(w3, coeff=c('z_absolute_mob', 'z_gini'),
#   coeff_labels=c('Absolute Mobility', 'Gini'), exponential=TRUE) + xlim(0.95,1.15) + ylim(NA, 60) + xlab('\nPosterior distribution'))
# dev.off()

# hispanic

m4 = inla(formula, data = men[race_i==3],
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo=TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

w4 = inla(formula, data = women[race_i==3],
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo=TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

# round(m4$summary.fixed, 4)
# bri.hyperpar.summary(m4)
#
# round(w4$summary.fixed, 4)
# bri.hyperpar.summary(w4)
#
# savepdf('output/m_coeff_age_race_pcprior_1_10_3')
# print(plot_fixed_coeff(m4, coeff=c('z_absolute_mob', 'z_gini'),
#   coeff_labels=c('Absolute Mobility', 'Gini'), exponential=TRUE) + xlim(0.95,1.15) + ylim(NA, 60) + xlab('\nPosterior distribution'))
# dev.off()
#
# savepdf('output/w_coeff_age_race_pcprior_1_10_3')
# print(plot_fixed_coeff(w4, coeff=c('z_absolute_mob', 'z_gini'),
#   coeff_labels=c('Absolute Mobility', 'Gini'), exponential=TRUE) + xlim(0.95,1.15) + ylim(NA, 60) + xlab('\nPosterior distribution'))
# dev.off()

# other

m5 = inla(formula, data = men[race_i==4],
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo=TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

w5 = inla(formula, data = women[race_i==4],
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

# savepdf('output/m_coeff_age_race_pcprior_1_10_4')
# print(plot_fixed_coeff(m5, coeff=c('z_absolute_mob', 'z_gini'),
#   coeff_labels=c('Absolute Mobility', 'Gini'), exponential=TRUE) + xlim(0.95,1.15) + ylim(NA, 60) + xlab('\nPosterior distribution'))
# dev.off()
#
# savepdf('output/w_coeff_age_race_pcprior_1_10_4')
# print(plot_fixed_coeff(w5, coeff=c('z_absolute_mob', 'z_gini'),
#   coeff_labels=c('Absolute Mobility', 'Gini'), exponential=TRUE) + xlim(0.95,1.15) + ylim(NA, 60) + xlab('\nPosterior distribution'))
# dev.off()
#

# create plots

# simulations
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

# plots
m_plot_data = get_fixed_sim(m_sim_list,
  coeff=c('z_absolute_mob', 'z_gini'),
  coeff_labels=c('Absolute Mobility', 'Gini'),
  groups_labels=race_groups)

head(m_plot_data, 10)

w_plot_data = get_fixed_sim(w_sim_list,
  coeff=c('z_absolute_mob', 'z_gini'),
  coeff_labels=c('Absolute Mobility', 'Gini'),
  groups_labels=race_groups)

head(w_plot_data, 10)

savepdf('output/m_race_dist_pcprior_1_10_abs')
plot_fixed_sim(m_plot_data) + xlim(.90, 1.2)
dev.off()

savepdf('output/w_race_dist_pcprior_1_10_abs')
plot_fixed_sim(w_plot_data) + xlim(.90, 1.2)
dev.off()

# le differences

value_matrix = rbind(
    c(1, 0, 0,   1, 1, 0),
    c(1, 1, 0,   1, 1, 0),
    c(1, 0, 0,   1, 0, 1),
    c(1, 0, 1,   1, 0, 1))

rownames(value_matrix) = c('mob_0', 'mob_1', 'gini_0', 'gini_1')

# men
m_les = list()
for (m in 1:length(m_sim_list)) {
  t = estimate_le_counterfactuals(m_sim_list[[m]],
    fixed=c('z_absolute_mob', 'z_gini'),
    random=c('age', 'mob_age', 'gini_age'),
    value_matrix=value_matrix)
  t[, group := race_groups[m]]
  m_les[[m]] = t
}

# absolute

les = rbindlist(m_les)
les[, mob := mob_1 - mob_0]
les[, gini := gini_1 - gini_0]

s = les[, .(m=median(mob),
      lo=quantile(mob, 0.025),
      hi=quantile(mob, 0.975)),
      by = .(age, group)]
s[, age := factor(age, labels=e_groups)]

savepdf('output/m_le_differences_race_mob_pcprior_1_10_abs')
ggplot(s[group %in% c('White', 'African-American')], aes(x=age, y=m)) +
   geom_ribbon(aes(ymin=lo, ymax=hi, group=group, fill=group), alpha=0.1)  +
   geom_point(aes(color=group), size=0.9, alpha=0.3)  +
   geom_line(aes(color=group, group=group), size = 0.4, alpha=0.3)  +
   theme_classic() +
   scale_color_manual(values=c("#0072B2", "#D55E00")) +
   scale_fill_manual(values=c("#0072B2", "#D55E00")) +
   labs(x='\nLife expectancy at age', y='Difference in years\n') +
   geom_hline(yintercept = 0,  size = 0.2, alpha = 0.3, linetype = 2) +
   theme(axis.text.x = element_text(angle = 40, hjust = 1),
   legend.position="top",
   legend.title=element_blank()) +
   ylim(-2.5, 0.35)
dev.off()

s = les[, .(m=median(gini),
      lo=quantile(gini, 0.025),
      hi=quantile(gini, 0.975)),
      by = .(age, group)]
s[, age := factor(age, labels=e_groups)]

savepdf('output/m_le_differences_race_gini_pcprior_1_10_abs')
ggplot(s[group %in% c('White', 'African-American')], aes(x=age, y=m)) +
   geom_ribbon(aes(ymin=lo, ymax=hi, group=group, fill=group), alpha=0.1)  +
   geom_point(aes(color=group), size=0.9, alpha=0.3)  +
   geom_line(aes(color=group, group=group), size = 0.4, alpha=0.3)  +
   theme_classic() +
   scale_color_manual(values=c("#0072B2", "#D55E00")) +
   scale_fill_manual(values=c("#0072B2", "#D55E00")) +
   labs(x='\nLife expectancy at age', y='Difference in years\n') +
   geom_hline(yintercept = 0,  size = 0.2, alpha = 0.3, linetype = 2) +
   theme(axis.text.x = element_text(angle = 40, hjust = 1),
   legend.position="top",
   legend.title=element_blank()) +
   ylim(-2.5, 0.35)
dev.off()


# relative

les = rbindlist(m_les)
les[, mob := (mob_1 - mob_0)/mob_0]
les[, gini := (gini_1 - gini_0)/gini_0]

s = les[, .(m=median(mob),
      lo=quantile(mob, 0.025),
      hi=quantile(mob, 0.975)),
      by = .(age, group)]
s[, age := factor(age, labels=e_groups)]

savepdf('output/m_le_re_differences_race_mob_pcprior_1_10_abs')
ggplot(s[group %in% c('White', 'African-American')], aes(x=age, y=m)) +
   geom_ribbon(aes(ymin=lo, ymax=hi, group=group, fill=group), alpha=0.1)  +
   geom_point(aes(color=group), size=0.9, alpha=0.3)  +
   geom_line(aes(color=group, group=group), size = 0.4, alpha=0.3)  +
   theme_classic() +
   scale_color_manual(values=c("#0072B2", "#D55E00")) +
   scale_fill_manual(values=c("#0072B2", "#D55E00")) +
   labs(x='\nLife expectancy at age', y='Difference in years\n') +
   geom_hline(yintercept = 0,  size = 0.2, alpha = 0.3, linetype = 2) +
   theme(axis.text.x = element_text(angle = 40, hjust = 1),
   legend.position="top",
   legend.title=element_blank()) +
   ylim(-.15, 0.05)
dev.off()

s = les[, .(m=median(gini),
      lo=quantile(gini, 0.025),
      hi=quantile(gini, 0.975)),
      by = .(age, group)]
s[, age := factor(age, labels=e_groups)]

savepdf('output/m_le_re_differences_race_gini_pcprior_1_10_abs')
ggplot(s[group %in% c('White', 'African-American')], aes(x=age, y=m)) +
   geom_ribbon(aes(ymin=lo, ymax=hi, group=group, fill=group), alpha=0.1)  +
   geom_point(aes(color=group), size=0.9, alpha=0.3)  +
   geom_line(aes(color=group, group=group), size = 0.4, alpha=0.3)  +
   theme_classic() +
   scale_color_manual(values=c("#0072B2", "#D55E00")) +
   scale_fill_manual(values=c("#0072B2", "#D55E00")) +
   labs(x='\nLife expectancy at age', y='Difference in years\n') +
   geom_hline(yintercept = 0,  size = 0.2, alpha = 0.3, linetype = 2) +
   theme(axis.text.x = element_text(angle = 40, hjust = 1),
   legend.position="top",
   legend.title=element_blank()) +
   ylim(-.15, 0.05)
dev.off()


# women

w_les = list()
for (m in 1:length(w_sim_list)) {
  t = estimate_le_counterfactuals(w_sim_list[[m]],
    fixed=c('z_absolute_mob', 'z_gini'),
    random=c('age', 'mob_age', 'gini_age'),
    value_matrix=value_matrix)
  t[, group := race_groups[m]]
  w_les[[m]] = t
}

# absolute

les = rbindlist(w_les)
les[, mob := mob_1 - mob_0]
les[, gini := gini_1 - gini_0]

s = les[, .(m=median(mob),
      lo=quantile(mob, 0.025),
      hi=quantile(mob, 0.975)),
      by = .(age, group)]
s[, age := factor(age, labels=e_groups)]

savepdf('output/w_le_differences_race_mob_pcprior_1_10_abs')
ggplot(s[group %in% c('White', 'African-American')], aes(x=age, y=m)) +
   geom_ribbon(aes(ymin=lo, ymax=hi, group=group, fill=group), alpha=0.1)  +
   geom_point(aes(color=group), size=0.9, alpha=0.3)  +
   geom_line(aes(color=group, group=group), size = 0.4, alpha=0.3)  +
   theme_classic() +
   scale_color_manual(values=c("#0072B2", "#D55E00")) +
   scale_fill_manual(values=c("#0072B2", "#D55E00")) +
   labs(x='\nLife expectancy at age', y='Difference in years\n') +
   geom_hline(yintercept = 0,  size = 0.2, alpha = 0.3, linetype = 2) +
   theme(axis.text.x = element_text(angle = 40, hjust = 1),
   legend.position="top",
   legend.title=element_blank()) +
   ylim(-2.5, 0.35)
dev.off()

s = les[, .(m=median(gini),
      lo=quantile(gini, 0.025),
      hi=quantile(gini, 0.975)),
      by = .(age, group)]
s[, age := factor(age, labels=e_groups)]

savepdf('output/w_le_differences_race_gini_pcprior_1_10_abs')
ggplot(s[group %in% c('White', 'African-American')], aes(x=age, y=m)) +
   geom_ribbon(aes(ymin=lo, ymax=hi, group=group, fill=group), alpha=0.1)  +
   geom_point(aes(color=group), size=0.9, alpha=0.3)  +
   geom_line(aes(color=group, group=group), size = 0.4, alpha=0.3)  +
   theme_classic() +
   scale_color_manual(values=c("#0072B2", "#D55E00")) +
   scale_fill_manual(values=c("#0072B2", "#D55E00")) +
   labs(x='\nLife expectancy at age', y='Difference in years\n') +
   geom_hline(yintercept = 0,  size = 0.2, alpha = 0.3, linetype = 2) +
   theme(axis.text.x = element_text(angle = 40, hjust = 1),
   legend.position="top",
   legend.title=element_blank()) +
   ylim(-2.5, 0.35)
dev.off()

# relative

les = rbindlist(w_les)
les[, mob := (mob_1 - mob_0)/mob_0]
les[, gini := (gini_1 - gini_0)/gini_0]

s = les[, .(m=median(mob),
      lo=quantile(mob, 0.025),
      hi=quantile(mob, 0.975)),
      by = .(age, group)]
s[, age := factor(age, labels=e_groups)]

savepdf('output/w_le_re_differences_race_mob_pcprior_1_10_abs')
ggplot(s[group %in% c('White', 'African-American')], aes(x=age, y=m)) +
   geom_ribbon(aes(ymin=lo, ymax=hi, group=group, fill=group), alpha=0.1)  +
   geom_point(aes(color=group), size=0.9, alpha=0.3)  +
   geom_line(aes(color=group, group=group), size = 0.4, alpha=0.3)  +
   theme_classic() +
   scale_color_manual(values=c("#0072B2", "#D55E00")) +
   scale_fill_manual(values=c("#0072B2", "#D55E00")) +
   labs(x='\nLife expectancy at age', y='Difference in years\n') +
   geom_hline(yintercept = 0,  size = 0.2, alpha = 0.3, linetype = 2) +
   theme(axis.text.x = element_text(angle = 40, hjust = 1),
   legend.position="top",
   legend.title=element_blank()) +
   ylim(-.15, 0.05)
dev.off()

s = les[, .(m=median(gini),
      lo=quantile(gini, 0.025),
      hi=quantile(gini, 0.975)),
      by = .(age, group)]
s[, age := factor(age, labels=e_groups)]

savepdf('output/w_le_re_differences_race_gini_pcprior_1_10_abs')
ggplot(s[group %in% c('White', 'African-American')], aes(x=age, y=m)) +
   geom_ribbon(aes(ymin=lo, ymax=hi, group=group, fill=group), alpha=0.1)  +
   geom_point(aes(color=group), size=0.9, alpha=0.3)  +
   geom_line(aes(color=group, group=group), size = 0.4, alpha=0.3)  +
   theme_classic() +
   scale_color_manual(values=c("#0072B2", "#D55E00")) +
   scale_fill_manual(values=c("#0072B2", "#D55E00")) +
   labs(x='\nLife expectancy at age', y='Difference in years\n') +
   geom_hline(yintercept = 0,  size = 0.2, alpha = 0.3, linetype = 2) +
   theme(axis.text.x = element_text(angle = 40, hjust = 1),
   legend.position="top",
   legend.title=element_blank()) +
   ylim(-.15, 0.05)
dev.off()

########################################
# CDC mortality - income mobility paper
# analysis by race
# author: sebastian daza
########################################

# utils
source("src/utils/utils.R")

# load libraries
library(ggthemes)
# devtools::install_github("thomasp85/patchwork")
library(patchwork)
library(fmsb)

# INLA options
inla.setOption("num.threads", 30)
inla.setOption(pardiso.license = "../pardiso.lic")
inla.pardiso.check()
inla.setOption(short.summary = TRUE)

# read data
descriptive_info = readRDS('output/descriptive_info.rds')
data = readRDS(paste0('output/cdc_chetty_',
                      descriptive_info$number_counties,
                      '_counties.rds')
)

# create indicators for county and state
data[, county_i := .GRP, by = fips]
data[, state_i := .GRP, by = state]
data[, race_i := .GRP, by = race]

table(data$race)

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

# models

# set prior
pcprior = list(prec = list(prior="pc.prec",
                           param = c(1, 0.10)))

# general model
formula = deaths ~  1 + log_population + log_income + z_relative_mob +
          z_gini + z_segregation_income + log_unemployment + log_pct_hispanic +
          log_pct_black + z_uninsured + z_medicare_expenses +
          f(state_i, model = 'iid', hyper = pcprior) +
          f(county_i, model = 'iid', hyper = pcprior) +
          f(age, model = 'iid', hyper = pcprior) +
          f(id, model = 'iid', hyper = pcprior) +
          f(mob_age, z_relative_mob, model = 'iid', hyper = pcprior) +
          f(gini_age, z_gini, model = 'iid', hyper = pcprior)


model_function = function(formula = NULL, data = NULL) {

    output = inla(formula = formula, data = data,
                  family = 'poisson', E = pop,
                  control.compute = list(config = TRUE, dic = TRUE, waic = TRUE,
                                         cpo = TRUE),
                  control.predictor = list(link = 1, compute = TRUE),
                  control.inla = list(strategy = 'gaussian')
    )

    return(output)
}


# white
m1 = model_function(formula = formula, data = men[race_i == 1])
w1 = model_function(formula = formula, data = women[race_i == 1])
# black
m2 = model_function(formula = formula, data = men[race_i == 2])
w2 = model_function(formula = formula, data = women[race_i == 2])
# hispanic
m3 = model_function(formula = formula, data = men[race_i == 3])
w3 = model_function(formula = formula, data = women[race_i == 3])
# other
m4 = model_function(formula = formula, data = men[race_i == 4])
w4 = model_function(formula = formula, data = women[race_i == 4])

men_models = list(m1, m2, m3, m4)
women_models = list(w1, w2, w3, w4)

# coefficient plots
savepdf('output/m_race_dist_pcprior_1_10')
print(plot_fixed_coeff(men_models,
                 coeff = c('z_relative_mob', 'z_gini'),
                 coeff_labels = c('Relative Mobility', 'Gini'),
                 exponential = TRUE,
                 group_labels = race_groups)
)
dev.off()

savepdf('output/w_race_dist_pcprior_1_10')
print(plot_fixed_coeff(women_models,
                 coeff = c('z_relative_mob', 'z_gini'),
                 coeff_labels = c('Relative Mobility', 'Gini'),
                 exponential = TRUE,
                 group_labels = race_groups)
)
dev.off()

# simulations for LE estimates
m_sim_list = list()
for (i in 1:length(men_models)) {
    print(paste0('::: Model ', i, ' :::'))
    m_sim_list[[i]] = inla.posterior.sample(n = 2000, result = men_models[[i]])
}

w_sim_list = list()
for (i in 1:length(women_models)) {
    print(paste0('::: Model ', i, ' :::'))
    w_sim_list[[i]] = inla.posterior.sample(n = 2000, result = women_models[[i]])
}

# le differences
value_matrix = rbind(
                     c(1, 0, 0,   1, 1, 0),
                     c(1, 1, 0,   1, 1, 0),
                     c(1, 0, 0,   1, 0, 1),
                     c(1, 0, 1,   1, 0, 1)
)

rownames(value_matrix) = c('mob_0', 'mob_1', 'gini_0', 'gini_1')


# men
m_les = list()
for (m in 1:length(m_sim_list)) {
    t = estimate_le_counterfactuals(m_sim_list[[m]],
                                    fixed = c('z_relative_mob', 'z_gini'),
                                    random = c('age', 'mob_age', 'gini_age'),
                                    value_matrix = value_matrix
    )
    t[, group := race_groups[m]]
    m_les[[m]] = t
}


# plot absolute differences
les = rbindlist(m_les)
les[, mob := mob_1 - mob_0]
les[, gini := gini_1 - gini_0]

s = les[, .(m = median(mob),
            lo = quantile(mob, 0.025),
            hi = quantile(mob, 0.975)),
            by = .(age, group)]
s[, age := factor(age, labels = e_groups)]

savepdf('output/m_le_differences_race_mob_pcprior_1_10')
print(
plot_le_differences(s[group %in% c('White', 'African-American')],
                    ylim_hi = 0.35,
                    ylim_low = -2.0)
)
dev.off()

s = les[, .(m = median(gini),
            lo = quantile(gini, 0.025),
            hi = quantile(gini, 0.975)),
            by = .(age, group)]
s[, age := factor(age, labels = e_groups)]

savepdf('output/m_le_differences_race_gini_pcprior_1_10')
print(
plot_le_differences(s[group %in% c('White', 'African-American')],
                    ylim_hi = 0.35,
                    ylim_low = -2.0)
)
dev.off()


# plot relative differences
les = rbindlist(m_les)
les[, mob := (mob_1 - mob_0) / mob_0]
les[, gini := (gini_1 - gini_0) / gini_0]

s = les[, .(m = median(mob),
            lo = quantile(mob, 0.025),
            hi = quantile(mob, 0.975)),
            by = .(age, group)]
s[, age := factor(age, labels = e_groups)]

savepdf('output/m_le_re_differences_race_mob_pcprior_1_10')
print(
plot_le_differences(s[group %in% c('White', 'African-American')],
                    ylim_hi = 0.05,
                    ylim_low = -.15)
)
dev.off()

s = les[, .(m = median(gini),
            lo = quantile(gini, 0.025),
            hi = quantile(gini, 0.975)),
        by = .(age, group)]
s[, age := factor(age, labels = e_groups)]

savepdf('output/m_le_re_differences_race_gini_pcprior_1_10')
print(
plot_le_differences(s[group %in% c('White', 'African-American')],
                    ylim_hi = 0.05,
                    ylim_low = -.15)
)
dev.off()


# women
w_les = list()
for (m in 1:length(w_sim_list)) {
    t = estimate_le_counterfactuals(w_sim_list[[m]],
                                    fixed = c('z_relative_mob', 'z_gini'),
                                    random = c('age', 'mob_age', 'gini_age'),
                                    value_matrix = value_matrix)
    t[, group := race_groups[m]]
    w_les[[m]] = t
}

# absolute difference
les = rbindlist(w_les)
les[, mob := mob_1 - mob_0]
les[, gini := gini_1 - gini_0]

s = les[, .(m = median(mob),
            lo = quantile(mob, 0.025),
            hi = quantile(mob, 0.975)),
        by = .(age, group)]
s[, age := factor(age, labels = e_groups)]

savepdf('output/w_le_differences_race_mob_pcprior_1_10')
print(
plot_le_differences(s[group %in% c('White', 'African-American')],
                    ylim_hi = 0.35,
                    ylim_low = -2.0)
)
dev.off()

s = les[, .(m = median(gini),
            lo = quantile(gini, 0.025),
            hi = quantile(gini, 0.975)),
        by = .(age, group)]
s[, age := factor(age, labels = e_groups)]

savepdf('output/w_le_differences_race_gini_pcprior_1_10')
print(
plot_le_differences(s[group %in% c('White', 'African-American')],
                    ylim_hi = 0.35,
                    ylim_low = -2.0)
)
dev.off()

# relative differences
les = rbindlist(w_les)
les[, mob := (mob_1 - mob_0)/mob_0]
les[, gini := (gini_1 - gini_0)/gini_0]

s = les[, .(m = median(mob),
            lo = quantile(mob, 0.025),
            hi = quantile(mob, 0.975)),
            by = .(age, group)]
s[, age := factor(age, labels = e_groups)]

savepdf('output/w_le_re_differences_race_mob_pcprior_1_10')
print(
plot_le_differences(s[group %in% c('White', 'African-American')],
                    ylim_hi = 0.05,
                    ylim_low = -.15)
)
dev.off()

s = les[, .(m  = median(gini),
            lo = quantile(gini, 0.025),
            hi = quantile(gini, 0.975)),
        by = .(age, group)]
s[, age := factor(age, labels = e_groups)]

savepdf('output/w_le_re_differences_race_gini_pcprior_1_10')
print(
plot_le_differences(s[group %in% c('White', 'African-American')],
                    ylim_hi = 0.05,
                    ylim_low = -.15)
)
dev.off()

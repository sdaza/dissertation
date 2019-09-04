########################################
# CDC mortality - income mobility paper
# main analysis using absolute mobility
# author: sebastian daza
########################################


# utils
source("src/utils/utils.R")

# load libraries
library(fmsb)

 # INLA options
inla.setOption("num.threads", 15)
inla.setOption(pardiso.license = "../pardiso.lic")
inla.pardiso.check()
inla.setOption(short.summary = TRUE)

# read data
descriptive_info = readRDS('output/descriptive_info.rds')
data = readRDS(paste0('output/cdc_chetty_',
                      descriptive_info$number_counties,
                      '_counties.rds')
)

# revert absolute mobility sign
data[, z_absolute_mob := z_absolute_mob * -1]

# create indicators for county and state
data[, county_i := .GRP, by = fips]
data[, state_i := .GRP, by = state]

length(unique(data[, county_i]))

# aggregate data by gender
male = data[sex == 1]
female = data[sex == 2]

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

male_cov = male[, s := 1:.N, by = .(age, county)][s == 1,][
    , c('pop','deaths', 'deaths1', 'deaths2', 'deaths3',
        'deaths4', 'state', 'race', 's', 'sex') := NULL]

female_cov = female[, s := 1:.N, by = .(age, county)][s == 1,][
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


# models

# define prior
pcprior = list(prec = list(prior = "pc.prec",
                           param = c(1, 0.10)))


# baseline (0)
print(':::::::: running baseline')

formula = deaths ~  1 + log_population + log_income + z_absolute_mob +
          z_gini +
          f(state_i, model = 'iid', hyper = pcprior) +
          f(county_i, model = 'iid', hyper = pcprior) +
          f(age, model = 'iid', hyper = pcprior) +
          f(id, model = 'iid', hyper = pcprior)

m0 = inla(formula, data = men,
          family = 'poisson', E = pop,
          control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
          control.predictor = list(link = 1, compute = TRUE),
          control.inla = list(strategy = 'gaussian'),
          )

w0 = inla(formula, data = women,
          family = 'poisson', E = pop,
          control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
          control.predictor = list(link = 1, compute = TRUE),
          control.inla = list(strategy = 'gaussian')
          )

round(m0$summary.fixed, 4)
bri.hyperpar.summary(m0)

round(w0$summary.fixed, 4)
bri.hyperpar.summary(w0)


# varying gini and mobility by age (1)
print(':::::::: varying gini and mobility by age')

formula = deaths ~  1 + log_population + log_income + z_absolute_mob +
          z_gini +
          f(state_i, model = 'iid', hyper = pcprior) +
          f(county_i, model = 'iid', hyper = pcprior) +
          f(age, model = 'iid', hyper = pcprior) +
          f(id, model = 'iid', hyper = pcprior) +
          f(mob_age, z_absolute_mob, model = 'iid', hyper = pcprior) +
          f(gini_age, z_gini, model = 'iid', hyper = pcprior)

m1 = inla(formula, data = men,
          family = 'poisson', E = pop,
          control.compute = list(config = TRUE, dic = TRUE, waic = TRUE,
                                 cpo = TRUE),
          control.predictor = list(link = 1, compute = TRUE),
          control.inla = list(strategy = 'gaussian')
          )

w1 = inla(formula, data = women,
          family = 'poisson', E = pop,
          control.compute = list(config = TRUE, dic = TRUE, waic = TRUE,
                                 cpo= TRUE),
          control.predictor = list(link = 1, compute = TRUE),
          control.inla = list(strategy = 'gaussian')
          )

round(m1$summary.fixed, 4)
bri.hyperpar.summary(m1)

round(w1$summary.fixed, 4)
bri.hyperpar.summary(w1)


# interaction mob x gini (2)
print(':::::::: interaction mob and gini')

formula = deaths ~  1 + log_population + log_income + z_absolute_mob  * z_gini +
          f(state_i, model = 'iid', hyper = pcprior) +
          f(county_i, model = 'iid', hyper = pcprior) +
          f(age, model = 'iid', hyper = pcprior) +
          f(id, model = 'iid', hyper = pcprior) +
          f(mob_age, z_absolute_mob, model = 'iid', hyper = pcprior) +
          f(gini_age, z_gini, model = 'iid', hyper = pcprior)

m2 = inla(formula, data = men,
          family = 'poisson', E = pop,
          control.compute = list(config = TRUE, dic = TRUE, waic = TRUE,
                                 cpo= TRUE),
          control.predictor = list(link = 1, compute = TRUE),
          control.inla = list(strategy = 'gaussian')
          )

w2 = inla(formula, data = women,
          family = 'poisson', E = pop,
          control.compute = list(config = TRUE, dic = TRUE, waic = TRUE,
                                 cpo= TRUE),
          control.predictor = list(link = 1, compute = TRUE),
          control.inla = list(strategy = 'gaussian')
          )

round(m2$summary.fixed, 4)
bri.hyperpar.summary(m2)

round(w2$summary.fixed, 4)
bri.hyperpar.summary(w2)


# interaction income x mob  (3)
print(':::::::: running interaction income x mob')

formula = deaths ~  1 + log_population + log_income * z_absolute_mob +  z_gini +
          f(state_i, model = 'iid', hyper = pcprior) +
          f(county_i, model = 'iid', hyper = pcprior) +
          f(age, model = 'iid', hyper = pcprior) +
          f(id, model = 'iid', hyper = pcprior) +
          f(mob_age, z_absolute_mob, model = 'iid', hyper = pcprior) +
          f(gini_age, z_gini, model = 'iid', hyper = pcprior)

m3 = inla(formula, data = men,
          family = 'poisson', E = pop,
          control.compute = list(config = TRUE, dic = TRUE, waic = TRUE,
                                 cpo= TRUE),
          control.predictor = list(link = 1, compute = TRUE),
          control.inla = list(strategy = 'gaussian')
          )

w3 = inla(formula, data = women,
          family = 'poisson', E = pop,
          control.compute = list(config = TRUE, dic = TRUE, waic = TRUE,
                                 cpo= TRUE),
          control.predictor = list(link = 1, compute = TRUE),
          control.inla = list(strategy = 'gaussian')
          )

round(m3$summary.fixed, 4)
bri.hyperpar.summary(m3)

round(w3$summary.fixed, 4)
bri.hyperpar.summary(w3)


# covariate adjustment (4)
print(':::::::: running covariate adjustment')

formula = deaths ~  1 + log_population + log_income + z_absolute_mob +
          z_gini + z_segregation_income + log_unemployment + log_pct_hispanic +
          log_pct_black + z_uninsured + z_medicare_expenses +
          f(state_i, model = 'iid', hyper = pcprior) +
          f(county_i, model = 'iid', hyper = pcprior) +
          f(age, model = 'iid', hyper = pcprior) +
          f(id, model = 'iid', hyper = pcprior) +
          f(mob_age, z_absolute_mob, model = 'iid', hyper = pcprior) +
          f(gini_age, z_gini, model = 'iid', hyper = pcprior)

m4 = inla(formula, data = men,
          family = 'poisson', E = pop,
          control.compute = list(config = TRUE, dic = TRUE, waic = TRUE,
                                 cpo = TRUE),
          control.predictor = list(link = 1, compute = TRUE),
          control.inla = list(strategy = 'gaussian'),
          )

w4 = inla(formula, data = women,
          family = 'poisson', E = pop,
          control.compute = list(config = TRUE, dic = TRUE, waic = TRUE,
                                 cpo = TRUE),
          control.predictor = list(link = 1, compute = TRUE),
          control.inla = list(strategy = 'gaussian')
          )

round(m4$summary.fixed, 4)
bri.hyperpar.summary(m4)

round(w4$summary.fixed, 4)
bri.hyperpar.summary(w4)


# spatial (5)
print(':::::::: running spatial model')

# specify path of the graph object
county.adj <- "data/county.graph"

formula = deaths ~  1 + log_population + log_income + z_absolute_mob +
          z_gini + z_segregation_income + log_unemployment + log_pct_hispanic +
          log_pct_black + z_uninsured + z_medicare_expenses +
          f(state_i, model = 'iid', hyper = pcprior) +
          f(county_i, model = 'bym2', graph = county.adj, hyper = pcprior) +
          f(age, model = 'iid', hyper = pcprior) +
          f(id, model = 'iid', hyper = pcprior) +
          f(mob_age, z_absolute_mob, model = 'iid', hyper = pcprior) +
          f(gini_age, z_gini, model = 'iid', hyper = pcprior)

m5 = inla(formula, data = men,
          family = 'poisson', E = pop,
          control.compute = list(config = TRUE, dic = TRUE, waic = TRUE,
                                 cpo = TRUE),
          control.predictor = list(link = 1, compute = TRUE),
          control.inla = list(strategy = 'gaussian')
          )

w5 = inla(formula, data = women,
          family = 'poisson', E = pop,
          control.compute = list(config = TRUE, dic = TRUE, waic = TRUE,
                                 cpo = TRUE),
          control.predictor = list(link = 1, compute = TRUE),
          control.inla = list(strategy = 'gaussian')
          )

round(m5$summary.fixed, 4)
bri.hyperpar.summary(m5)

round(w5$summary.fixed, 4)
bri.hyperpar.summary(w5)

# create table with models
cnames <- list(
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


cmodels <- c('Baseline', 'Varying-Coefficient',
             'Absolute mobility x Gini', 'Absolute mobility x Income',
             'Covariates', 'Spatial')

# men
m_models <- list(m0,m1,m2,m3,m4,m5)

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
       include.dic = TRUE,
       include.waic = TRUE,
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
       file = "output/w_age_pcprior_1_10_abs.tex"
)

# remove models
rm(m0, w0, m1, w1, m2, w2, m3, w3, m5, w5)


# create plots

m4.resid = bri.Pois.resid(m4, plot = TRUE)
savepdf('output/m4_resid_pcprior_1_10_abs')
    qqnorm(m4.resid$resid)
    qqline(m4.resid$resid)
dev.off()

w4.resid = bri.Pois.resid(w4, plot = TRUE)
savepdf('output/w4_resid_pcprior_1_10_abs')
    qqnorm(w4.resid$resid)
    qqline(w4.resid$resid)
dev.off()

# # savepdf('output/m4_predvalues_pcprior_1_10_abs')
# # hist_pred_pvalues(m4, men$deaths) # pretty bad
# # dev.off()

# # savepdf('output/w4_predvalues_pcprior_1_10_abs')
# # hist_pred_pvalues(w4, men$deaths) # pretty bad
# # dev.off()

savepdf('output/m4_loo_pcprior_1_10_abs')
    plot_loo(m4)
dev.off()

savepdf('output/w4_loo_pcprior_1_10_abs')
    plot_loo(w4)
dev.off()

# # re-run cpo very slow
# m4.cpo = inla.cpoinla.cpo(m4)
# # improve loo (very slow)
# # improved.m4 = inla.cpo(m4)
# # improved.w4 = inla.cpo(w4)
# # savepdf('output/m4_loo_pcprior_1_10_abs')
# # plot_loo(improved.m4)
# # dev.off()
# # savepdf('output/w4_loo_pcprior_1_10_abs')
# # plot_loo(improved.w4)
# # dev.off()

# coefficient plots
# get warnings because contrains of axis
savepdf('output/m4_coefficients_age_pcprior_1_10_abs')
print(
plot_fixed_coeff(m4, coeff = c('z_absolute_mob', 'z_gini'),
                 coeff_labels = c('Absolute Mobility', 'Gini'),
                 exponential = TRUE)
                 + xlim(0.95,1.15)
)
dev.off()

savepdf('output/w4_coefficients_age_pcprior_1_10_abs')
print(
plot_fixed_coeff(w4, coeff = c('z_absolute_mob', 'z_gini'),
                 coeff_labels = c('Absolute Mobility', 'Gini'),
                 exponential = TRUE)
                 + xlim(0.95,1.15)
)
dev.off()

# create simulation for plots
sim_m4 = inla.posterior.sample(n = 5000, result = m4)
sim_w4 = inla.posterior.sample(n = 5000, result = w4)

# age effects
savepdf('output/m4_effects_age_pcprior_1_10_abs')
print(
plot_random_fixed_effects_sim(sim_m4, fixed = c('z_absolute_mob', 'z_gini'),
                              random = c('mob_age', 'gini_age'),
                              x_labels = age_groups,
                              ylabel = 'Rate ratio\n',
                              xlabel = '\nAge group',
                              effects_labels = c('Absolute Mobility', 'Gini'),
                              sorted = FALSE,
                              exponential = TRUE) + ylim(0.8, 1.3)
)
dev.off()

savepdf('output/w4_effects_age_pcprior_1_10_abs')
print(
plot_random_fixed_effects_sim(sim_w4, fixed = c('z_absolute_mob', 'z_gini'),
                              random = c('mob_age', 'gini_age'),
                              x_labels = age_groups,
                              ylabel = 'Rate ratio\n',
                              xlabel = '\nAge group',
                              effects_labels = c('Absolute Mobility', 'Gini'),
                              sorted = FALSE,
                              exponential = TRUE) + ylim(0.8, 1.3)
)
dev.off()

# life expectancy predictions
value_matrix = rbind(
                     c(1, 0, 0,   1, 1, 0),
                     c(1, 1, 0,   1, 1, 0),
                     c(1, 0, 0,   1, 0, 1),
                     c(1, 0, 1,   1, 0, 1))

rownames(value_matrix) = c('mob_0', 'mob_1', 'gini_0', 'gini_1')

les_m = estimate_le_counterfactuals(sim_m4,
                                    fixed = c('z_absolute_mob', 'z_gini'),
                                    random = c('age', 'mob_age', 'gini_age'),
                                    value_matrix = value_matrix)


savepdf('output/m4_le_differences_pcprior_1_10_abs')
print(
plot_le_counterfactuals(les_m, x_labels = e_groups,
                        name_groups= c('Absolute Mobility', 'Gini')) +
                        ylim(-1.6, 0.5)
)
dev.off()

savepdf('output/m4_le_re_differences_pcprior_1_10_abs')
print(
    plot_le_counterfactuals(les_m, relative = TRUE, x_labels = e_groups,
                            name_groups = c('Absolute Mobility', 'Gini')) +
                            ylim(-0.12, 0.05)
)
dev.off()

les_w = estimate_le_counterfactuals(sim_w4,
                                    fixed = c('z_absolute_mob', 'z_gini'),
                                    random = c('age', 'mob_age', 'gini_age'),
                                    value_matrix = value_matrix)

head(les_w)

savepdf('output/w4_le_differences_pcprior_1_10_abs')
print(
plot_le_counterfactuals(les_w, x_labels = e_groups,
                        name_groups = c('Absolute Mobility', 'Gini')) +
                        ylim(-1.6, 0.5)
)
dev.off()

savepdf('output/w4_le_re_differences_pcprior_1_10_abs')
print(
plot_le_counterfactuals(les_w, relative = TRUE,  x_labels = e_groups,
                        name_groups = c('Absolute Mobility', 'Gini')) +
                        ylim(-0.12, 0.05)
)
dev.off()


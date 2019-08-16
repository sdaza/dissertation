########################################
# CDC mortality - income mobility paper
# analysis by cause of death
# author: sebastian daza
########################################


# utils
source('src/utils/utils.R')

# load libraries
library(patchwork)
library(fmsb)
library(stringr)

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

# expand data by cause
dc = melt(data,
          measure = patterns('^deaths[0-9]'),
          value.name = c('deaths_by_cause'),
          variable.name = 'cause')

# create indicators for county and state
dc[, county_i := .GRP, by = fips]
dc[, state_i := .GRP, by = state]
dc[, cause_i := .GRP, by = cause]

table(dc$cause_i)

# aggregate data by gender
# not taking into account race differences for now
male = dc[sex == 1]
female = dc[sex == 2]

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

# remove unused columns
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


# models
pcprior = list(prec = list(prior="pc.prec",
                           param = c(1, 0.10)))

age_groups = c('0-4','5-9','10-14','15-19','20-24',
                '25-39','30-34','35-39','40-44','45-49',
                '50-54','55-59','60-64','65-69','70-74',
                '75-79','80-84','85+')

cause_groups = c('Communicable', 'Non-communicable',
                 'Injury', 'Residuals')


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


# cause 1
m1 = model_function(formula = formula, data = men[cause_i == 1])
w1 = model_function(formula = formula, data = women[cause_i == 1])
# cause 2
m2 = model_function(formula = formula, data = men[cause_i == 2])
w2 = model_function(formula = formula, data = women[cause_i == 2])
# cause 3
m3 = model_function(formula = formula, data = men[cause_i == 3])
w3 = model_function(formula = formula, data = women[cause_i == 3])
# cause 4
m4 = model_function(formula = formula, data = men[cause_i == 4])
w4 = model_function(formula = formula, data = women[cause_i == 4])

men_models = list(m1, m2, m3, m4)
women_models = list(w1, w2, w3, w4)


# plot coefficients
savepdf('output/m_cause_dist_pcprior_1_10')
print(
plot_fixed_coeff(men_models,
               coeff = c('z_relative_mob', 'z_gini'),
               coeff_labels = c('Relative Mobility', 'Gini'),
               exponential = TRUE,
               group_labels = cause_groups) + xlim(.95, 1.5)
)
dev.off()

savepdf('output/w_cause_dist_pcprior_1_10')
print(
plot_fixed_coeff(women_models,
               coeff = c('z_relative_mob', 'z_gini'),
               coeff_labels = c('Relative Mobility', 'Gini'),
               exponential = TRUE,
               group_labels = cause_groups) + xlim(.95, 1.5)
)
dev.off()
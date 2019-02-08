# CDC mortality - income mobility
# author: Sebastina Daza

# R < 00dissertation/ch02/src/00_cdc_inla_age_models_prior_sensitivity.R > 00dissertation/ch02/src/00_cdc_inla_age_models_age_sensitivity.log  --no-save  &

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
inla.setOption("num.threads", (nodes/2) - 6)

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

##############################
# models
##############################

# m1 (default)

formula = deaths ~  1 + log_population + log_income + z_relative_mob +
          z_gini + z_segregation_income + log_unemployment + log_pct_hispanic +
          log_pct_black + z_uninsured + z_medicare_expenses +
        f(state_i, model = 'iid') +
        f(county_i, model = 'iid') +
        f(age, model = 'iid') +
        f(id, model = 'iid') +
        f(mob_age, z_relative_mob, model = 'iid') +
        f(gini_age, z_gini, model = 'iid')

m1 = inla(formula, data = men,
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo=TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

w1 = inla(formula, data = women,
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo=TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

# m2 (pc(1, .10)

pcprior = list(prec = list(prior="pc.prec",
  param = c(1, 0.10)))

formula = deaths ~  1 + log_population + log_income + z_relative_mob +
          z_gini + z_segregation_income + log_unemployment + log_pct_hispanic +
          log_pct_black + z_uninsured + z_medicare_expenses +
        f(state_i, model = 'iid', hyper=pcprior) +
        f(county_i, model = 'iid', hyper=pcprior) +
        f(age, model = 'iid', hyper=pcprior) +
        f(id, model = 'iid', hyper=pcprior) +
        f(mob_age, z_relative_mob, model = 'iid', hyper=pcprior) +
        f(gini_age, z_gini, model = 'iid', hyper=pcprior)

m2 = inla(formula, data = men,
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo=TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

w2 = inla(formula, data = women,
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo=TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

# m3 (pc(10, .10))

pcprior = list(prec = list(prior="pc.prec",
  param = c(10, 0.10)))


formula = deaths ~  1 + log_population + log_income + z_relative_mob +
          z_gini + z_segregation_income + log_unemployment + log_pct_hispanic +
          log_pct_black + z_uninsured + z_medicare_expenses +
        f(state_i, model = 'iid', hyper=pcprior) +
        f(county_i, model = 'iid', hyper=pcprior) +
        f(age, model = 'iid', hyper=pcprior) +
        f(id, model = 'iid', hyper=pcprior) +
        f(mob_age, z_relative_mob, model = 'iid', hyper=pcprior) +
        f(gini_age, z_gini, model = 'iid', hyper=pcprior)

m3 = inla(formula, data = men,
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo=TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

w3 = inla(formula, data = women,
                family = 'poisson', E = pop,
                control.compute = list(config=TRUE, dic=TRUE, waic=TRUE,
                  cpo=TRUE),
                control.predictor = list(link = 1, compute = TRUE),
                control.inla = list(strategy = 'gaussian')
              )

# m4 (pc(10, 0.01))

pcprior = list(prec = list(prior="pc.prec",
  param = c(10, 0.01)))

formula = deaths ~  1 + log_population + log_income + z_relative_mob +
          z_gini + z_segregation_income + log_unemployment + log_pct_hispanic +
          log_pct_black + z_uninsured + z_medicare_expenses +
        f(state_i, model = 'iid', hyper=pcprior) +
        f(county_i, model = 'iid', hyper=pcprior) +
        f(age, model = 'iid', hyper=pcprior) +
        f(id, model = 'iid', hyper=pcprior) +
        f(mob_age, z_relative_mob, model = 'iid', hyper=pcprior) +
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


# create table
source('src/utils/extract_inla.R')

cnames = list(
               '(Intercept)' = 'Constant',
               z_relative_mob = 'Income relative mobility',
               z_gini = 'Gini',
               'sd for id' = 'SD observations',
               'sd for age' = 'SD age group',
               'sd for county_i' = 'SD counties',
               'Phi for county_i' = 'Phi counties',
               'sd for state_i' = 'SD states',
               'sd for mob_age' = 'SD mobility by age',
               'sd for gini_age' = 'SD gini by age')

cmodels = c('INLA Default', 'PC(1, .10)', 'PC(10, .10)', 'PC(10, 0.01)')

# men
m_models = list(m1,m2,m3,m4)

texreg(m_models,
            include.dic = TRUE, include.waic = TRUE,
            ci.test = FALSE,
            float.pos = "htp",
            caption = "County Level Poisson Models, Prior Sensitivity, Men, CDC 2000-2014",
            booktabs = TRUE,
            use.packages = FALSE,
            dcolumn = TRUE,
            caption.above = TRUE,
            scalebox = 0.65,
            label = 'tbl:m_age_prior_sensitivity',
            # sideways = TRUE,
            digits = 2,
            custom.model.names = cmodels,
            custom.coef.map = cnames,
            groups = list("Random Effects" = c(4:9)),
            custom.note = "Note: Selected coefficients (mean of marginal posterior distribution).
            Poisson model with offset = \\texttt{log(population)}. 95\\% credibility intervals.",
             file = "output/m_age_prior_sensitivity.tex")


# women
w_models = list(w1,w2,w3,w4)

texreg(w_models,
           include.dic = TRUE, include.waic = TRUE,
           ci.test = FALSE,
           float.pos = "htp",
           caption = "County Level Poisson Models, Prior Sensitivity, Women, CDC 2000-2014",
           booktabs = TRUE,
           use.packages = FALSE,
           dcolumn = TRUE,
           caption.above = TRUE,
           scalebox = 0.65,
           label = 'tbl:w_age_prior_sensitivity',
           # sideways = TRUE,
           digits = 2,
           custom.model.names = cmodels,
           custom.coef.map = cnames,
           groups = list("Random Effects" = c(4:9)),
           custom.note = "Note: Selected coefficients (mean of marginal posterior distribution).
           Poisson model with offset = \\texttt{log(population)}. 95\\% credibility intervals.",
            file = "output/w_age_prior_sensitivity.tex")

######################
# end models
######################

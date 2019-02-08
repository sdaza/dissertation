# load libraries
library(sdazar)
library(INLA)
library(brinla)

library(ggplot2)
library(patchwork)

library(USAboundaries)
library(maptools)
library(spdep)
library(sp)
library(sf)

# load functions
source('../utils/functions.R')

# plot options
options(repr.plot.width = 5, repr.plot.height = 4) # plot options

# INLA node  options
(nodes <- parallel::detectCores()) / 3
INLA:::inla.dynload.workaround()
inla.setOption("num.threads", (nodes/2) - 6)

# read data
data = readRDS('../data/cdc_chetty.rds')
dim(data)
countmis(data)

# remove missing records
variables = c('z_relative_mob', 'z_absolute_mob', 'log_crime_rate', 'z_gini', 
              'z_medicare_expenses', 'log_unemployment', 'z_uninsured', 
              'log_pct_black', 'log_pct_hispanic')

data = data[complete.cases(data[, variables, with=FALSE])]
countmis(data)

# define counties for spatial analysis

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

counties.sp = as(counties, "Spatial")

temp = poly2nb(counties.sp)

nb2INLA("counties.graph", temp)
county.adj = "counties.graph"

H = inla.read.graph(filename = county.adj)
image(inla.graph2matrix(H), xlab="",ylab="")


# aggregate by gender


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

##############################
# models
##############################

# baseline

men[ id := 1:.N]

formula = deaths ~  1 + log_population + log_income + 
        f(state_i, model = 'iid') + 
        f(county_i, model = 'iid') + 
        f(age, model = 'iid') + 
        f(id, model = 'iid')


m0 = inla(formula, data = male,
                  family = 'poisson', E = pop, 
                  control.compute = list(dic=TRUE, waic = TRUE, cpo = TRUE),
                  control.inla = list(strategy ='gaussian'),
                  control.predictor = list(link = 1, compute = TRUE))

round(m0$summary.fixed, 4)
bri.hyperpar.summary(m0) # only small variability by state
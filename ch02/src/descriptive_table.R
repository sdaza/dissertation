# CDC mortality - income mobility
# author: Sebastian Daza

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
library(xtable)
library(haven)
# library(sf)

# load functions
setwd('00dissertation/ch02')
source('src/utils/functions.R')

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

covs = data.table(read_dta('data/cty_full_covariates.dta',
  encoding = 'latin1'))

#names(covs)
vars = c('cty', 's_rank', 'e_rank_b', 'gini99', 'cty_pop2000', 'hhinc00', 'cs00_seg_inc', 'unemp_rate', 'cs_frac_hisp', 'cs_frac_black', 'puninsured2010', 'reimb_penroll_adj10' )

nvars = c('county', 'relative_mob', 'absolute_mob', 'gini', 'population',
  'hincome', 'segregation_income', 'unemployement', 'pct_hispanic', 'pct_black', 'uninsured', 'medicare_expenses')

setnames(covs, vars, nvars)

covs = covs[, c('county', nvars), with=FALSE]
agg_deaths = data[, .(deaths = sum(deaths)), by=county]
setkey(covs, county)
setkey(agg_deaths, county)

covs = agg_deaths[covs]
covs = covs[complete.cases(covs)]
nrow(covs)

cor(covs[, .(relative_mob, absolute_mob)])

# summary values
covs[, lapply(.SD, function(x) list(mean, median)]

summaryfun = function(x) list(N=length(x),
  Mean=mean(x),
  Median=median(x),
  SD=sd(x),
  Min=min(x),
  Max=max(x))

nvars[-1]

# loop to get descriptives
ss =list()
for (i in nvars[-1]) {
  ss[[i]] = data.table(Variable=i, covs[, summaryfun(get(i))])
}
ss = rbindlist(ss)

# define variable names
var_labs = c('Relative income mobility', 'Absolute income mobility', 'Gini coefficient', 'Population 2000', 'Household income', 'Income segregation',
'Unemployment rate', '% Hispanic', '% African-American', '% Uninsured',
'Medicare expenses')

ss[, Variable := var_labs ]

# create xtable object and print
tab = xtable(ss)
print(tab, include.rownames=FALSE)

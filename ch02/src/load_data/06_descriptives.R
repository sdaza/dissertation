###################################
# CDC mortality - income mobility
# descriptive table and plot
# author: Sebastian Daza
###################################


# load libraries
library(sdazar)
library(xtable)
library(haven)
library(ggplot2)

# utils
source("src/utils/utils.R")

# plot options
options(repr.plot.width = 5, repr.plot.height = 4) # plot options

# read data
data = readRDS('output/cdc_chetty.rds')
dim(data)
countmis(data)

# remove missing records
variables = c('z_relative_mob', 'z_absolute_mob', 'z_gini',
              'z_medicare_expenses', 'log_unemployment', 'z_uninsured',
              'log_pct_black', 'log_pct_hispanic')

data = data[complete.cases(data[, ..variables])]
countmis(data)

covs = data.table(read_dta('data/cty_full_covariates.dta',
                  encoding = 'latin1'))

#names(covs)
vars = c('cty', 's_rank', 'e_rank_b', 'gini99', 'cty_pop2000',
         'hhinc00', 'cs00_seg_inc', 'unemp_rate', 'cs_frac_hisp',
         'cs_frac_black', 'puninsured2010', 'reimb_penroll_adj10' )

nvars = c('county', 'relative_mob', 'absolute_mob', 'gini', 'population',
          'hincome', 'segregation_income', 'unemployement',
          'pct_hispanic', 'pct_black', 'uninsured', 'medicare_expenses')

setnames(covs, vars, nvars)

covs = covs[, c('county', nvars), with=FALSE]
agg_deaths = data[, .(deaths = sum(deaths)), by = county]
setkey(covs, county)
setkey(agg_deaths, county)

covs = agg_deaths[covs]
covs = covs[complete.cases(covs)]
nrow(covs)

# correlations
cor(covs[, .(relative_mob, absolute_mob)])

# summary of values
summaryfun = function(x) {
    list(N = length(x),
         Mean = mean(x),
         Median = median(x),
         SD = sd(x),
         Min = min (x),
         Max = max(x)
    )
}

nvars[-1]

# loop to get descriptives
ss =list()

for (i in nvars[-1]) {
    ss[[i]] = data.table(Variable = i, covs[, summaryfun(get(i))])
}
ss = rbindlist(ss)

# define variable names
var_labs = c('Relative income mobility', 'Absolute income mobility',
             'Gini coefficient', 'Population 2000', 'Household income', 'Income segregation',
             'Unemployment rate', '% Hispanic', '% African-American', '% Uninsured',
             'Medicare expenses')

ss[, Variable := var_labs ]

# create xtable object and print
tab = xtable(ss)
print(tab, include.rownames = FALSE, file = "output/descriptive_table.tex")

# create heatmap income mobility and inequality
cov = data.table(read_dta('data/cty_full_covariates.dta'))

vars = c('e_rank_b', 's_rank', 'gini99')
nvars = c('absolute_mob', 'relative_mob', 'gini')

setnames(cov, vars, nvars)
nrow(cov)
cov = cov[complete.cases(cov[, .(relative_mob, gini)])]
nrow(cov)

summary(cov[, .(relative_mob, absolute_mob)])

cov[, relative_mob := relative_mob / 100]
cov[, q_relative_mob := cut(relative_mob, quantile(relative_mob, probs=0:10/10),
                                                   include.lowest = TRUE)]

cov[, q_gini := cut(gini, quantile(gini, probs=0:10/10),
                                   include.lowest = TRUE)]

table(cov$q_gini)
table(cov$q_relative_mob)

tab = data.table(table(cov[, q_relative_mob, q_gini]))
names(tab)

table(tab$N)


# create heatmap mobility and inequality
savepdf('output/heatmap_mob_gini')
print(
    ggplot(tab, aes(x = q_gini, y = q_relative_mob, fill = N)) +
           geom_tile() +
           scale_fill_gradient(
               name = "Counts",
               low = "white",
               high = "black",
               # limit = c(min(tab$N), 1),
               space = "Lab",
               guide = "colourbar") +
           theme_minimal() +
           labs(x = '\n Gini index', y = 'Relative income mobility\n') +
           theme(axis.text.x = element_text(angle = 40, hjust = 1))
)
dev.off()

summary(tab)

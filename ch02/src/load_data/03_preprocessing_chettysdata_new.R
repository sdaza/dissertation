##############################################
# process Chetty's data
# author: sebastian daza
##############################################

#+ set directory
pdata = "/Users/sdaza/Documents/Workplace/Data/mortality/data/"

# libraries
library(here)
library(sdazar)
library(haven)
library(ggplot2)
library(gridExtra)

source('src/utils/functions.R')
options(repr.plot.width = 4, repr.plot.height = 3)
####################################
#+ import and match with chetty data
####################################

# load chetty's data
mob = data.table(
  haven::read_stata('data/cty_full_covariates.dta'))

mob[, fips := sprintf("%05d", cty)]
mob[, state := sprintf("%02d", state_id)]
length(unique(mob$state)) # 51 states

#########################################
#+ adjust covariates chetty's data
#########################################

length(mob$cty) # 3138

# rename variables

vars = c('cty', 'county_name', 'cty_pop2000', 'statename', 'stateabbrv',
        'pop_density', 'gini99', 's_rank', 'e_rank_b', 'cs00_seg_inc',
         'cs_race_theil_2000', 'hhinc00', 'poor_share', 'frac_middleclass',
        'mig_inflow', 'mig_outflow', 'cs_born_foreign', 'rel_tot', 'crime_total',
         'puninsured2010',  'cs_labforce', 'unemp_rate', 'cs_frac_black', 'cs_frac_hisp',
         'bmi_obese', 'cur_smoke','exercise_any', 'median_house_value', 'cs_educ_ba',
         'reimb_penroll_adj10', 'subcty_exp_pc')

nvars = c('county', 'county_name', 'population', 'statename', 'stateabbrv',
        'density', 'gini', 'relative_mob', 'absolute_mob', 'segregation_income',
          'segregation_race', 'income', 'poverty', 'middle_class', 'mig_inflow',
          'mig_outflow', 'foreign', 'religion', 'crime_rate', 'uninsured',
        'labor_force', 'unemployment', 'pct_black', 'pct_hispanic',
          'obesity', 'smoking', 'exercise', 'house_value', 'college', 'medicare_expenses',
        'local_gov_exp')

setnames(mob, vars, nvars)

# explore continous variables

vars = c('gini', 'relative_mob', 'absolute_mob', 'population', 'density', 'crime_rate', 'poverty', 'middle_class',
            'mig_inflow', 'mig_outflow', 'foreign', 'pct_black', 'pct_hispanic',
            'religion', 'uninsured', 'medicare_expenses', 'house_value', 'local_gov_exp',
            'college', 'obesity', 'exercise', 'smoking', 'unemployment',
            'labor_force', 'income',  'segregation_income', 'segregation_race')

for (i in seq_along(vars)) {
  p = ggplot(mob, aes_string(vars[i])) + geom_density() + theme_minimal()
  assign(paste0('g', i), p)
}

options(repr.plot.width = 7, repr.plot.height = 7)
# grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12)
grid.arrange(g13, g14, g15, g16, g17, g18, g19, g20, g21, g22, g23, g24,
    g25, g26, g27)


# transform
z_variables = c('gini', 'relative_mob', 'absolute_mob',
            'middle_class', 'religion',  'labor_force', 'uninsured', 'medicare_expenses', 'segregation_income',
            'college', 'obesity', 'smoking', 'exercise')

zscores(mob, z_variables)

lookvar(mob, '^z_')

log_variables = c('population', 'crime_rate', 'poverty', 'mig_inflow',
            'mig_outflow', 'foreign', 'pct_black', 'pct_hispanic',
            'house_value', 'local_gov_exp', 'unemployment', 'income')

logtrans(mob, log_variables)

vars = lookvar(mob, '^log')
length(vars)

for (i in seq_along(vars)) {
  p = ggplot(mob, aes_string(vars[i])) + geom_density() + theme_minimal()
  assign(paste0('g', i), p)
}

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12)

#######################
#+ save data
#######################

subset_vars = c('fips', 'county', 'county_name', 'stateabbrv',
               'statename', 'z_relative_mob', 'z_absolute_mob', 'z_gini', 'log_population', 'log_income', 'z_segregation_income', 'log_unemployment',
               'z_uninsured', 'z_medicare_expenses', 'log_crime_rate', 'log_pct_black',
               'log_pct_hispanic', 'z_obesity', 'z_smoking', 'z_exercise')

subset_vars[!subset_vars %in% names(mob)]
names(mob)
subset_vars
temp = mob[, subset_vars, with = FALSE]
saveRDS(temp, file = 'output/chetty_data.rds')

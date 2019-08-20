########################################
# CDC mortality - income mobility paper
# process chetty's data
# author: sebastian daza
########################################


# utils
source("src/utils/utils.R")

# libraries
library(haven)

# set plot options
options(repr.plot.width = 4, repr.plot.height = 3)

# import and match with chetty data
mob = data.table(haven::read_stata('data/cty_full_covariates.dta'))
mob[, fips := sprintf("%05d", cty)]
mob[, state := sprintf("%02d", state_id)]
length(unique(mob$state)) # 51 states

# adjust covariates chetty's data
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
vars = c('gini', 'relative_mob', 'absolute_mob', 'population', 'density', 'crime_rate',
         'poverty', 'middle_class',
         'mig_inflow', 'mig_outflow', 'foreign', 'pct_black', 'pct_hispanic',
         'religion', 'uninsured', 'medicare_expenses', 'house_value', 'local_gov_exp',
         'college', 'obesity', 'exercise', 'smoking', 'unemployment',
         'labor_force', 'income',  'segregation_income', 'segregation_race')

# transform
z_variables = c('gini', 'relative_mob', 'absolute_mob',
                'middle_class', 'religion',  'labor_force', 'uninsured',
                'medicare_expenses', 'segregation_income',
                'college', 'obesity', 'smoking', 'exercise')

zscores(mob, z_variables)

lookvar(mob, '^z_')

log_variables = c('population', 'crime_rate', 'poverty', 'mig_inflow',
                  'mig_outflow', 'foreign', 'pct_black', 'pct_hispanic',
                  'house_value', 'local_gov_exp', 'unemployment', 'income')

logtrans(mob, log_variables)

vars = lookvar(mob, '^log')
length(vars)


# select variables
subset_vars = c('fips', 'county', 'county_name', 'stateabbrv',
               'statename', 'z_relative_mob', 'z_absolute_mob', 'z_gini',
               'log_population', 'log_income', 'z_segregation_income', 'log_unemployment',
               'z_uninsured', 'z_medicare_expenses', 'log_crime_rate', 'log_pct_black',
               'log_pct_hispanic', 'z_obesity', 'z_smoking', 'z_exercise')

subset_vars[!subset_vars %in% names(mob)]
names(mob)
subset_vars
temp = mob[, subset_vars, with = FALSE]

# save data
saveRDS(temp, file = 'output/chetty_data.rds')

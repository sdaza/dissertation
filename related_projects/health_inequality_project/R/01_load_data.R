
###############################
# load chetty's data
# author: sebastian daza
###############################

library(data.table)
library(sdazar) # https://github.com/sdaza/sdazar
library(haven)

# read data

cov = read_stata('dissertation/related_projects/health_inequality_project/data/cty_full_covariates.dta')
cov = data.table(cov)

# rename columns

ovars = c('cty', 'county_name', 'cty_pop2000', 'statename', 'stateabbrv', 'pop_density',
          'gini99', 's_rank', 'e_rank_b', 'cs00_seg_inc', 'cs_race_theil_2000', 'hhinc00',
          'poor_share', 'frac_middleclass', 'mig_inflow', 'mig_outflow', 'cs_born_foreign',
          'rel_tot', 'crime_total', 'puninsured2010',  'cs_labforce', 'unemp_rate',
          'cs_frac_black', 'cs_frac_hisp', 'bmi_obese', 'cur_smoke','exercise_any',
          'median_house_value', 'cs_educ_ba', 'reimb_penroll_adj10', 'subcty_exp_pc')

nvars = c('county', 'county_name', 'population', 'statename', 'stateabbrv', 'density',
          'gini', 'relative_mob', 'absolute_mob', 'segregation_income', 'segregation_race',
          'income', 'poverty', 'middle_class', 'mig_inflow', 'mig_outflow', 'foreign',
          'religion', 'crime_rate', 'uninsured', 'labor_force', 'unemployment', 'pct_black',
          'pct_hispanic', 'obesity', 'smoking', 'exercise', 'house_value', 'college',
          'medicare_expenses', 'local_gov_exp')

setnames(cov, ovars, nvars)

cov = cov[, ..nvars]

# missing data

m = countmis(cov)
m[m>0]

# impute missing by state




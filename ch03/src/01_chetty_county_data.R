##############################
# county income mobility and individual health
# chetty's county variables
# author: sebastian daza
##############################

# library
library(haven)
library(data.table)
library(readxl)
library(hash)
library(modelr)
source("src/utils.R")

# read data
county = data.table(haven::read_dta("../ch02/data/cty_full_covariates.dta"))
core = data.table(haven::read_dta("data/income_mob_measures.dta"))
gini = data.table(read_excel("data/gini_census.xls", sheet = 2, skip = 2))

# rename columns
setnames(gini,
         c('StateCounty', 'GINI', 'MEAN...11'),
         c('fips', 'gini_census', 'income_average')
         )

gini = gini[, fips := as.numeric(fips)][, .(fips, gini_census, income_average)]

setnames(core, c('county_id', 'gini', 's_rank_8082', 'e_rank_b'),
               c('fips', 'gini_core', 's_rank_core', 'e_rank_b_core'))

core = core[, .(fips, gini_core, s_rank_core, e_rank_b_core)]

setnames(county, 'cty', 'fips')
county = merge(county, core, by = "fips", all.x = TRUE)
county = merge(county, gini, by = "fips", all.x = TRUE)

names(county)
# countmis(county)

# selection of key variables
county = county[, .(fips, statename, county_name, gini_census,
                    s_rank, e_rank_b,
                    hhinc00, cty_pop2000, cs_frac_black)]

county_vars = hash(
    "s_rank" = "relative_mob",
    "e_rank_b" = "absolute_mob",
    "gini_census"  = "gini",
    "hhinc00" = "county_income",
    "cty_pop2000" = "population",
    "cs_frac_black" = "prop_black"
)

renameColumns(county, county_vars)

county[, log_county_income := scale(log(county_income))]
county[, log_population := scale(log(population))]

vars = c("relative_mob", "gini", "absolute_mob")
county[, (paste0("z_", vars)) := lapply(.SD, scale), .SDcol = vars]
county[, ("z_prop_black") := scale(prop_black)]

cor(county[, .(z_gini,
               z_relative_mob, z_absolute_mob,
               log_county_income, log_population, z_prop_black)])

# linear models (residuals)
model_rel_mob = lm(z_relative_mob ~ z_gini + log_population + log_county_income + z_prop_black, data = county)
model_gini = lm(z_gini ~ z_relative_mob  + log_population + log_county_income + z_prop_black, data = county)
model_abs_mob = lm(z_absolute_mob ~ z_gini + log_population + log_county_income + + z_prop_black, data = county)

county = data.table(add_residuals(county, model_rel_mob))
setnames(county, "resid", "relative_mob_resid")
county = data.table(add_residuals(county, model_gini))
setnames(county, "resid", "gini_resid")
county = data.table(add_residuals(county, model_abs_mob))
setnames(county, "resid", "absolute_mob_resid")

# exploring correlations
cor(county[, .(z_relative_mob, relative_mob_resid)])
cor(county[, .(z_absolute_mob, absolute_mob_resid)])
cor(county[, .(z_gini, gini_resid)])

vars = c(vars, "gini_resid", "absolute_mob_resid", "relative_mob_resid")
county[, (paste0("q_", vars)) := lapply(.SD, createQuantiles), .SDcol = vars]

county[, mean(relative_mob, na.rm = TRUE), q_relative_mob]
county[, mean(absolute_mob, na.rm = TRUE), q_absolute_mob]

county = county[, .(fips, statename, county_name, prop_black,
                    gini, z_gini, gini_resid, q_gini, q_gini_resid,
                    relative_mob, z_relative_mob, relative_mob_resid,
                    q_relative_mob, q_relative_mob_resid,
                    absolute_mob, z_absolute_mob, absolute_mob_resid,
                    q_absolute_mob, q_absolute_mob_resid,
                    log_county_income, log_population, z_prop_black
                    )]

setnames(county, "fips", "imp_fips")

# save county data
saveRDS(county, "output/data/chetty_county_data.rds")
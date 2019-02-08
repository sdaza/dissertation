##############################################
# co
# author: sebastian daza
##############################################

#+ set directory
# restricted data (not optimal for reproducibility)
pdata = "/Users/sdaza/Documents/Workplace/Data/mortality/data/"

# libraries
library(here)
library(haven)
library(readr)
library(sdazar)

# functions
source('src/utils/functions.R')

# read fips codes
codes = read_csv('data/fips_codes_website.csv')
codes = data.table(codes)
codes = codes[, c(1,2,3,6), with = FALSE]
setnames(codes, c("abr", "state", "county", "name"))
states = codes[, .N , by = .(abr, state)][, N := NULL]

####################################
#+ load data
####################################

mob = readRDS('output/chetty_data.rds')
mort = readRDS('output/mortality_population.rds')

# some adjustments to compare counties
mort[, fips := paste0(state, county)]
mob[, state := substr(fips, 1, 2)]

vstates = sort(unique(mort$state))
length(vstates)

checkCounties(mob, mort, vstates, 'fips')


#+ adjust some county fips
mob[fips == "51560", ] # nothing to do with this
mob[fips == "51005", ] # remove it also (backwards adjustment)

mob[fips == "12025", fips := "12086"] # to get map better
mort[fips == "12086" & year == 2000, sum(pop) ] # I can change it
# mort[fips == "12086", fips := "12025"]

mort[fips == "02232", fips := "02231"]
mort[fips == "02282", fips := "02231"]
mort[fips == "02068", fips :=  "02290"]

mob[fips == "02231"]
mob[fips %in% c("08001", "08013", "08123", "08059", "08014"), .(fips, log_population)] # nothing to do with this
mort[year == 2014 &
  fips %in% c("08001", "08013", "08123", "08059",    "08014"), .(pop = sum(pop)),  by = .(year, fips)]

mob[county_name == "Weld"]
mob[fips == "02290"]

mort[fips == "15005", fips := "15009"]
mort[fips %in% c("02105", "02230"), fips := "02232"]
mort[fips == "02198", fips := "02201"]
mort[fips %in% c("02275", "02195"), fips := "02280"]

#+ check counties again
checkCounties(mort, mob, vstates, 'fips')

checkCountiesA # 51560 and 51515, 51019
checkCountiesB # 022332, 08014, 51917


##############################################################
#+ aggregate data (years and counties)
##############################################################
mort = mort[, .(deaths = sum(deaths),
                 deaths1 = sum(deaths1),
                 deaths2 = sum(deaths2),
                 deaths3 = sum(deaths3),
                 deaths4 = sum(deaths4),
                 pop = sum(pop)),
            by = .(fips, sex, age, race)] # collapse years!

nrow(mort) # 437025

###########################
#+ merge datasets
###########################

anyDuplicated(mob[, .(fips)])

vars = c("csa", "csa_name", "cbsa", "cbsa_name", "intersects_msa", "state")

names(mob)
names(mort)
# create merge identifiers
mort[, mort := 1]
mob[, mob := 1]

# merge
mort = merge(mort, mob, all = TRUE, by = "fips")
mort[mort == 1 & mob == 1, merge := 3]
mort[mort == 1 & is.na(mob), merge := 1]
mort[mob == 1 & is.na(mort), merge := 2]

# explore some cases
table(mort$merge, useNA = "ifany")
mob[fips == "51019"]
mob[fips == "51015"]

mort[merge == 2, .(fips, merge, log_population )] #
mort[merge == 1, .(fips, merge, log_population )] # 08014
# mort[fips == "08014", .(fips, year, mob, mort, pop)]
# mort[fips == "08001", .(year, fips, mob, mort, pop)]

unique(mort[merge == 1, fips])
unique(mort[merge == 2, fips])
length(unique(mort[merge == 3, fips])) # 3135
mort[, c("mob", "mort") := NULL]

# counties
counties = unique(mort[!is.na(z_relative_mob), fips])# 2873 counties
length(counties) # 2873

anyDuplicated(mort[, .(fips, sex, age, race)]) # ok!

table(mort$sex, useNA = "ifany")
table(mort$age, useNA = "ifany")
table(mort$race, useNA = "ifany")

#+ population greater than 0, match successful, and deaths equal or smaller than population
a = nrow(mort)
mort = mort[pop > 0 & merge == 3 & pop >= deaths]

b = nrow(mort)
round(b / a * 100, 1) # 99.6% most of the cases matched!

#######################
#+ save data
#######################

saveRDS(mort, file = 'output/cdc_chetty.rds')

######################################################
# county income mobility and individual health
# add county information
# author: sebastian daza
######################################################


library(data.table)
library(zoo)
library(readr)
source("src/utils.R")

# read county data
cty00 = fread('data/county2000.csv')
cty10 = fread('data/county2010.csv')
setnames(cty00, names(cty00), tolower(names(cty00)))
setnames(cty10, names(cty10), tolower(names(cty10)))

# select 2010 counties before 2013 and with valid codes 
cty10 = cty10[state10 <98 & county10 < 999]
cty10[, fips := state10 * 1000 + county10]

table(cty00$fipscnty)

cty00 = cty00[fipstate < 98 & fipscnty < 999]
cty00[, fips := fipstate * 1000 + fipscnty]

length(unique(cty10$fips))
length(unique(cty00$fips))

# score of geo match, ignore for now
summary(cty10$score10)

# create key variables
cty10[, year := year10]
cty10[, fn := famid10]
cty10merge = cty10[, .(fips, year, fn)]

cty00[, fn := famid]
cty00merge = cty00[, .(fips, year, fn)]
cty00merge

# read data
data = readRDS("output/data/psid_data_ready_for_imputation.rds")
table(data$year)
names(data)
countmis(data[, .(fn)])

# merge with fips info
data00 = merge(data, cty00merge, by = c("year", "fn"), all.x = TRUE)

countmis(data00[, .(fn, fips)])

data00missing = data00[is.na(fips)][, fips := NULL]
data00 = data00[!is.na(fips)]

data00missing = merge(data00missing, cty10merge, by = c("year", "fn"), all.x = TRUE)
countmis(data00missing[, .(fn, fips)])

data = rbind(data00, data00missing)
countmis(data)

# impute missing fips
setorder(data, pid, year)
data[, flag_missing_fips := ifelse(is.na(fips), 0, 1)]
data[, imp_fips := impute_locf(fips), pid]
countmis(data[, .(fn, fips, imp_fips)])

# add county info
county = readRDS("output/data/chetty_county_data.rds")
data = merge(data, county, by = "imp_fips", all.x = TRUE)

# 172 cases without mobility data
remove_ids = unique(data[head_wife == 1 & (is.na(z_relative_mob) | is.na(z_gini)), pid])

print(
  paste0(
    "Number of respondents without mob or gini info: ",
    length(remove_ids)
  )
)

print(
  paste0(
    "Proportion of respondents without mob or gini info: ",
    round(length(remove_ids) / length(unique(data[head_wife ==1, pid])), 2)
  )
)

data = data[!(pid %in% remove_ids)]

length(unique(data[, imp_fips]))

# cumulative residential moves
setorder(data, year, pid)

data[, lag_imp_fips := shift(imp_fips), pid]
data[, respondent_moved := ifelse(imp_fips == lag_imp_fips, 0, 1), pid]
data[is.na(respondent_moved), respondent_moved := 0]
data[, nmoves := cumsum(respondent_moved), pid]

table(data$nmoves)
summary(data$nmoves)
table(data$time)

moves = data[time <= 20 & head_wife == 1, .(moves = max(nmoves), 
                           missing_fips = sum(flag_missing_fips)), 
             pid]

nrow(moves)

summary(moves$moves)
prop.table(table(moves$moves > 0))
summary(moves$moves)

summary(moves$missing_fips)
prop.table(table(moves$missing_fips == 20))
prop.table(table(moves$missing_fips < 7))
summary(moves$missing_fips)

# save data
saveRDS(data, "output/data/psid_data_ready_for_imputation_county_info.rds" )

##############################
# county income mobility and individual health
# household data
# author: sebastian daza
##############################

# libraries
library(sdazar)
library(stringr)

source("ch03/src/utils.R")

# read raw data
dat = fread("ch03/data/nlsy97/household/20191011_household.csv")

# fine years of panel
years = c(1997:2011, 2013, 2015)

# lowercase of columns names
setnames(dat, names(dat), tolower(names(dat)))

# rename demographics
renameColumns(dat, hash("r0000100", "id"))

# create hash objects to rename columns
relationship_hash = hash(
                         "r131" = 58:74,
                         "r241" = 63:76,
                         "r372" = 69:82,
                         "r519" = 19:31,
                         "r69" = 197:212,
                         "s135" = 39:51,
                         "s341" = 70:82,
                         "s517" = 16:27,
                         "s694" = 69:81,
                         "s892" = 26:39,
                         "t133" = 35:45,
                         "t342" = 44:56,
                         "t484" = 53:70,
                         "t649" = 15:29,
                         "t799" = 46:59,
                         "t925" = 37:48,
                         "u126" = 17:33
                         )

flag_hash = hash(
                "r000" = 58:74,
                 "r240" = 65:78,
                 "r371" = 60:73,
                 "r51" = 798:811,
                 "r690" = 61:76,
                 "s134" = 49:61,
                 "s3" = 4094:4106,
                 "s51" = 507:518,
                 "s691" = 85:97,
                 "s889" = 68:81,
                 "t13" = 101:111,
                 "t3" = 3999:4011,
                 "t48" = 187:204,
                 "t646" = 42:56,
                 "t796" = 61:74,
                 "t925" = 15:26,
                 "u12" = 587:603
                 )

employment_hash = hash(
                       "r10" = 898:907,
                       "r240" =  32:45,
                       "r395" =  13:25,
                       "r517" =  50:61,
                       "r6" = 9008:9020,
                       "s13" = 402:414,
                       "s340" = 56:67,
                       "s51" = 401:411,
                       "s690" = 36:46,
                       "s88" = 807:819,
                       "t129" = 51:61,
                       "t338" = 38:47,
                       "t480" = 37:49,
                       "t644" = 83:92,
                       "t79" = 491:500,
                       "t92" = 505:514,
                       "u125" = 74:86
                       )

marital_hash = hash(
                    "r110" = 42:54,
                    "r241" =  21:34,
                    "r372" =  27:40,
                    "r518" =  78:89,
                    "r691" = 52:64,
                    "s134" = 75:87,
                    "s341" = 20:31,
                    "s51" = 191:201,
                    "s686" = 86:96,
                    "s88" = 505:517,
                    "t126" = 52:62,
                    "t335" = 25:36,
                    "t477" = 10:23,
                    "t641" = 29:39,
                    "t791" = 37:46,
                    "t924" = 84:94,
                    "u125" = 53:64
                    )

# create list of hashes
list_hash = list(relationship_hash, flag_hash, employment_hash, marital_hash)

# add missing columns
dat[, paste0("r000", 58:74, "00") := NA]

# loop to rename and melt data
header_vars = c("relationship_", "status_", "employment_", "marital_")
pattern_vars = c("^relationship", "^status", "^employment_", "^marital")
value_names = c("relationship", "status", "employment", "marital")

for (i in seq_along(list_hash)) {
    renameColumns(dat, hashHHColumns(list_hash[[i]], years, header_vars[i]))
}

melt_list = list()

# everything as numeric
dat = dat[, lapply(.SD, as.numeric)]

for (i in seq_along(pattern_vars)) {

    temp = melt(dat,
                id.vars = "id",
                measure = sort(lookvar(dat, pattern_vars[i])),
                value.name = value_names[i],
                variable.name= "name",
                variable.factor = FALSE
                )

    temp[, year := as.numeric(str_extract(name, "19[0-9]+|20[0-9]+"))]
    temp[, person := as.numeric(str_extract(name, "[0-9]+"))]
    temp[, name := NULL]
    melt_list[[i]] = temp

}

hdat = Reduce(function(...) merge(..., by = c("id", "year", "person")), melt_list)


# create index by year
hdat[year == 1997 & is.na(status), status := 1]

# code for parents

# 3 Mother
# 4 Father
# 5 Adoptive mother
# 6 Adoptive father
# 7 Step-mother
# 8 Step-father
# 9 Foster mother
# 10 Foster father

hdat[, living_any_parent := ifelse(relationship %in% 3:10, 1, 0)]
hdat[, living_any_parent := getMax(living_any_parent), .(id, year)]
# hdat = hdat[relationship %in% 3:10]

np = unique(hdat[living_any_parent == 0], by = c("id", "year"))
np[, employment := NA]
np[, marital := NA]

pp = hdat[living_any_parent == 1 & relationship %in% 3:10]

hdat = rbind(pp, np)

# employment
vars = c("employment", "marital")
hdat[, (vars) := lapply(.SD, replaceMissing), .SDcol = vars]


table(hdat[, .(year, employment)])
hdat[, employment := ifelse(employment == 0, NA, employment)]
hdat[year == 1997, parent_employed := ifelse(employment == 1, 1, 0)]
hdat[year > 1997, parent_employed := ifelse(employment %in% c(1,2), 1, 0)]
hdat[is.na(employment), parent_employed := NA]
hdat[, parent_married := ifelse(marital == 1, 1, 0)]

table(hdat[, .(year, parent_employed)])
table(hdat[, .(year, parent_married)])

table(hdat[, .(year, status)])

table(hdat$relationship)

hhdat = hdat[, .(parent_married = getMax(parent_married),
                 parent_employed = getMax(parent_employed),
                 living_any_parent = getMax(living_any_parent)),
             .(id, year)]

ids = unique(hhdat$id)
hhdat[id == sample(id, 1)]

saveRDS(hhdat, 'ch03/output/data/nlsy97_household.rds')
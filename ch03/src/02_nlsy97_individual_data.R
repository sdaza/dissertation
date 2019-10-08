##############################
# load individual NLSY97 data
# author: sebastian daza
# version: 0.01
##############################

# todo

# add date of interview and impute ages (use average date by wave)
# fill records so that to complete the period from 12 to the start of the panel
# identify more reasonable time of outcomes, otherwise, adjust by age of measurement
# think about imputation and exposure more generally

# libraries
library(data.table)
library(sdazar)
library(texreg)
library(ggplot2)
library(zoo)
library(hash)
library(mice)

# read raw data
dat = fread("ch03/data/nlsy97/individual/20191008_selection.csv")

# auxiliary functions
table = function (...) base::table(..., useNA = 'ifany')
cor = function (...) stats::cor(..., use = "complete.obs")

impute_locf = function(x) {
    output = na.locf(na.locf(x, FALSE), fromLast=TRUE)
    return(output)
}

renameColumns = function(dat, hash) {
    oldnames = keys(hash)
    newnames = values(hash)

    if (length(oldnames) != length(newnames)) {
        stop("Vector of names should have the same length")
    }

    setnames(dat, oldnames, newnames)
}


fillMissingColumns = function(dat, expression, expected) {
    varnames = names(dat)
    observed = grep(expression,  varnames, value = TRUE)
    dat[, ( expected[!expected %in% observed] ) := NA]
}

replaceMissing = function(x) {
    return(
        ifelse(x < 0, NA, x)
    )
}

get_max = function(x) {
    x = na.omit(x)
    if (length(x) == 0) {
        return(NA_real_)
    } else {
        return(max(x))
    }
}

# fine years of panel
years = c(1997:2011, 2013, 2015)

# lowercase of columns names
setnames(dat, names(dat), tolower(names(dat)))

# rename demographics
hash_demographics = hash(
    "r0000100" = "id",
    "r0536300" = "sex",
    "r0536401" = "birth_month",
    "r0536402" = "birth_year",
    "r1194000" = "age",
    "r0538700" = "race",
    "r0538600" = "hispanic",
    "r1235800" = "type",
    "r1236201" = "wt",
    "r1489700" = "stratum",
    "r1489800" = "cluster"
)

renameColumns(dat, hash_demographics)

dat[, male := ifelse(sex == 1, 1, 0)]

table(dat$age) # from 12 to 16

dat[, race := ifelse(race < 0, NA, race)]
table(dat$race)
table(dat$hispanic)

# table(dat$urban)
table(dat$type)
summary(dat$wt)
summary(dat$stratum)
table(dat$cluster)

# non-response situation
ovars = c("r2510200", "r3827700", "r5341500", "r7085400", "s1524700",
          "s3590300", "s4966600", "s6706700", "s8679600", "t1099500",
          "t3176800", "t4587900", "t6221000", "t7718200", "t9118900",
          "u1110400")
nvars = paste0("nres", years[-1])

renameColumns(dat, hash(ovars, nvars))
dat[, nres1997 := -4]

# age at interview date
ovars = c("r1194100", "r2553500", "r3876300", "r5453700", "r7216000",
          "s1531400", "s2001000", "s3801100", "s5401000", "s7501200",
          "t0008500", "t2011100", "t3601500", "t5201400", "t6651300",
          "t8123600", "u0001800")
nvars = paste0("age_", years)
renameColumns(dat, hash(ovars, nvars))

#########################################################
# Outcomes measured in 2015
# I used previous values only for imputation
# Exposure is assumed to happen during childhood (1-18 years old)
#########################################################

# subjective health
ovars = c("r0320600","r2164000","r3481900","r4880100","r6497500","s1225000",
           "s3302500","s4919500","s6661100","s8644200","t1049500","t3144600",
           "t4562200","t6206400","t7703800","t9093100", "u1096500")

nvars = paste0("shealth", years)
renameColumns(dat, hash(ovars, nvars))

# bmi

# high, feet
ovars = c("r0322500", "r2164100", "r3482000", "r4880200", "r6497600",
          "s0905500", "s2978200", "s4677000", "s6309200", "s8330100",
          "t0737900", "t2780200", "t4494600", "t6141700", "t7635600")

nvars = paste0("height_feet_", years[1:15])
renameColumns(dat, hash(ovars, nvars))

# high, inches
ovars = c("r0322600", "r2164200", "r3482100", "r4880300", "r6497700",
          "s0905600", "s2978300", "s4677100", "s6309300", "s8330200",
          "t0738000", "t2780300", "t4494700", "t6141800", "t7635700")

nvars = paste0("height_inches_", years[1:15])
renameColumns(dat, hash(ovars, nvars))

# weigth in pounds
ovars = c("r0322700", "r2164300", "r3482200", "r4880400", "r6497800", "s0905700",
          "s2978400", "s4677200", "s6309400", "s8330300", "t0738100", "t2780400",
          "t4494800", "t6141900", "t7635800", "t9039300", "u1028700")

nvars = paste0("weight_", years)
renameColumns(dat, hash(ovars, nvars))

# depression
depression_items = list(
    "2000" = c("r4893600", "r4893700", "r4893800", "r4893900", "r4894000"),
    "2002" = c("s0920800", "s0920900", "s0921000", "s0921100", "s0921200"),
    "2004" = c("s4681900", "s4682000", "s4682100", "s4682200", "s4682300"),
    "2006" = c("s8332300", "s8332400", "s8332500", "s8332600", "s8332700"),
    "2008" = c("t2782600", "t2782700", "t2782800", "t2782900", "t2783000"),
    "2010" = c("t6143700", "t6143800", "t6143900", "t6144000", "t6144100"),
    "2015" = c("u1030700", "u1030800", "u1030900", "u1031000", "u1031100")
)

for (i in seq_along(depression_items)) {
    renameColumns(dat,
               hash(depression_items[[i]],
                    paste0("dep", 1:5, "_", names(depression_items)[i]))
               )
}

# smoking

# ever
ovars = c("r0357900", "r2189400", "r3508500", "r4906600", "r6534100",
          "s0921600", "s2988300", "s4682900", "s6318400", "s8333400",
          "t0740200", "t2783700", "t4495400", "t6144300", "t7638800",
          "t9040800", "u1031300")

nvars = paste0("smoking_ever_", years)
renameColumns(dat, hash(ovars, nvars))

# number of days smoking last 30 days
ovars = c("r0358100", "r2189500", "r3508600", "r4906700", "r6534200",
          "s0921700", "s2988400", "s4683000", "s6318500", "s8333500",
          "t0740300", "t2783800", "t4495500", "t6144400", "t7638900",
          "t9040900", "u1031400")

nvars = paste0("smoking_30_days", years)
renameColumns(dat, hash(ovars, nvars))

##############################
# covariates
##############################

# list of covariates

# gender x
# birth year x
# optimism x
# marital status at birth
# mother's age at birth x

# base line

# health baseline
# smoking baseline

# weight at birth
# education parents x
# public assistance
# income mobility county x
# cumulative residential moves x
# size household x
# home ownership

# time varying

# marital status
# work / hours / hh
# income
# employment
# income mobility current county
# size household

# time invariant

timeinvariant_vars = hash(
    "r1200200" = "mother_age_at_birth",
    "r1302600" = "father_highest_grade",
    "r1302700" = "mother_highest_grade",
    "r0357500" = "optimism_1",
    "r0357600" = "optimism_2",
    "r0357700" = "optimism_3",
    "r0357800" = "optimism_4",
    "r0642200" = "residential_moves_by_12"
)

renameColumns(dat, timeinvariant_vars)

# time variant

# income
ovars = c("r1204500","r2563300","r3884900","r5464100","r7227800","s1541700",
           "s2011500","s3812400","s5412800","s7513700","t0014100","t2016200",
           "t3606500","t5206900","t6656700","t8129100", "u0008900")

nvars  = paste0("income", years)
renameColumns(dat, hash(ovars, nvars))
summary(dat$income1997)

# household size
ovars = c("r1205400", "r2563700", "r3885300", "r5464500", "r7228200", "s1542100",
          "s2011900", "s3813400", "s5413000", "s7513900", "t0014300", "t2016400",
          "t3606700", "t5207100", "t6656900", "t8129300", "u0009100")

nvars = paste0("hhsize", years)
renameColumns(dat, hash(ovars, nvars))

#############################
# transform to long format
#############################

expected = sort(as.vector(outer(paste0("dep", 1:5, "_"), years, paste0)))
fillMissingColumns(dat, "^dep[1-5]", expected)

expected = c(paste0("height_inches_", years), paste0("height_feet_", years))
fillMissingColumns(dat, "^height_feet|^height_inches", expected)

vars = lookvar(dat,
               "id|nres|income|shealth|^dep[1-5]|optimism|race|hispanic|sex|age|birth_month|birth_year|shealth
                |smoking_ever_|smoking_30|^height_feet|^height_inches|^weight_|^hhsize|type|wt|stratum|cluster
                |residential_moves|mother_age_at_birth|mother_highest_grade|father_highest_grade"
               )

dat = dat[, lapply(.SD, as.numeric)]

vars = sort(vars)
sdat = dat[, ..vars]

patt = c("^shealth", "^income", "^nres", "^hhsize", "^weight_",
         "^height_feet", "^height_inches_", "^dep1", "^dep2",
         "^dep3", "^dep4", "^dep5", "^smoking_ever_", "^smoking_30", "^age_")
vnames = c("health", "income", "nres", "hhsize", "weight", "height_feet", "height_inches",
           "dep1", "dep2", "dep3", "dep4", "dep5", "smoking_ever", "smoking_30",
           "age_interview")

length(patt) == length(vnames)

ldat = melt(
    sdat,
    measure.vars = patterns(patt),
    value.name = vnames,
    variable.name = "time"
)

ldat[, time := as.numeric(time)]

dim(ldat)
ldat[, .(id, time, height_inches, age_interview)]

for (i in seq_along(years)) {
    ldat[time == i, year := years[i]]
}

str(ldat)
ldat = ldat[, lapply(.SD, function(x) ifelse(x < 0, NA, x))]
ldat[is.na(nres), nres := 60]

table(ldat$time)
ids = unique(ldat$id)

# bmi

# carry forward height and inches
ldat[, max_height_inches := get_max(height_inches), id]
ldat[, max_height_feet := get_max(height_feet), id]
ldat[, bmi := 703 * weight / (max_height_inches + max_height_feet * 12) ^ 2]

ldat
ldat[bmi > 70, bmi := NA]

ldat[id == sample(ids, 1), .(id, nres, income, year, age, age_interview, weight,
                             height_inches, height_feet, bmi)]

dim(ldat)
summary(ldat$hhsize)

ldat[bmi > 70, .(weight, height_inches, height_feet, bmi)]

# fill ages with missing


# vector with ids
ids = unique(ldat$id)
ldat[id == sample(ids, 1)]
ldat[, response := ifelse(nres < 0 | (nres >= 60 & nres <= 69), 1, 0)]


setorder(ldat, id, -year)
ldat[, cumresponses := cumsum(response), id]
ldat[, maxresponses := max(cumresponses), id]
ldat[, dropout := ifelse(cumresponses == 0, 1, 0)]
ldat[, dropout := max(dropout), id]
# ldat = ldat[cumresponses > 0]
setorder(ldat, id, year)

# explore samples
ldat[id == sample(ids, 1)]

# load location data
loc = readRDS("ch03/output/data/nlsy97_location.rd")
ldat[, time := as.numeric(time)]
ldat = merge(ldat, loc, by = c("id", "time"), all = TRUE)

ldat[, age_min := min(age, na.rm = TRUE), id]

ldat = ldat[age_min > 12]
names(ldat)

ids = unique(ldat$id)
ldat[id == sample(ids, 1), .(id, age, age_min, birth_month, birth_year, time, year,
                             response, fips, flag12, s_rank)]

setorder(ldat, id, time)
columns_to_impute = c('income', 'sex', 'race', 'health', 's_rank')
ldat[, paste0('imp_', columns_to_impute) := lapply(.SD, impute_locf), by = id, .SDcol = columns_to_impute]

names(ldat)
# test ipw
library(ipw)

testdat = ldat[complete.cases(ldat[, .(id, imp_s_rank, imp_race, imp_sex, imp_health, imp_income)])]

# create lag varible
# setorder(testdat, id, time)
# testdat[, lag_imp_s_rank := shift(imp_s_rank), id]
# testdat[, lag_imp_health := shift(imp_health), id]
# testdat[, lag_imp_income := shift(imp_income), id]

fdata = testdat
# test = data.table(campaign_long)
# table(test$week)

variables = c("id", "imp_s_rank", "imp_sex", "imp_income", "imp_race")
fdata = testdat[complete.cases(testdat[, ..variables])]

temp = ipwtm(exposure = imp_s_rank,
             family = "gaussian",
             numerator = ~ as.factor(imp_sex) + as.factor(imp_race),
             denominator = ~ as.factor(imp_sex) + as.factor(imp_race) + imp_income + time,
             timevar = time,
             type = "all",
             corstr = "ar1",
             id = id,
             data = fdata)

fdata[, ipw := temp$ipw.weights]
exposure = fdata[time < 9, .(avg_s_rank = mean(imp_s_rank, na.rm = TRUE)), id]
fdata = merge(fdata, exposure, by = "id")
fdata

last_obs = fdata[, .SD[.N], by=id]
last_obs

ids = unique(last_obs$id)
last_obs[id == sample(ids, 1), .(id, year, imp_s_rank, avg_s_rank, time, ipw)]

m0 = lm(imp_health ~  avg_s_rank + as.factor(imp_sex) + as.factor(imp_race),
        weights = last_obs$ipw, data = last_obs)
summary(m0)


m0 = lm(imp_health ~ avg_s_rank, data = last_obs)
summary(m0)

require(MASS)
require(Hmisc)

last_obs = last_obs[imp_health > 0][, imp_health := factor(imp_health)]
m <- polr(imp_health ~ avg_s_rank, weights = last_obs$ipw, data = last_obs, Hess=TRUE)
summary(m)

–

length(temp$ipw.weights)
finalw = testdat[, .SD[.N], id]
# recode some variables
table(ldat$health)
ldat[, health := ifelse(health < 0, NA, health)]
table(ldat$health)

ldat[, male := ifelse(sex == 1, 1, 0)]
table(ldat$male)
ldat

# create age variable
setkey(ldat, id, year)
ldat[, s := 1:.N, id]
ldat[, agei := age + s - 1]
ldat[id == 10, .(id, year, age, agei)]

countmis(ldat) # 14%

saveRDS(ldat, 'ch03/output/data/nlsy97_analytic.rd')

# number of days smoking

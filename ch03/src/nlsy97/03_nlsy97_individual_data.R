##############################
# county income mobility and individual health
# individual data
# author: sebastian daza
##############################

# libraries
library(data.table)
library(texreg)
library(ggplot2)
library(zoo)
library(haven)
library(hash)
library(lubridate)
library(readxl)
source("src/utils.R")

# read raw data
dat = fread("data/nlsy97/individual/20191008_selection.csv")

# auxiliary functions
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
    "r1482600" = "ethnicity",
    "r1235800" = "type",
    "r1236201" = "wt",
    "r1489700" = "stratum",
    "r1489800" = "cluster",
    "r9829600" = "asvab_score"
)

renameColumns(dat, hash_demographics)

dat[, asvab_score := replaceMissing(asvab_score) / 1000]
dat[, male := ifelse(sex == 1, 1, 0)]

table(dat$age) # from 12 to 16

dat[, race := ifelse(race < 0, NA, race)]
dat[, hispanic := ifelse(race < 0, NA, race)]

table(dat$race)
table(dat$hispanic)
table(dat$ethnicity)

# table(dat$urban)
table(dat$type)
summary(dat$wt)
summary(dat$stratum)
table(dat$cluster)

# interview date

# month
ovars = c("r1209401", "r2568301", "r3890301", "r5472301", "r7236101",
          "s1550901", "s2020801", "s3822001", "s5422001", "s7524101",
          "t0024501", "t2019401", "t3610001", "t5210401", "t6661401",
          "t8132901", "u0013201")

nvars = paste0("interview_month_", years)
renameColumns(dat, hash(ovars, nvars))

# year
ovars = c("r1209402", "r2568302", "r3890302", "r5472302", "r7236102",
          "s1550902", "s2020802", "s3822002", "s5422002", "s7524102",
          "t0024502", "t2019402", "t3610002", "t5210402", "t6661402",
          "t8132902", "u0013202")
nvars = paste0("interview_year_", years)
renameColumns(dat, hash(ovars, nvars))

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

# self-reporte health
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

ovars = c("r0357900", "r2189400", "r3508500", "r4906600", "r6534100",
          "s0921600", "s2988300", "s4682900", "s6318400", "s8333400",
          "t0740200", "t2783700", "t4495400", "t6144300", "t7638800",
          "t9040800", "u1031300")

table(dat$u1031300)
nrow(dat)

nvars = paste0("smoking_", years)
renameColumns(dat, hash(ovars, nvars))

# number of days smoking last 30 days
# number of cigarretes per day was asked until 2011
ovars = c("r0358100", "r2189500", "r3508600", "r4906700", "r6534200",
          "s0921700", "s2988400", "s4683000", "s6318500", "s8333500",
          "t0740300", "t2783800", "t4495500", "t6144400", "t7638900",
          "t9040900", "u1031400")

nvars = paste0("smoking_days_days", years)
renameColumns(dat, hash(ovars, nvars))

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

summary(dat$income2015)

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

columns_expr = c("id", "nres", "interview_month", "interview_year", "income",
                 "shealth", "^dep[1-5]", "hispanic", "optimism", "race", "ethnicity",
                 "sex", "age", "birth_month", "birth_year",
                 "shealth", "smoking_", "smoking_days", "^height_feet",
                 "^height_inches", "^weight_", "^hhsize", "type", "wt", "stratum",
                 "cluster", "residential_moves", "mother_age_at_birth",
                 "mother_highest_grade", "father_highest_grade", "asvab_score")

expr = paste0(columns_expr, collapse = "|")
vars = lookvar(dat, expr)
dat = dat[, lapply(.SD, as.numeric)]

vars = sort(vars)
sdat = dat[, ..vars]

pattern_names = hash(
    "^shealth" = "health",
    "^income" = "income",
    "^nres" = "nres",
    "^interview_month" = "interview_month",
    "^interview_year" = "interview_year",
    "^hhsize" = "hhsize",
    "^weight_" = "weight",
    "^height_feet" = "height_feet",
    "^height_inches_" = "height_inches",
    "^dep1" = "dep1",
    "^dep2" = "dep2",
    "^dep3" = "dep3",
    "^dep4" = "dep4",
    "^dep5" = "dep5",
    "^smoking_" = "smoking",
    "^smoking_days" = "smoking_days",
    "^age_" = "age_interview"
)

# long format data
ldat = melt(
    sdat,
    measure.vars = patterns(keys(pattern_names)),
    value.name = values(pattern_names),
    variable.name = "time"
)

# ldat[id == 2413]

# define years based on time
for (i in seq_along(years)) {
    ldat[time == i, year := years[i]]
}

table(ldat[year == 2015, smoking])
# impute interview age
vars = c("birth_year", "birth_month", "interview_month", "interview_year", "age_interview")
ldat[, (vars) := lapply(.SD, replaceMissing), .SDcol = vars]

ldat[, dob := ymd(paste0(birth_year, "-", birth_month, "/", 15))]
ldat[, mean_interview_month := floor(mean(interview_month, na.rm = TRUE)), time]
ldat[, interview_month := ifelse(is.na(interview_month), mean_interview_month, interview_month)]
ldat[, interview_year := ifelse(is.na(interview_year), year, interview_year)]
ldat[!is.na(mean_interview_month), doi := ymd(paste0(interview_year, "-", interview_month, "-", 15))]
ldat[, age_interview_est := ifelse(is.na(age_interview),
                                   floor(time_length(difftime(doi, dob), "years")),
                                   age_interview)]

# ldat[id == 2413]
summary(ldat$age_interview_est)

ldat[, time := as.numeric(time)]
setorder(ldat, id, time)

# impute as *interview completed* missing non-responded values
ldat[is.na(nres), nres := 60]
ldat[nres == -4, nres := 60]

dim(ldat)

# merge with household info
hh = readRDS("output/data/nlsy97_household.rds")
ldat = merge(ldat, hh, by = c("id", "year"), all.x = TRUE)

ldat[nres > 69, living_any_parent := NA]

ids = unique(ldat$id)
ldat[id == sample(ids, 1), .(id, year, nres, living_any_parent,
                             age_interview_est, income, parent_married, parent_employed)]

# impute parent status variables to facilitate imputation
setorder(ldat, id, year)
hh_vars = c("living_any_parent", "parent_employed", "parent_married")
ldat[age_interview_est <= 20, (paste0("imp_", hh_vars)) := lapply(.SD, impute_locf), id, .SDcol = hh_vars]
ldat[, (paste0("imp_", hh_vars)) := lapply(.SD, impute_locf), id, .SDcol = paste0("imp_", hh_vars)]

# merge all values
loc = readRDS("output/data/nlsy97_location.rds")
ldat = merge(ldat, loc, by = c("id", "time"), all = TRUE)

# create vector of ids for exploring data
ids = unique(ldat$id)
ldat[, min_age := getMin(age_interview_est), id]
ldat[, min_year := getMin(year), id]
ldat[, diff_age_12 := min_age - 12]

# ldat[id == 2413]
ldat[flag12 == 1 & diff_age_12 > 0, age_interview_est := 12]
ldat[flag12 == 1 & diff_age_12 > 0, year := min_year - diff_age_12]
ldat[flag12 == 1 & diff_age_12 > 0 & !is.na(fips), nres := 60]
ldat[flag12 == 1 & diff_age_12 > 0 & is.na(fips), nres := 90]

# impute missing fips for age 12 using location data
setorder(ldat, id, time)
fips12  = ldat[flag12 == 1, .(fips12 = head(fips, 1)), id]
ldat = merge(ldat, fips12, by = "id", all.x = TRUE)
ldat[flag12 == 0 & time == 1 & is.na(fips), fips := fip12]
ldat[is.na(flag12), flag12 := 0]

# ldat[id == sample(ids, 1), .(id, year, time, age_interview_est,
#                              flag12, diff_age_12)]

# remove redundant rows at age 12
ldat = ldat[!(flag12 == 1 & diff_age_12 == 0)]

# fill missing ages from 12 to first wave
ldat[, first_year := getMin(year), id]
ldat[, last_year := getMax(year), id]

vmin_year = min(ldat$first_year)
vmax_year = max(ldat$last_year)

# ldat[, c("min_year") := NULL]
setkey(ldat, id, year)
tt = ldat[CJ(id, year = seq(vmin_year, vmax_year), unique = TRUE)]
tt[id == 2, .(time, year, age_interview_est, first_year, last_year)]
tt[, first_year := getMin(first_year), id]
tt[, last_year := getMax(last_year), id]
tt = tt[year >= first_year & year <= last_year]
setorder(tt, id, year)
tt = tt[year %in% c(vmin_year:1996, years)]
oldat = data.table::copy(ldat)
ldat = data.table::copy(tt)

ldat[, flag_missing_exposure := ifelse(is.na(time), 0, 1)]

# fill values for time invariant covariates
baseline_vars = c(paste0("optimism_", 1:4), "time", "age", "residential_moves_by_12",
                  "mother_age_at_birth", "father_highest_grade", "mother_highest_grade",
                  "sex", "race", "hispanic", "ethnicity", "stratum", "type", "wt",
                  "interview_month", "interview_year", "asvab_score",
                  "birth_month", "birth_year", "dob", "doi", "mean_interview_month")

ldat[, (baseline_vars) := lapply(.SD, replaceMissing), .SDcol = baseline_vars]
ldat[, (baseline_vars) := lapply(.SD, fillWithFirstValue), id, .SDcol = baseline_vars]

ldat[, age_interview_est := imputeAge(age_interview_est, year)]

ldat[id == sample(ids, 1), .(id, year, time, age_interview_est, min_age)]
ldat[, min_time := min(time), id]

table(dat$age)

uniqueN(ldat[min_time > 0, id])
uniqueN(ldat[min_time == 0, id])
ldat[id == sample(ids, 1),]

# income adjustments
cpi = fread("data/cpi.csv")

ldat[, previous_year := year - 1]
ldat = merge(ldat, cpi, by.x = "previous_year", by.y = "year")

ldat[income %in% c(-5:-1), income := NA]
prop.table(table(is.na(ldat[year == 2011, income])))
ldat[, income_adj := income * cpi / 100]
prop.table(table(is.na(ldat[year == 2011, income_adj])))
ldat[income_adj > 0, log_income_adj := log(income_adj)]
ldat[income_adj < 1, log_income_adj := log(1)]
ldat[, log_income_adj := scale(log_income_adj, scale = FALSE)]
summary(ldat$income_adj)

prop.table(table(is.na(ldat[year == 2015, log_income_adj])))

# explore
ids = unique(ldat$id)
ldat[id == sample(ids, 1), .(id, year, income, income_adj)]
# summary(ldat[is.na(log_income_adj), .(income_adj, log_income_adj)])

# compute exposure time
setorder(ldat, id, time)
ldat[, lead_year := shift(year, type = "lead"), id]
ldat[, exposure_time := lead_year - year]
ldat[year == 2015 & is.na(exposure_time), exposure_time := 1]

# exposure?
ldat[, stime := 1:.N, id]
ldat[id == sample(ids, 1), .(id, year, age_interview_est, exposure_time, stime)]

exposure_periods = ldat[stime <= 8,
                        .(rows = .N, years = sum(exposure_time)), id]
summary(exposure_periods$rows)
summary(exposure_periods$years)

# sex
ldat[, male := ifelse(sex == 1, 1, 0)]

# household size
ldat[hhsize < 0, hhsize := NA]

# impute values

# fips
setorder(ldat, id, time)
ldat[, flag_missing_fips := ifelse(is.na(fips), 0, 1)]
ldat[, imp_fips := impute_locf(fips), id]

# load chetty's county data
county = readRDS("output/data/chetty_county_data.rds")
names(county)
cor(county[, .(gini, relative_mob)])
cor(county[, .(gini, absolute_mob)])
sd(county$relative_mob, na.rm = TRUE)

ldat = merge(ldat, county, by = "imp_fips", all.x = TRUE)


remove_ids = unique(ldat[is.na(z_relative_mob) | is.na(z_gini), id])
print(
    paste0(
        "Number of respondents without mob or gini info: ",
        length(remove_ids)
    )
)

ldat = ldat[!(id %in% remove_ids)]
print(paste0("Number of respondents: ", length(unique(ldat$id))))
print(paste0("Number of rows: ", nrow(ldat)))

# education parent (max value)
ldat[, parent_education := pmax(ifelse(father_highest_grade == 95,
                                       1,
                                       father_highest_grade),
                                ifelse(mother_highest_grade == 95,
                                       1,
                                       mother_highest_grade),
                                na.rm = TRUE)]

table(ldat$parent_education)
table(ldat$mother_age_at_birth)
ldat[mother_age_at_birth < 12 | mother_age_at_birth > 54, mother_age_at_birth := NA]
table(ldat$residential_moves_by_12)

# outcomes

#  health
ldat[, health := replaceMissing(health)]
ldat[, rev_health := reverseScale(health)]
table(ldat[, .(health, rev_health)])

# depression
depression_cols = paste0("dep", 1:5)

ldat[, (depression_cols) := lapply(.SD, replaceMissing), .SDcol = depression_cols]

depression_cols_positive = c("dep1", "dep3", "dep5")
ldat[, (paste0("rev_", depression_cols_positive)) := lapply(.SD, reverseScale),
     .SDcols = depression_cols_positive]

ldat[, depression := apply(.SD, 1, mean, na.rm = TRUE),
     .SDcol = c("rev_dep1", "dep2", "rev_dep3", "dep4", "rev_dep5")]

setorder(ldat, id, depression)
ldat[year < 2015, depression := impute_locf(depression), .(id)]

ldat[, ethnicity := factor(ethnicity)]

# smoking
ldat[year %in% c(2013, 2015), smoking := ifelse(smoking == -4, 0, smoking)]
smoking_cols = c("smoking", "smoking_days")

ldat[, smoking_days := ifelse(smoking == 0 & smoking_days == -4,
                            0,
                            smoking_days)]

ldat[, (smoking_cols) := lapply(.SD, replaceMissing), .SDcol = smoking_cols]

# bmi
setorder(ldat, id, time)
bmi_cols = c("weight", "height_feet", "height_inches")
ldat[, (bmi_cols) := lapply(.SD, replaceMissing), .SDcol = bmi_cols]

height = c("height_feet", "height_inches")
ldat[age_interview_est >= 25, (height) := lapply(.SD, fillWithLastValue), id, .SDcol = height]

summary(ldat[, ..bmi_cols])

ids = unique(ldat$id)
ldat[id == sample(id, 1), .(id, age_interview_est, year, weight,
                            height_feet, height_inches)]
ldat[, bmi := 703 * weight / (height_inches + height_feet * 12) ^ 2]
summary(ldat[year == 2015, bmi])

ldat[bmi > 40, bmi := 40]
ldat[bmi < 15, bmi := 15]

# cumulative residential moves
setorder(ldat, year, id)

ldat[, lag_imp_fips := shift(imp_fips), id]
ldat[id == sample(ids, 1), .(id, time, stime, year, imp_fips, lag_imp_fips)]
ldat[, respondent_moved := ifelse(imp_fips == lag_imp_fips, 0, 1), id]
ldat[is.na(respondent_moved), respondent_moved := 0]
table(ldat$respondent_moved)

ldat[, nmoves := cumsum(respondent_moved), id]
table(ldat$nmoves)
summary(ldat$nmoves)

ldat[id == sample(ids, 1), .(id, time, stime, year,
                             imp_fips, lag_imp_fips, nmoves)]

ldat[, max_age_interview_est := getMax(age_interview_est), id]
# ldat[, age_interview_est := factor(age_interview_est)]
ldat[, max_age_interview_est := factor(max_age_interview_est)]

# checking income missing data
table(ldat$flag12)
table(ldat$stime)
table(is.na(ldat[, income]))
table(is.na(ldat[, imp_parent_married]))
prop.table(table(!is.na(ldat[year == 1997 & flag12 == 1, income])))
prop.table(table(!is.na(ldat[age > 12 & stime == 1, imp_parent_married])))
prop.table(table(!is.na(ldat[stime == 2 & flag12 == 0 , imp_parent_married])))

# sampling weight data
setorder(ldat, id, year)
ldat = ldat[, `:=` (stratum = getFirst(stratum),
    cluster = getFirst(cluster),
    sweight = getFirst(wt)), .(id)]
print(paste0("Number of rows: ", nrow(ldat)))

# save final data
saveRDS(ldat, "output/data/nlsy97_data_ready_for_imputation.rds")
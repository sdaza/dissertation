##############################
# load individual NLSY97 data
# author: sebastian daza
# version: 0.01
##############################

# libraries
library(data.table)
library(sdazar)
library(texreg)
library(ggplot2)
library(zoo)
library(haven)
library(hash)
library(mice)
library(lubridate)
library(ipw)


source("ch03/src/utils.R")

# read raw data
dat = fread("ch03/data/nlsy97/household/household_20191011.csv")

# fine years of panel
years = c(1997:2011, 2013, 2015)

# lowercase of columns names
setnames(dat, names(dat), tolower(names(dat)))

# rename demographics
renameColumns(dat, hash("r0000100", "id"))

employment = c(
               paste0("r10", 898:907, "00"),
               paste0("r240", 32:45, "00"),
               paste0("r395", 13:25, "00"),
               paste0("r517", 50:61, "00")
               )

new_employment = NULL
num_columns = list(
                   seq_along(898:907),
                   seq_along(32:45),
                   seq_along(13:25),
                   seq_along(50:61)
                   )

for (i in seq_along(years[1:4])) {
    new_employment = c(new_employment,
                       apply(expand.grid("employment",
                                         as.character(num_columns[[i]]),
                                         years[i]),
                             1,
                             paste0,
                             collapse = "_")
                       )
}

renameColumns(dat, hash(employment, new_employment))


test = melt(dat,
     id.vars = 'id',
     measure = patterns("^employment_"),
     value.name = c("test")
    )

table(test$test)

length(898:907)
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

columns_expr = c("id", "nres", "interview_month", "interview_year", "income",
                 "shealth", "^dep[1-5]", "hispanic", "optimism", "race", "ethnicity",
                 "sex", "age", "birth_month", "birth_year",
                 "shealth", "smoking_ever_", "smoking_30", "^height_feet",
                 "^height_inches", "^weight_", "^hhsize", "type", "wt", "stratum",
                 "cluster", "residential_moves", "mother_age_at_birth",
                 "mother_highest_grade", "father_highest_grade")

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
    "^smoking_ever_" = "smoking_ever",
    "^smoking_30" = "smoking_30",
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

# impute interview age
vars = c("birth_year", "birth_month", "interview_month", "interview_year", "age_interview")
ldat[, (vars) := lapply(.SD, replaceMissing), .SDcol = vars]

ldat[, dob := ymd(paste0(birth_year, "-", birth_month, "/", 15))]
ldat[, mean_interview_month := floor(mean(interview_month, na.rm = TRUE)), time]
ldat[, interview_month := ifelse(is.na(interview_month), mean_interview_month, interview_month)]
ldat[, interview_year := ifelse(is.na(interview_year), year, interview_year)]
ldat[, doi := ymd(paste0(interview_year, "-", interview_month, "-", 15))]
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

# merge all values
loc = readRDS("ch03/output/data/nlsy97_location.rd")
ldat = merge(ldat, loc, by = c("id", "time"), all = TRUE)

# create vector of ids for exploring data
ids = unique(ldat$id)

ldat[, min_age := get_min(age_interview_est), id]
ldat[, min_year := get_min(year), id]
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

# remove redundant rows at age 12
ldat = ldat[!(flag12 == 1 & diff_age_12 == 0)]
# ldat[id == 2413]

baseline_vars = c(paste0("optimism_", 1:4), "age", "residential_moves_by_12",
                  "mother_age_at_birth", "father_highest_grade", "mother_highest_grade",
                  "sex", "race", "hispanic", "ethnicity", "stratum", "type", "wt",
                  "interview_month", "interview_year",
                  "birth_month", "birth_year", "dob", "doi", "mean_interview_month")

ldat[, (baseline_vars) := lapply(.SD, replaceMissing), .SDcol = baseline_vars]
ldat[, (baseline_vars) := lapply(.SD, fillWithFirstValue), id, .SDcol = baseline_vars]

# income adjustments

cpi = fread("ch03/data/cpi.csv")[, cpi := value / 100][, .(year, cpi)]
setnames(cpi, "year", "cpi_year")

ldat[, previous_year := year - 1]
setkey(cpi, cpi_year)
setkey(ldat, previous_year)
ldat = cpi[ldat]

ldat[income %in% c(-1, -2, -3, -4, -5), income := NA]
ldat[, income_adj := ifelse(cpi_year <= 2013, income * cpi, income / cpi)]
ldat[, log_income_adj := ifelse(income_adj < 1, log(1), log(income_adj))]

# compute exposure time
setorder(ldat, id, time)
ldat[, lead_year := shift(year, type = "lead"), id]
ldat[, exposure_time := lead_year - year]

ids = unique(ldat$ids)
ldat[id == sample(ids, 1), .(id, fips, imp_fips, time,
                             nres, diff_age_12,
                             income, income_adj, year, age_interview_est,
                             health)]

# ldat[id == 1]

# exposure?
exposure_periods = ldat[age_interview_est < 20, .(rows = .N, years = sum(exposure_time)), id]
summary(exposure_periods$rows)
summary(exposure_periods$years)

# sex
ldat[, male := ifelse(sex == 1, 1, 0)]

# household size
ldat[hhsize < 0, hhsize := NA]

# impute values

# fips
setorder(ldat, id, time)
ldat[, imp_fips := impute_locf(fips), id]

# get county information
county = data.table(haven::read_dta("ch02/data/cty_full_covariates.dta"))

county_vars = hash(
    "s_rank" = "relative_mob",
    "e_rank_b" = "absolute_mob",
    "gini99"  = "gini"
)

renameColumns(county, county_vars)

county[, (paste0("z_", hash::values(county_vars))) :=
    lapply(.SD, scale), .SDcol = hash::values(county_vars)]

county[, (paste0("q_", hash::values(county_vars))) :=
    lapply(.SD, createQuantiles), .SDcol = hash::values(county_vars)]

county[, mean(relative_mob, na.rm = TRUE), q_relative_mob]
county[, mean(absolute_mob, na.rm = TRUE), q_absolute_mob]

county = county[, .(cty, statename, county_name, gini, z_gini, q_gini, relative_mob, z_relative_mob,
                    q_relative_mob, absolute_mob, z_absolute_mob, q_absolute_mob
                    )]

setnames(county, "cty", "imp_fips")

dim(ldat)
ldat = merge(ldat, county, by = "imp_fips", all.x = TRUE)
dim(ldat)

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

ldat[, ethnicity := factor(ethnicity)]

# smoking
ldat[, smoking_30 := ifelse(smoking_ever == 0 & smoking_30 == -4,
                            0,
                            smoking_30)]

ldat[year %in% c(2013, 2015), smoking_ever := ifelse(smoking_ever == -4, 0, smoking_ever)]
smoking_cols = c("smoking_ever", "smoking_30")
ldat[, (smoking_cols) := lapply(.SD, replaceMissing), .SDcol = smoking_cols]

# bmi
setorder(ldat, id, time)
bmi_cols = c("weight", "height_feet", "height_inches")
ldat[, (bmi_cols) := lapply(.SD, replaceMissing), .SDcol = bmi_cols]

height = c("height_feet", "height_inches")
ldat[age_interview_est >= 25, (height) := lapply(.SD, fillWithLastValue), id, .SDcol = height]

summary(ldat[, ..bmi_cols])

ids = unique(ldat$id)
ldat[id == sample(id, 1), .(id, age_interview_est, year, weight, height_feet, height_inches)]
ldat[, bmi := 703 * weight / (height_inches + height_feet * 12) ^ 2]
summary(ldat[year == 2015, bmi])
ldat[bmi < 10 | bmi > 40, bmi := NA]
# hist(ldat$bmi)

ldat[, age_interview_est2 := age_interview_est ^ 2]

 # multiple imputation
mm = ldat[, .(id, male, ethnicity, age_interview_est, age_interview_est2,
              hhsize, z_relative_mob,
              log_income_adj, parent_education, mother_age_at_birth,
              residential_moves_by_12,
              health, bmi, depression, smoking_ever, smoking_30)]

ini = mice(mm, maxit = 0)
pred = ini$pred
meth = ini$meth
pred[,] = 0

# exploring some models
# lm(bmi ~ male + smoking_30, data = ldat)

# set up methods and prediction matrix
meth[c("hhsize", "z_relative_mob", "log_income_adj",
       "parent_education", "mother_age_at_birth",
       "residential_moves_by_12",
       "health", "bmi", "depression", "smoking_ever", "smoking_30")
      ] = c("2l.pmm", "2l.pmm", "2l.pmm", "2lonly.pmm",
            "2lonly.pmm", "2lonly.pmm",
            "2l.pmm", "2l.pmm", "2l.pmm", "2l.pmm", "2l.pmm")

pred["hhsize",
     c("id", "male", "ethnicity", "age_interview_est", "age_interview_est2",
       "z_relative_mob", "log_income_adj",
       "parent_education", "mother_age_at_birth", "residential_moves_by_12",
       "health", "bmi", "depression", "smoking_ever", "smoking_30")
     ] = c(-2, 1, 1, 1, 1,
           1, 1, 1, 1, 1,
           1, 0, 1, 1, 1)

pred["z_relative_mob",
     c("id", "male", "ethnicity", "age_interview_est", "age_interview_est2",
       "hhsize", "log_income_adj",
       "parent_education", "mother_age_at_birth", "residential_moves_by_12",
       "health", "bmi", "depression", "smoking_ever", "smoking_30")
     ] = c(-2, 1, 1, 1, 1,
           1, 1, 1, 1, 1,
           1, 0, 1, 1, 1)

pred["log_income_adj",
     c("id", "male", "ethnicity", "age_interview_est", "age_interview_est2",
       "hhsize", "z_relative_mob",
       "parent_education", "mother_age_at_birth", "residential_moves_by_12",
       "health", "bmi", "depression", "smoking_ever", "smoking_30")
     ] = c(-2, 1, 1, 1, 1,
           1, 1, 1, 1, 1,
           1, 0, 1, 1, 1)

pred["parent_education",
     c("id", "male", "ethnicity", "age_interview_est", "age_interview_est2",
       "hhsize", "z_relative_mob",
       "log_income_adj", "mother_age_at_birth", "residential_moves_by_12",
       "health", "bmi", "depression", "smoking_ever", "smoking_30")
     ] = c(-2, 1, 1, 1, 1,
           1, 1, 1, 1, 1,
           1, 0, 1, 1, 1)

pred["mother_age_at_birth",
     c("id", "male", "ethnicity", "age_interview_est", "age_interview_est2",
       "hhsize", "z_relative_mob",
       "log_income_adj", "parent_education", "residential_moves_by_12",
       "health", "bmi", "depression", "smoking_ever", "smoking_30")
     ] = c(-2, 1, 1, 1, 1,
           1, 1, 1, 1, 1,
           1, 0, 1, 1, 1)

pred["residential_moves_by_12",
     c("id", "male", "ethnicity", "age_interview_est", "age_interview_est2",
       "hhsize", "z_relative_mob",
       "log_income_adj", "parent_education", "mother_age_at_birth",
       "health", "bmi", "depression", "smoking_ever", "smoking_30")
     ] = c(-2, 1, 1, 1, 1,
          1, 1, 1, 1, 1,
          1, 0, 1, 1, 1)

pred["health",
     c("id", "male", "ethnicity", "age_interview_est", "age_interview_est2",
       "hhsize", "z_relative_mob",
       "log_income_adj", "parent_education", "mother_age_at_birth",
        "residential_moves_by_12",
        "bmi", "depression", "smoking_ever", "smoking_30")
     ] = c(-2, 1, 1, 1, 1,
           1, 1, 1, 1, 1, 1,
           1, 1, 1, 1)

pred["bmi",
     c("id", "male", "ethnicity", "age_interview_est", "age_interview_est2",
       "hhsize", "z_relative_mob",
       "log_income_adj", "parent_education", "mother_age_at_birth",
       "residential_moves_by_12",
        "health", "depression", "smoking_ever", "smoking_30")
     ] = c(-2, 1, 1, 1, 1,
           1, 1, 1, 1, 1, 1,
           1, 1, 1, 1)

pred["depression",
     c("id", "male", "ethnicity", "age_interview_est", "age_interview_est2",
       "hhsize", "z_relative_mob",
       "log_income_adj", "parent_education", "mother_age_at_birth",
       "residential_moves_by_12",
        "bmi", "health", "smoking_ever", "smoking_30")
     ] = c(-2, 1, 1, 1, 1,
           1, 1, 1, 1, 1, 1,
           1, 1, 1, 1)

pred["smoking_ever",
     c("id", "male", "ethnicity", "age_interview_est", "age_interview_est2",
       "hhsize", "z_relative_mob",
       "log_income_adj", "parent_education", "mother_age_at_birth",
       "residential_moves_by_12",
        "bmi", "health", "depression", "smoking_30")
     ] = c(-2, 1, 1, 1, 1,
           1, 1, 1, 1, 1, 1,
           1, 1, 1, 1)

pred["smoking_30",
     c("id", "male", "ethnicity", "age_interview_est", "age_interview_est2",
       "hhsize", "z_relative_mob",
       "log_income_adj", "parent_education", "mother_age_at_birth",
       "residential_moves_by_12",
        "bmi", "health", "depression", "smoking_ever")
     ] = c(-2, 1, 1, 1, 1,
           1, 1, 1, 1, 1, 1,
           1, 1, 1, 1)

pred["health",]
pred["smoking_30",]
pred["bmi",]

imp = mice::mice(mm, predictorMatrix = pred, method = meth,
           m = 5, maxit = 5)

# explore first imputation
# test = data.table(complete(imp, 1))
# imp_ids = unique(test$id)

# test[id == sample(imp_ids, 1)]

# test[id == 7310]



# filter dropout
# setorder(ldat, id, -year)
# ldat[, cumresponses := cumsum(response), id]
# ldat[, maxresponses := max(cumresponses), id]
# ldat[, dropout := ifelse(cumresponses == 0, 1, 0)]
# ldat[, dropout := max(dropout), id]
# # ldat = ldat[cumresponses > 0]
# setorder(ldat, id, year)


# testdat = ldat[complete.cases(ldat[, .(id, imp_s_rank, imp_race, imp_sex, imp_health, imp_income)])]

# # create lag varible
# # setorder(testdat, id, time)
# # testdat[, lag_imp_s_rank := shift(imp_s_rank), id]
# # testdat[, lag_imp_health := shift(imp_health), id]
# # testdat[, lag_imp_income := shift(imp_income), id]

# fdata = testdat
# # test = data.table(campaign_long)
# # table(test$week)

# variables = c("id", "imp_s_rank", "imp_sex", "imp_income", "imp_race")
# fdata = testdat[complete.cases(testdat[, ..variables])]

# temp = ipwtm(exposure = imp_s_rank,
#              family = "gaussian",
#              numerator = ~ as.factor(imp_sex) + as.factor(imp_race),
#              denominator = ~ as.factor(imp_sex) + as.factor(imp_race) + imp_income + time,
#              timevar = time,
#              type = "all",
#              corstr = "ar1",
#              id = id,
#              data = fdata)

# fdata[, ipw := temp$ipw.weights]
# exposure = fdata[time < 9, .(avg_s_rank = mean(imp_s_rank, na.rm = TRUE)), id]
# fdata = merge(fdata, exposure, by = "id")
# fdata

# last_obs = fdata[, .SD[.N], by=id]
# last_obs

# ids = unique(last_obs$id)
# last_obs[id == sample(ids, 1), .(id, year, imp_s_rank, avg_s_rank, time, ipw)]

# m0 = lm(imp_health ~  avg_s_rank + as.factor(imp_sex) + as.factor(imp_race),
#         weights = last_obs$ipw, data = last_obs)
# summary(m0)


# m0 = lm(imp_health ~ avg_s_rank, data = last_obs)
# summary(m0)

# require(MASS)
# require(Hmisc)

# last_obs = last_obs[imp_health > 0][, imp_health := factor(imp_health)]
# m <- polr(imp_health ~ avg_s_rank, weights = last_obs$ipw, data = last_obs, Hess=TRUE)
# summary(m)

# –

# length(temp$ipw.weights)
# finalw = testdat[, .SD[.N], id]
# # recode some variables
# table(ldat$health)
# ldat[, health := ifelse(health < 0, NA, health)]
# table(ldat$health)

# ldat[, male := ifelse(sex == 1, 1, 0)]
# table(ldat$male)
# ldat

# # create age variable
# setkey(ldat, id, year)
# ldat[, s := 1:.N, id]
# ldat[, agei := age + s - 1]
# ldat[id == 10, .(id, year, age, agei)]

# countmis(ldat) # 14%

# add county data

# getting income mobility data
# get counties from chetty data and fix differences


# saveRDS(ldat, 'ch03/output/data/nlsy97_analytic.rd')

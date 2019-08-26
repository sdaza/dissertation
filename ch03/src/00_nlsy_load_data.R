##############################
# load individual NLSY97 data
# author: sebastian daza
# version: 0.01
##############################


# libraries
library(data.table)
library(sdazar)
library(lme4)
library(texreg)
library(ggplot2)

# load data

load("/Users/sdaza/Google Drive/00Dissertation/Chapters/ch03/output/rdata/nlsy97/nsly97selection.Rdata")

years <- c(1997:2011, 2013)

# id
dat
setnames(dat, "r0000100", "id")
anyDuplicated(dat[, id])

# demographics
ovars <- c("r0536300", "r0536700", "r0538700")
nvars <- c("sex", "age", "race")
setnames(dat, ovars, nvars)

table(dat$sex, useNA = "ifany")
table(dat$age, useNA = "ifany") # from 12 to 16
dat[, race := ifelse(race < 0, NA, race)]
table(dat$race, useNA = "ifany") # 80 missing cases

# survey design

ovars <- c("r1217500","r1235800","r1236101","r1489700","r1489800")
nvars <- c("urban", "type", "wt", "stratum", "cluster")
setnames(dat, ovars, nvars)

table(dat$urban, useNA = "ifany")
table(dat$type, useNA = "ifany")
summary(dat$wt)
summary(dat$stratum)
summary(dat$cluster) # only two

#############################
# outcomes
#############################


# subjective health

ovars <- c("r0320600","r2164000","r3481900","r4880100","r6497500","s1225000",
           "s3302500","s4919500","s6661100","s8644200","t1049500","t3144600",
           "t4562200","t6206400","t7703800","t9093100")


nvars <- paste0("shealth", years)

setnames(dat, ovars, nvars)

table(dat$shealth2007, useNA = "ifany")

# depression

# smoking

# alcohol

# drugs

# bmi


##############################
# covariates
##############################

# income
ovars <- c("r1204500","r2563300","r3884900","r5464100","r7227800","s1541700",
           "s2011500","s3812400","s5412800","s7513700","t0014100","t2016200",
           "t3606500","t5206900","t6656700","t8129100")

nvars  <- paste0("income", years)
setnames(dat, ovars, nvars)

summary(dat$income1997)
summary(dat$income2000)
summary(dat$income2013) # to adjust or not by deaths

#############################
# transform to long format
#############################

vars <- lookvar(dat, "id|type|urbanrace|income|sex|age|shealth|stratum|cluster|wt")

sdat <- dat[, vars, with = FALSE]

patt <- c("^shealth", "^income")
vnames <- c("health", "income")


ldat <- melt(sdat, measure.vars = patterns(patt),
      value.name = vnames,
      variable.name = "time")

for (i in seq_along(years)) {
ldat[time == i, year := years[i]]
}

ldat[, time := NULL]

# load location data
load("/Users/sdaza/Google Drive/00Dissertation/Chapters/ch03/output/rdata/nlsy97/location_nlsy97.Rdata")
ls()

setkey(loc, id, year)
setkey(ldat, id, year)

dim(ldat)
ldat <- loc[ldat]
dim(ldat)

# recode some variables
table(ldat$health, useNA = "ifany")
ldat[, health := ifelse(health < 0, NA, health)]
table(ldat$health, useNA = "ifany")

ldat[, male := ifelse(sex == 1, 1, 0)]
table(ldat$male, useNA = "ifany")
ldat

# create age variable
setkey(ldat, id, year)
ldat[, s := 1:.N, id]
ldat[, agei := age + s - 1]
ldat[id == 10, .(id, year, age, agei)]

countmis(ldat) # 14%

summary(ldat$s_rank)
x <- ldat[agei < 20, .(moves = max(moves, na.rm = TRUE)), id]
prop.table(table(x$moves > 0)) # 13%

hist(ldat[, agei])
hist(ldat[year == 2013, agei])
summary(ldat[year == 2013, agei]) # 27-31 years

# some plots
ggplot(ldat, aes(y = health, x = s_rank)) + geom_jitter()

# define simple model to explore (ordinal model?)
m1 <- lm(health ~ male + age + s_rank, data = ldat[s == 1]) #
summary(m1)

me1 <- lmer(health ~ male + as.factor(age) + gini99 + (1 | id), data = ldat)
screenreg(me1)


# number of days smoking

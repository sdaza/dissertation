
# read NATS data and estimate initiation rates
options(scipen=999)
library(haven)
library(data.table)
library(survival)
library(muhaz)
library(xlsx)
table = function (...) base::table(..., useNA = 'ifany')

# read date
dat = data.table(read_sas("data/nats_2013.sas7bdat"))
setnames(dat, "wt_national", "wt")
summary(dat$wt)

dat[, flag := ifelse(smok100 == 1 | 
    (age %in% 18:29 & smok100 %in% c(2, -8, -7) & smoknow %in% 1:2) | smokever == 1, 1, 0)]

# skip pattern for age of initiation
# /ASK IF Q3 SMOK100 EQ
# 1 OR (Q2 AGE EQ (18-29)
# AND Q3 SMOK100 EQ
# (2,-8,-7) AND Q4
# SMOKNOW EQ (1,2)) OR
# Q13 SMOKEVER EQ 1/ 

# only valid age
dat = dat[age > 0]
summary(dat$age)
nrow(dat)
dat[smokfirstage %in% c(-8, -7), smokfirstage := NA]
dat[, init := smokfirstage]
dat[, init := ifelse(init == -1, age, smokfirstage)]
dat[, smoking := ifelse(smokfirstage == -1, 0, 1)]

names(dat)
ss = Surv(dat$init, dat$smoking)
m = survfit(ss ~ 1, data = dat)
m = summary(m)
m$n.event / m$n.risk

table(dat$income2)
dat[, incometype := NULL]
dat[income2 %in% 1:2, incometype := 1]
dat[income2 %in% 3:5, incometype := 2]
dat[income2 %in% 6:8, incometype := 3]
table(dat$incometype)

dat[, incometype := factor(incometype, levels = c(2, 1, 3))]
res_cox = coxph(Surv(init, smoking) ~ incometype, data = dat)
res_cox

sdat = dat[!is.na(init), .(init, smoking, incometype, wt = round(wt))]
sdat = sdat[rep(seq(.N), wt), !"wt"]

# expand data
mh = muhaz(dat$init, dat$smoking)
summary(mh)
plot(mh, xlab = "Age", ylab = "Smoking initiation hazard rate")
rates = data.table(age = 1:length(mh$haz.est), rate = mh$haz.est)
rates =  rates[age > 64, rate := 0][age <= 65]
write.xlsx(rates, "models/MobHealthRecycling/data/smoking-rates.xlsx", row.names = FALSE)

# income category and risk of smoking 
sdat[, incometype := factor(incometype, levels = c(2, 1,3))]



# difference between highest and lowes income
log((22.3+18.8)/(7.1+21.4))

current_smokers = c(22.3, 11.3, 17.8, 14.0, 13.2, 7.1)
former_smokers = c(18.8, 22.0, 21.4, 22.1, 23.8, 21.4)

smokers = current_smokers + former_smokers
smokers
avg_middle_group = mean(smokers[2:5])
avg_middle_group

log(smokers[1]/avg_middle_group)
log(smokers[6]/avg_middle_group)
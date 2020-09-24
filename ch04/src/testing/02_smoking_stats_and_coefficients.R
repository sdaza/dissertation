
# read NATS data and estimate initiation rates

options(scipen = 999)
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

setnames(dat, c("SMOKEVER_R2", "SMOKESTATUS3_R2"), c("sever", "scurrent"))
table(dat$sever)
table(dat$scurrent)

dat[sever == 3, sever := NA]
dat[scurrent == 7, scurrent:= NA]
dat[, sever := ifelse(sever == 1, 1, 0)]
dat[, scurrent:= ifelse(scurrent == 1, 1, 0)]

table(dat$sever)
table(dat$scurrent)
# proportion of ever smokers and current smokers
dat[, weighted.mean(sever, wt, na.rm = TRUE)]
dat[, weighted.mean(scurrent, wt, na.rm = TRUE)]
dat[age %in% 30:40, weighted.mean(sever, wt, na.rm = TRUE)]
dat[age %in% 30:40, weighted.mean(scurrent, wt, na.rm = TRUE)]

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

# income categories
table(dat$income2)
dat[, incometype := NULL]
dat[income2 %in% 1:2, incometype := 1]
dat[income2 %in% 3:5, incometype := 2]
dat[income2 %in% 6:8, incometype := 3]
table(dat$incometype)

dat[, .(sever = weighted.mean(sever, wt, na.rm = TRUE),
    weighted.mean(scurrent, wt, na.rm = TRUE)), incometype]

dat[, incometype := factor(incometype, levels = c(2, 1, 3))]

# logistic model
m1 = glm(scurrent ~ incometype, data = dat)
summary(m1)
res_cox = coxph(Surv(init, smoking) ~ incometype, data = dat)

sdat = dat[!is.na(init), .(init, smoking, incometype, wt = round(wt))]
sdat = sdat[rep(seq(.N), wt), !"wt"]

# expand data
mh = muhaz(sdat$init, sdat$smoking)
summary(mh)
plot(mh, xlab = "Age", ylab = "Smoking initiation hazard rate")
rates = data.table(age = 1:length(mh$haz.est), rate = mh$haz.est)
rates =  rates[age > 64, rate := 0][age <= 65]
write.xlsx(rates, "models/MobHealthRecycling/data/smoking-rates.xlsx", row.names = FALSE)



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
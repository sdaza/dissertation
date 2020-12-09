

library(data.table)
library(metafor)
library(texreg)
library(survival)


source("src/utils.R")
path = "models/MobHealthRecycling/output/verification/exogenous-experiment"
m = fread(paste0(path, "/mortality.csv"))
m = m[iteration %in% 1:5]
m = m[age > 18]
mean(m$age)


# pretty high
names(m)
table(m$iteration)
# only 50%
cor(m[iteration %in% 10:12, .(county_rank_slope, total_rank_slope_exposure, total_rank_correlation_exposure)])
cor(m$total_rank_correlation_exposure, m$income)

m[, status := 1]
screenreg(lm(age ~ total_rank_slope_exposure, data = m[age > 0]))
screenreg(coxph(Surv(age, status) ~ total_rank_slope_exposure, data = m[age > 0]))

m[, imc := cut(total_rank_slope_exposure, breaks=c(quantile(total_rank_slope_exposure,
        probs = seq(0, 1, 1/5))),include.lowest = TRUE, labels = FALSE),]

table(m$imc, m$age0)
quantile(m$total_rank_slope_exposure)

names(m)
cor(m[, .(total_slope_correlation_exposure, total_rank_correlation_exposure)])
# correlation total exposure and current county
cor(m[, .(income_type, total_rank_correlation_exposure)])
cor(m[, .(income_type, county_rank_slope, total_rank_correlation_exposure, income, county_mean_income)])


# exposure
table(m[age0 ==1, total_rank_slope_exposure])
mean(m$total_rank_slope_exposure)
hist(m[total_rank_slope_exposure < 0.0334, age])
hist(m[total_rank_correlation_exposure , age])

hist(m[total_rank_correlation_exposure > 0 & total_rank_correlation_exposure < 0.20, age])

table(m[, .(age == 0, total_rank_correlation_exposure < .20)])
table(m[, .(age == 0, total_rank_correlation_exposure < 0)])



m = m[age != 0]

dim(m[total_rank_correlation_exposure > .20,])

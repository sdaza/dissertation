###############################################
# county income mobility and individual health
# simulation example for average MSM effects
# author: sebastian daza
###############################################


library(data.table)
library(survey)
source("ch03/src/utils.R")

#set seed to replicate results
set.seed(12345)

# continous example

#define sample size
n = 2000

#define confounder c
c = rnorm(n,0,1)

#define treatment at time 1 as function of confounder
t1 = .1 * c + rnorm(n,0, sqrt(.99))

#define depression at time 1 as function of confounder and treat1
d1 = .1 * c  + .4 * t1 +  rnorm(n,0, sqrt(.822))

#define treatment at time 2 as function of confounder and dep1
t2 = .1*c + .4 * d1 + .4 * t1 + rnorm(n,0, sqrt(.5196))

#define outcome depression at time 2 as function of confounder, treat1, and dep1
d2 = .1 * c + .4 * t2 + .4 * d1 + rnorm(n,0, sqrt(.4582))

# add ID variable to do mixed effects models later
id = rep(1: length(c))

# data.table
dt = data.table(id, c, t1, d1, t2, d2)

# weights at time 1
w1 = dnorm(dt$t1, predict(lm(t1~1)),
            sd(lm(t1~ 1)$residuals)) / dnorm(dt$t1, predict(lm(t1 ~ c)),
                                             sd(lm(t1~ c)$residuals))

# weights at time 2
w2 = dnorm(dt$t2, predict(lm(t2~t1)),
            sd(lm(t2~t1)$residuals)) / dnorm(dt$t2, predict(lm(t2~c+d1+t1)),
                                             sd(lm(t2~c+d1+t1)$residuals))

wt = w1 * w2

c(mean(wt), min(wt), max(wt))

dt[, average_treatment := apply(.SD, 1, mean), .SDcols = c("t1", "t2")]
dt[, wt := wt]

summary(lm(d2 ~ average_treatment, data = dt))
summary(lm(d2 ~ average_treatment + c, data = dt))

summary(svyglm(d2 ~ average_treatment + c,
               design = svydesign(~ 1, weights = ~ wt, data = dt)))

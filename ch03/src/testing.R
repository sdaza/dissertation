

# residual balance ipw
library(rbw)
library(data.table)
library(survey)

data(campaign_long)


data = data.table(campaign_long)

test_wide = data.table(campaign_wide)

test_wide
data

table(data$d.gone.neg)


m1 <- lm(dem.polls ~ (d.gone.neg.l1 + dem.polls.l1 + undother.l1) *  factor(week), data = campaign_long)

summary(m1)
m2 <- lm(undother ~ (d.gone.neg.l1 + dem.polls.l1 + undother.l1) * factor(week), data = campaign_long)
xmodels <- list(m1, m2)
summary(m1)

(data)
setorder(data, id, week)

ids = unique(data$id)
head(data[id == sample(ids, 1),
    .(id, week, dem.polls, undother, d.gone.neg.l1, neg.rep, d.gone.neg, dem.polls.l1, undother.l1)])

table(data$office)

# residual balancing weights
fit <- rbwPanel(exposure = d.gone.neg,
                xmodels = xmodels, id = id,
                time = week,
                data = campaign_long,
                max_iter = 500,
                tol = 0.001)

hist(fit$weights$rbw)

campaign_wide <- merge(campaign_wide, fit$weights, by = "id")

hist(fit$weights)

# fitting a marginal structural model
rbw_design <- svydesign(ids = ~ 1,
                        weights = ~ rbw,
                        data = campaign_wide)

msm_rbw <- svyglm(demprcnt ~ cum_neg * deminc + camp.length + factor(year) + office,
                  design = rbw_design)

summary(msm_rbw)

# library CBPS

library(CBPS)
data(Blackwell)

## Quickly fit a short model to test
form0 <- "d.gone.neg ~ d.gone.neg.l1 + camp.length"

fit0 <- CBMSM(formula = form0, time = Blackwell$time, id = Blackwell$demName,
            data = Blackwell, type="MSM",  iterations = NULL, twostep = TRUE,
            msm.variance = "approx", time.vary = TRUE)


# Fitting the models in Imai and Ratkovic (2014)
# Warning: may take a few mintues; setting time.vary to FALSE
# Results in a quicker fit but with poorer balance
# Usually, it is best to use time.vary TRUE
form1 <- "d.gone.neg ~ d.gone.neg.l1 + d.gone.neg.l2 + d.neg.frac.l3 +
         camp.length + deminc + base.poll + year.2002 +
          year.2004 + year.2006 + base.und + office"

## Note that  init="glm" gives the published results but the default is now init="opt"
fit1 <- CBMSM(formula = form1, time=Blackwell$time,id=Blackwell$demName,
            data=Blackwell, type="MSM",  iterations = NULL, twostep = TRUE,
            msm.variance = "full", time.vary = TRUE, init = "glm")

fit2 <- CBMSM(formula = form1, time=Blackwell$time,id=Blackwell$demName,
            data=Blackwell, type="MSM",  iterations = NULL, twostep = TRUE,
            msm.variance = "approx", time.vary = TRUE, init="glm")


## Assessing balance
bal1 <- balance.CBMSM(fit1)
bal2 <- balance.CBMSM(fit2)

length(fit2$weights)
##Effect estimation: Replicating Effect Estimates in
##Table 3 of Imai and Ratkovic (2014)

lm1 <-lm(demprcnt[time==1]~fit1$treat.hist,data=Blackwell,
         weights=fit1$glm.weights)
lm2 <- lm(demprcnt[time==1]~fit1$treat.hist,data=Blackwell,
          weights=fit1$weights)
lm3<-lm(demprcnt[time==1]~fit1$treat.hist,data=Blackwell,
        weights=fit2$weights)

lm4<-lm(demprcnt[time==1]~fit1$treat.cum,data=Blackwell,
weights=fit1$glm.weights)

lm5<-lm(demprcnt[time==1]~fit1$treat.cum,data=Blackwell,
weights=fit1$weights)

lm6<-lm(demprcnt[time==1]~fit1$treat.cum,data=Blackwell,
weights=fit$weights$rbw)


### Example: Multiple Binary Treatments Administered at the Same Time
n<-200
k<-4
set.seed(1040)
X1<-cbind(1,matrix(rnorm(n*k),ncol=k))

betas.1<-betas.2<-betas.3<-c(2,4,4,-4,3)/5
probs.1<-probs.2<-probs.3<-(1+exp(-X1 %*% betas.1))^-1

treat.1<-rbinom(n=length(probs.1),size=1,probs.1)
treat.2<-rbinom(n=length(probs.2),size=1,probs.2)
treat.3<-rbinom(n=length(probs.3),size=1,probs.3)
treat<-c(treat.1,treat.2,treat.3)
X<-rbind(X1,X1,X1)
time<-c(rep(1,nrow(X1)),rep(2,nrow(X1)),rep(3,nrow(X1)))
id<-c(rep(1:nrow(X1),3))
y<-cbind(treat.1,treat.2,treat.3) %*% c(2,2,2) +
X1 %*% c(-2,8,7,6,2) + rnorm(n,sd=5)

multibin1<-CBMSM(treat~X,id=id,time=time,type="MultiBin",twostep=TRUE)
summary(lm(y~-1+treat.1+treat.2+treat.3+X1, weights=multibin1$w))


library(data.table)

dt1 <- data.table(x = c("a", "b", "c", "d"), dt1_y = c(11.9, 21.4, 5.7, 18))
dt2 <- data.table(dt2_y = c(10, 15, 20), z = c("one", "two", "three"))

# add row ids and duplicate y
dt1[, `:=` (dt1_row_id = .I, joint_y = dt1_y)]
dt2[, `:=` (dt2_row_id = .I, joint_y = dt2_y)]

dt1
dt2

dt2[dt1, on = .(joint_y), roll = "nearest"]
dt2[dt1, on = .(joint_y), roll = T]
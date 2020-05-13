library(ordinal)

data(soup)

## More manageable data set:
(tab26 <- with(soup, table("Product" = PROD, "Response" = SURENESS)))

dimnames(tab26)[[2]] <- c("Sure", "Not Sure", "Guess", "Guess", "Not Sure", "Sure")
dat26 <- expand.grid(sureness = as.factor(1:6), prod = c("Ref", "Test"))
dat26$wghts <- c(t(tab26))

m1 <- clm(sureness ~ prod, scale = ~prod, data = dat26,
          weights = wghts, link = "logit")

dat26
predict(m1)


# survey data negative binomial

library(survey)
library(sjstats)

data(nhanes_sample)
# create survey design
des <- svydesign(
id = ~SDMVPSU,
strat = ~SDMVSTRA,
weights = ~WTINT2YR,
nest = TRUE,
data = nhanes_sample
)
# fit negative binomial regression
fit <- svyglm.nb(total ~ factor(RIAGENDR) * (log(age) + factor(RIDRETH1)), des)
vcov(fit)
coef(fit)
# print coefficients and standard errors
fit

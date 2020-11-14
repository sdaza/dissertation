##############################
# generative model income mobility and mortality
# verify income and county stats
# author: sebastian daza
##############################


library(data.table)
library(haven)
library(ggplot2)
source("src/utils.R")
path = "models/MobHealthRecycling/output/income/"


covs = data.table(haven::read_dta('data/cty_full_covariates.dta'))

dim(covs)

covs[, relative_income_mob := s_rank / 100]

summary(covs$e_rank_b)


summary(covs$s_rank)

summary(covs$relative_income_mob)
sd(covs$relative_income_mob, na.rm = TRUE)
summary(covs$hhinc00)
covs[, income := log(hhinc00)]

mean(covs$hhinc00)
sd(covs$hhinc00)
summary(covs$hhinc00)

plot(covs$income, covs$relative_income_mob)
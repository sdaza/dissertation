##############################
# generative model income mobility and mortality
# verify income generation
# author: sebastian daza
##############################


library(data.table)
library(haven)

# read chetty's data
covs = data.table(haven::read_dta('data/cty_full_covariates.dta'))

summary(covs$e_rank_b)

summary(covs$s_rank)
covs[, relative_income_mob := s_rank / 100]
hist(covs$relative_income_mob)
hist(covs$gini99)

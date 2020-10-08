##############################
# generative model income mobility and mortality
# verify income generation and mobility
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


# check some anylogic output
data = fread('family.csv')


prop.table(table(data[generation==1, .(parent_type, kid_type)]), 1)
table(data[generation==1, parent_type])

# explore income exposure variable
setorder(data, -kid_income_exposure)
head(data)

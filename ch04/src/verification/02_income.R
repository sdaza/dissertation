##############################
# generative model income mobility and mortality
# verify income generation and mobility
# author: sebastian daza
##############################


library(data.table)
library(haven)
library(ggplot2)
source("src/utils.R")
path = "models/MobHealthRecycling/output/verification/income/"

# read files
p = readMultipleFiles("parameters", path, remove_files = TRUE)
e = readMultipleFiles("environment", path, remove_files = TRUE)

m = readMultipleFiles("mortality", path, remove_files = TRUE)

names(p)

parameters = c("complete_income_mob", "endogenous_income_generation", 
    "base_prob_same_income", "weight_income_exp")
setorderv(p, parameters)

dim(p)

p[, niteration := .GRP, by = parameters]
p[, nreplicate := 1:.N, by = niteration]
np = p[, c("iteration", "replicate", "niteration", "nreplicate", parameters), with = FALSE]

unique(np[, c("niteration", parameters), with = FALSE])

table(np$niteration)
# table(np$nreplicate)

e = merge(e, np, by = c("iteration", "replicate"))
m = merge(m, np, by = c("iteration", "replicate"))

names(m)
names(e)
# transition matrices
names(e)
summary(e[niteration == 1, rank_slope])
summary(e[niteration == 1, county_rank_slope_avg])
summary(e[niteration == 1, county_rank_slope_sd])

summary(e[niteration == 2, rank_slope])
summary(e[niteration == 2, county_rank_slope_avg])
summary(e[niteration == 2, county_rank_slope_sd])

summary(e[niteration == 3, rank_slope])
summary(e[niteration == 3, county_rank_slope_avg])
summary(e[niteration == 3, county_rank_slope_sd])

hist(e[niteration == 2, county_rank_slope_avg])
hist(e[niteration == 3, county_rank_slope_avg])

# compute transition matrices
tm = list()
for (i in unique(np$niteration)) {
    temp = copy(m[niteration == i])
    mat = as.matrix(
        prop.table(table(temp[, .(parent_income_type, income_type)]), 1)
    )
    tm[[i]] = mat
}

tm


x = xtable(mat, align=rep("", ncol(mat)+1))
print(x, floating=FALSE, tabular.environment="bmatrix", 
  hline.after=NULL, include.rownames=FALSE, include.colnames=FALSE)


mat = xtable(a,align = rep("", ncol(a)+1)) 
print(mat, floating = FALSE, tabular.environment = "bmatrix", hline.after = NULL, 
    include.rownames = FALSE, include.colnames = FALSE)


table(county$replicate)
summary(county$county)

county[, .(avg_income = mean(avg_income), gini = mean(gini),
    income_mobility = mean(income_mobility), population = mean(population)),
    .(county, iteration)]


# read chetty's data
covs = data.table(haven::read_dta('data/cty_full_covariates.dta'))

covs[, relative_income_mob := s_rank / 100]

summary(covs$e_rank_b)
summary(covs$s_rank)

summary(covs$relative_income_mob)
sd(covs$relative_income_mob, na.rm = TRUE)
summary(covs$hhinc00)
covs[, income := log(hhinc00)]

plot(covs$income, covs$relative_income_mob)

cor(covs[, .(income, relative_income_mob)])

hist(covs$relative_income_mob)
hist(covs$gini99)


# check some anylogic output
data = fread('family.csv')


prop.table(table(data[generation==1, .(parent_type, kid_type)]), 1)
table(data[generation==1, parent_type])

# explore income exposure variable
setorder(data, -kid_income_exposure)
head(data)

###############################
# income mobility and health
# testing measures
# author: sebastian daza
################################

# libraries
library(data.table)

# functions
table = function (...) base::table(..., useNA = 'ifany')
cor = function(...) stats::cor(..., use = "complete.obs")
perc.rank = function(x) trunc(rank(x))/length(x)

sample_based_on_type = function(type) {
  type_vector = c('type1', 'type2', 'type3')
  type_probs = list()
  type_probs[['type1']] = c(0.6, 0.2, 0.2)
  type_probs[['type2']] = c(0.2, 0.6, 0.2)
  type_probs[['type3']] = c(0.2, 0.2, 0.6)
  output = NULL
  for (i in type) {
    output = c(output, sample(type_vector, 1, prob = type_probs[[i]]))
  }
  return(output)
}

assign_income_based_on_type = function(type) {
  output = NULL
  for (i in type) {
    type_income = list()

    type_income[['type1']] = runif(1, 0, 10000)
    type_income[['type2']] = runif(1, 25000, 45000)
    type_income[['type3']] = runif(1, 90000, 110000)

    output = c(output, type_income[[i]])
  }
  return(output)
}

# simulate
nsim = 700 * 3
type_1_parent = runif(nsim, 0, 15000)
type_2_parent = runif(nsim, 25000, 45000)
type_3_parent = runif(nsim, 90000, 110000)

dd = data.table(income_parent = c(type_1_parent, type_2_parent, type_3_parent),
           type_parent = c(rep('type1', nsim), rep('type2', nsim), rep('type3', nsim)))

sample_based_on_type('type3')
assign_income_based_on_type('type3')


dd[, type_kid := sample_based_on_type(type_parent)]
dd[, income_kid := assign_income_based_on_type(type_kid)]

prop.table(table(dd$type_parent, dd$type_kid), 1)
prop.table(table(dd$type_parent, dd$type_kid))

# get coefficients from a multinnomial
m = multinom(type_kid ~ type_parent, data = dd)
summary(m)

pp = data.table(fitted(m))
unique(pp)

cf = coef(m)
cf

cf[1, 1]
cf[1, 2] + cf[1,3]

exp(0)  + exp(1.1) + exp(-1.3)

exp(0 * 1.1 * -1.3)

# compute probabilities 
den1 = 1 + exp(cf[1,1]) + exp(cf[2,1])
den1
p1 = 1 / den1
p2 = exp(cf[1,1]) / den1
p3 = exp(cf[2,1]) / den1
print(c(p1, p2, p3))
sum(p1, p2, p3)

den2 = 1 + exp(cf[1,1] + cf[1,2]) + exp(cf[2,1] + cf[2,2])
p1 = 1 / den2
p2 =  exp(cf[1,1] + cf[1,2]) / den2
p3 = exp(cf[2,1]) + cf[2,2]) / den2
print(c(p1, p2, p3))
sum(p1, p2, p3)

den3 = (1 + exp(cf[1,1] + cf[1,3]) + exp(cf[2,1] + cf[2,3]))
p1 = 1 / den3
p2 = exp(cf[1,1] + cf[1,3]) / den3
p3 = exp(cf[2,1] + cf[2,3]) / den3
print(c(p1, p2, p3))
sum(p1, p2, p3)


# testing rank-rank
cor(dd[, .(income_parent, income_kid)], method = 'spearman')

# test 1
dd[, rank_kid := rank(income_kid)]
dd[, rank_parent := rank(income_parent)]
cor(dd[, .(rank_parent, rank_kid)])

# test 2
dd[, prank_kid := perc.rank(income_kid)]
dd[, prank_parent := perc.rank(income_parent)]
cor(dd[, .(prank_parent, prank_kid)])

# check some anylogic output
data = fread('family.csv')

head(data)

prop.table(table(data[generation==1, .(parent_type, kid_type)]), 1)
table(data[generation==1, parent_type])

# explore income exposure variable
setorder(data, -kid_income_exposure)
head(data)


# explore chetty's data
covs = data.table(haven::read_dta('data/cty_full_covariates.dta'))

dim(covs)
head(covs)

covs[, relative_mob := s_rank / 100.0]
summary(covs$relative_mob)
hist(covs$relative_mob)
hist(covs$gini99)
names(covs)
hist(covs$cs00_seg_inc_aff75)

plot(covs$relative_mob, covs$gini99)

cor(covs[, .(relative_mob, gini99)])
cor(covs[, .(relative_mob, cs_race_theil_2000)])


# simulating multinomial data
library(fastDummies)

set.seed(45262)

reps <- 1000 # Set the number of repetitions at the top of the script
par.est.mnl <- matrix(NA, nrow = reps, ncol = 4) # Empty matrix to store

n = 3000
parent_type = factor(c(rep('A', n/3), rep('B', n/3), rep('C', n/3)))
income = c(runif(n/3, 25000, 45000),
           runif(n/3, 0, 10000),
           runif(n/3, 90000, 110000))

sincome = scale(income)
hist(sincome)

dummy_parent_type = dummy_cols(parent_type)
head(dummy_parent_type)

names(dummy_parent_type) = c('parent_type', 'a', 'b', 'c')
dummy_parent_type = dummy_parent_type[, c(3,4)]


b0B = -1.103402
b1B = 2.23886
b2B = 1.120015
b3B = 1.5
b0C = -1.082034
b1C =  1.103802
b2C = 2.183039
b3C = 1.5

# Xc = factor(sample(c("A", "B", "C"), n, prob = rep(1/3, 3), replace = TRUE))
# Xc = dummy_cols(Xc)[, c(2:4)]
# names(Xc) = c("A", "B", "C")
# head(Xc)

# Compute the probabilities of each outcome based on the DGP
expA = exp(0)
expB = exp(b0B + b1B*dummy_parent_type$b + b2B*dummy_parent_type$c + b3B*sincome)
expC = exp(b0C + b1C*dummy_parent_type$b + b2C*dummy_parent_type$c + b3C*sincome)

denominator =  expA + expB + expC
summary(denominator)

pB =  expB / denominator
pC =  expC / denominator
pA = 1 - pB - pC

mean(pA)
mean(pB)
mean(pC)


for (i in  1:reps) {
    Y = rep(NA, n)
    for (j in 1:n) {
      Y[j] = sample(c("A", "B", "C"), 1, replace = TRUE, prob = c(pA[j], pB[j], pC[j]))
    }
}

prop.table(table(parent_type, Y), 1)

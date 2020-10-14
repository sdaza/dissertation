###############################
# income mobility and health
# get coefficients for income assignment
# author: sebastian daza
################################

# libraries
library(data.table)
library(nnet)
library(haven)


# functions
table = function (...) base::table(..., useNA = 'ifany')
cor = function(...) stats::cor(..., use = "complete.obs")
perc.rank = function(x) trunc(rank(x))/length(x)

sample_based_on_type = function(type, hprob = 0.50) {

    type_probs = list(
    type1 = c(hprob, (1-hprob)/2, (1-hprob)/2),
    type2 = c((1-hprob)/2, hprob, (1-hprob)/2),
    type3 = c((1-hprob)/2, (1-hprob)/2, hprob))

    type_vector = c('type1', 'type2', 'type3')
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
        type_income[['type1']] = runif(1, 0, 20000)
        type_income[['type2']] = runif(1, 25000, 45000)
        type_income[['type3']] = runif(1, 90000, 110000)
    output = c(output, type_income[[i]])
    }
    return(output)
}

# 54%
nsim = 700 * 10
type_1_parent = runif(nsim, 0, 20000)
type_2_parent = runif(nsim, 25000, 45000)
type_3_parent = runif(nsim, 90000, 110000)

dd = data.table(income_parent = c(type_1_parent, type_2_parent, type_3_parent),
           type_parent = c(rep('type1', nsim), rep('type2', nsim), rep('type3', nsim)))

dd[type_parent == "type1", type_kid := sample_based_on_type(type_parent, .90)]
dd[type_parent == "type2", type_kid := sample_based_on_type(type_parent, .50)]
dd[type_parent == "type3", type_kid := sample_based_on_type(type_parent, .50)]
dd[, income_kid := assign_income_based_on_type(type_kid)]
dd[, upward := income_kid > income_parent]

table(dd[type_parent == "type3" & type_kid == "type3", upward])

hist(dd[type_parent == "type3" & type_kid == "type3"]$income_parent)
hist(dd[type_parent == "type3" & type_kid == "type3"]$income_kid)

prop.table(table(dd$type_parent, dd$type_kid), 1)
cor(dd[, .(income_parent, income_kid)], method = 'spearman')

dd[, .(mean(as.numeric(upward)), .N), .(type_parent, type_kid)]
dd[, .(mean(as.numeric(upward)), .N), .(type_parent)]

# get coefficients from a multinnomial
m = multinom(type_kid ~ type_parent, data = dd)
summary(m)
pp = data.table(fitted(m))
unique(pp)
cf = coef(m)
cf

# compute probabilities
den1 = 1 + exp(cf[1,1]) + exp(cf[2,1])
den1
p1 = 1 / den1
p2 = exp(cf[1,1]) / den1
p3 = exp(cf[2,1]) / den1
print(c(p1, p2, p3))
sum(p1, p2, p3)e

den2 = 1 + exp(cf[1,1] + cf[1,2]) + exp(cf[2,1] + cf[2,2])
p1 = 1 / den2
p2 =  exp(cf[1,1] + cf[1,2]) / den2
p3 = exp(cf[2,1] + cf[2,2]) / den2
print(c(p1, p2, p3))
sum(p1, p2, p3)

den3 = (1 + exp(cf[1,1] + cf[1,3]) + exp(cf[2,1] + cf[2,3]))
p1 = 1 / den3
p2 = exp(cf[1,1] + cf[1,3]) / den3
p3 = exp(cf[2,1] + cf[2,3]) / den3
print(c(p1, p2, p3))
sum(p1, p2, p3)


# 70%
dd = data.table(income_parent = c(type_1_parent, type_2_parent, type_3_parent),
           type_parent = c(rep('type1', nsim), rep('type2', nsim), rep('type3', nsim)))

dd[, type_kid := sample_based_on_type(type_parent, 0.70)]
dd[, income_kid := assign_income_based_on_type(type_kid)]
cor(dd[, .(income_parent, income_kid)], method = 'spearman')

# get coefficients from a multinnomial
m = multinom(type_kid ~ type_parent, data = dd)
summary(m)

pp = data.table(fitted(m))
unique(pp)
cf = coef(m)
cf


# 40%
dd = data.table(income_parent = c(type_1_parent, type_2_parent, type_3_parent),
           type_parent = c(rep('type1', nsim), rep('type2', nsim), rep('type3', nsim)))

dd[, type_kid := sample_based_on_type(type_parent, 0.45)]
dd[, income_kid := assign_income_based_on_type(type_kid)]
cor(dd[, .(income_parent, income_kid)], method = 'spearman')

# get coefficients from a multinomial
m = multinom(type_kid ~ type_parent, data = dd)
summary(m)

pp = data.table(fitted(m))
unique(pp)
cf = coef(m)
cf

# check absolute income mobility
source("src/utils.R")
path = "models/MobHealthRecycling/output/testing/"
m = readMultipleFiles("mortality", path)

prop.table(table(m$smoker, m$income_type), 2)
prop.table(table(m[age > 18, income > parent_income]))

m[age > 18, .(mean(as.numeric(income > parent_income)), .N), .(income_type)]

hist(m[income_type == 1, income])
hist(dd[type_kid == "type1", income_kid])

hist(m[income_type == 2, income])
hist(dd[type_kid == "type2", income_kid])

hist(m[income_type == 3, income])
hist(dd[type_kid == "type3", income_kid])
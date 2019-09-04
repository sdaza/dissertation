###############################
# income mobility and health
# testing measures
# author: sebastian daza
################################

# libraries
library(data.table)
library(nnet)
library(texreg)

# functions
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


# Two groups
# type 1 $90,000 and $110,000 a year
# type 2$25,000 and $45,000
# inmobile world

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

test <- multinom(type_kid ~ type_parent, data = dd)

pp = data.table(fitted(test))
pp = unique(pp)
pp

cf = coef(test)
cf

p1 = 1 / (1 + (exp(cf[1,1]) + exp(cf[2,1])))
prop.table(table(dd[, .(type_parent, type_kid)]), 1)

# check some anylogic output

setwd('/Users/sdaza/Documents/github/dissertation/ch04/models/mob_health_testing/output')
data = fread('family.csv')

head(data)

prop.table(table(data[generation==1, .(parent_type, kid_type)]), 1)
table(data[generation==1, parent_type])

# explore income exposure variable 
setorder(data, -kid_income_exposure)
head(data)

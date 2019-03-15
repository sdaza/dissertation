##############################
# INLA models by quartile
# author: sebastian daza
##############################


# libraries
library(INLA)
library(brinla)
library(data.table)
library(ggplot2)
library(texreg)

# functions
source('related_projects/health_inequality_project/src/utils/extract_inla.R')
source('related_projects/health_inequality_project/src/utils/simulation_no_random_effects.R')
# source('related_projects/health_inequality_project/src/utils/simulation_no_random_effects.R')

# load data
df = readRDS('related_projects/health_inequality_project/data/le_cov_sel.rds')

df[, state := .GRP, by = statename]
df[, cty := .GRP, by = county]
df[, income_qr := .GRP, by = income_q]

table(df[, .(income_qr, income_q)]) # ok, right!

# reverse sign of relative mobility
df[, z_relative_mob := z_relative_mob * -1.0]

# auxiliry variables
df[, state_mob := state]
df[, state_gini := state]

female = df[gender=='F']
male = df[gender=='M']

# run models by gender and quartile

# baseline mode

# male

lmod = lm(le ~ z_relative_mob  + z_gini + log_population + log_income, male)

# pc prior
sdres <- sd(residuals(lmod))
pcprior <- list(prec = list(prior="pc.prec", param = c(3*sdres,0.01)))

# run models per income quartile
for (i in 1:4) {
    formula = le ~ z_relative_mob  + z_gini + log_population + log_income +
       f(state, model = "iid", hyper = pcprior)
    model = inla(formula, family = "gaussian", data = male[income_qr==i],
#           control.predictor=list(compute = TRUE),
          control.compute = list(config = TRUE, dic = TRUE,
                                 waic = TRUE, cpo = TRUE),
#           control.inla = list(strategy ="gaussian"),
          verbose = TRUE)

    model_name = paste0('m1_', i)
    assign(model_name, model)
    }

bri.hyperpar.summary(m1_1)

# create data for prediction
# all values in their means except for constrast: income mobility

# 4 quartiles for 2 contrast values
nrep = 2
relative_mob_pred_data = data.table(
    z_relative_mob = c(0.0, 1.0),
    z_gini = rep(0, nrep),
    log_population = rep(0, nrep),
    log_income = rep(0, nrep))

# simulate values per quartile
sim_male_m1 = data.table()

for (i in 1:4) {
    model_name = paste0('m1_', i)
    s = simulate_pred_no_re(model=get(model_name),
                                          data=relative_mob_pred_data,
                                          contrast='z_relative_mob',
                                          nsim = 2000)
    d = s[, .(q = i, fd = diff(pred)), by = sim][, .(q, fd)]
    sim_male_m1 = rbind(sim_male_m1, d)
}


saveRDS(sim_male_m1, file = 'related_projects/health_inequality_project//sim_male_m1.rds')

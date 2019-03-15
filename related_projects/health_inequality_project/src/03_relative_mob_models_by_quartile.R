##############################
# INLA models by quartile
# relative mobility
# author: sebastian daza
##############################

# libraries
library(INLA)
library(brinla)
library(data.table)
library(ggplot2)
library(texreg)
library(stringr)

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

# check
bri.hyperpar.summary(m1_1)

# create data for prediction
# all values in their means except for constrast: income mobility

# # 4 quartiles for 2 contrast values
# nrep = 2
# relative_mob_pred_data = data.table(
#     z_relative_mob = c(0.0, 1.0),
#     z_gini = rep(0, nrep),
#     log_population = rep(0, nrep),
#     log_income = rep(0, nrep))

# # simulate values per quartile
# sim_male_m1 = data.table()

# for (i in 1:4) {
#     model_name = paste0('m1_', i)
#     s = simulate_pred_no_re(model=get(model_name),
#                                           data=relative_mob_pred_data,
#                                           contrast='z_relative_mob',
#                                           nsim = 2000)
#     d = s[, .(q = i, fd = diff(pred)), by = sim][, .(q, fd)]
#     sim_male_m1 = rbind(sim_male_m1, d)
# }

# saveRDS(sim_male_m1, file = 'related_projects/health_inequality_project/data/sim_male_m1.rds')

# female

lmod <- lm(le ~ z_relative_mob + z_gini + log_population + log_income, female)

sdres <- sd(residuals(lmod))
pcprior <- list(prec = list(prior="pc.prec", param = c(3*sdres,0.01)))

# run models per income quartile
for (i in 1:4) {
    formula = le ~ z_relative_mob  + z_gini + log_population + log_income +
       f(state, model = "iid", hyper = pcprior)
    model = inla(formula, family = "gaussian", data = female[income_qr==i],
#           control.predictor=list(compute = TRUE),
          control.compute = list(config = TRUE, dic = TRUE,
                                 waic = TRUE, cpo = TRUE),
#           control.inla = list(strategy ="gaussian"),
          verbose = TRUE)

    model_name = paste0('f1_', i)
    assign(model_name, model)
}

# check
bri.hyperpar.summary(f1_1)

# # simulate per quartile
# sim_female_f1 = data.table()

# for (i in 1:4) {
#     model_name = paste0('f1_', i)
#     s = simulate_pred_no_re(model=get(model_name),
#                                           data=relative_mob_pred_data,
#                                           contrast='z_relative_mob',
#                                           nsim = 2000)
#     d = s[, .(q = i, fd = diff(pred)), by = sim][, .(q, fd)]
#     sim_female_f1 = rbind(sim_female_f1, d)
#     }

# saveRDS(sim_female_m1, file = 'related_projects/health_inequality_project/data/sim_female_f1.rds')

# adjusting for covariates

# male

# define PC prior
lmod <- lm(le ~ z_relative_mob  + z_gini + log_population + log_income +
           log_crime_rate + z_segregation_income +  log_pct_black + log_pct_hispanic +
           log_unemployment +  z_uninsured + z_medicare_expenses, male)


sdres <- sd(residuals(lmod))
pcprior <- list(prec = list(prior="pc.prec", param = c(3*sdres,0.01)))

# models per quartile
for (i in 1:4) {
    formula = le ~ z_relative_mob  + z_gini + log_population + log_income +
        log_crime_rate + z_segregation_income +  log_pct_black + log_pct_hispanic +
        log_unemployment +  z_uninsured + z_medicare_expenses +
        f(state, model = "iid", hyper = pcprior)
    model = inla(formula, family = "gaussian", data = male[income_qr==i],
#           control.predictor=list(compute = TRUE),
          control.compute = list(config = TRUE, dic = TRUE,
                                 waic = TRUE, cpo = TRUE),
#           control.inla = list(strategy ="gaussian"),
          verbose = TRUE)

    model_name = paste0('m2_', i)
    assign(model_name, model)

    }

# # create data for predictions
# nrep =  2 # 2 contrast values
# relative_mob_pred_data = data.table(
#     z_relative_mob       = c(0.0, 1.0),
#     z_gini               = rep(0, nrep),
#     log_population       = rep(0, nrep),
#     log_income           = rep(0, nrep),
#     log_crime_rate       = rep(0, nrep),
# #     log_poverty          = rep(0, nrep),
# #     log_mig_inflow       = rep(0, nrep),
# #     log_mig_outflow      = rep(0, nrep),
# #     log_foreign          = rep(0, nrep),
#     log_pct_black        = rep(0, nrep),
#     log_pct_hispanic     = rep(0, nrep),
# #     log_house_value      = rep(0, nrep),
# #     log_local_gov_exp    = rep(0, nrep),
#     log_unemployment     = rep(0, nrep),
#     z_segregation_income = rep(0, nrep),
# #     z_religion           = rep(0, nrep),
# #     z_labor_force        = rep(0, nrep),
# #     z_college            = rep(0, nrep),
# #     z_middle_class       = rep(0, nrep),
#     z_uninsured          = rep(0, nrep),
#     z_medicare_expenses  = rep(0, nrep))

# # simulate by quartile
# sim_male_m2 = data.table()

# for (i in 1:4) {
#     model_name = paste0('m2_', i)
#     s = simulate_pred_no_re(model=get(model_name),
#                                           data=relative_mob_pred_data,
#                                           contrast='z_relative_mob',
#                                           nsim = 2000)
#     d = s[, .(q = i, fd = diff(pred)), by = sim][, .(q, fd)]
#     sim_male_m2 = rbind(sim_male_m2, d)
#     }

# saveRDS(sim_male_m2, file = 'related_projects/health_inequality_project/data/sim_male_m2.rds')

# female

# define PC prior
lmod <- lm(le ~ z_relative_mob  + z_gini + log_population + log_income +
       log_crime_rate + z_segregation_income +  log_pct_black + log_pct_hispanic +
       log_unemployment +  z_uninsured + z_medicare_expenses, female)

# pc prior
sdres <- sd(residuals(lmod))
pcprior <- list(prec = list(prior="pc.prec", param = c(3*sdres,0.01)))

for (i in 1:4) {
    formula = le ~ z_relative_mob  + z_gini + log_population + log_income +
        log_crime_rate + z_segregation_income +  log_pct_black + log_pct_hispanic +
        log_unemployment +  z_uninsured + z_medicare_expenses +
        f(state, model = "iid", hyper = pcprior)
    model = inla(formula, family = "gaussian", data = female[income_qr==i],
#           control.predictor=list(compute = TRUE),
          control.compute = list(config = TRUE, dic = TRUE,
                                 waic = TRUE, cpo = TRUE),
#           control.inla = list(strategy ="gaussian"),
          verbose = TRUE)

    model_name = paste0('f2_', i)
    assign(model_name, model)

    }

# # simulate per quartile
# sim_female_f2 = data.table()

# for (i in 1:4) {
#     model_name = paste0('f2_', i)
#     s = simulate_pred_no_re(model=get(model_name),
#                                           data=relative_mob_pred_data,
#                                           contrast='z_relative_mob',
#                                           nsim = 2000)
#     d = s[, .(q = i, fd = diff(pred)), by = sim][, .(q, fd)]
#     sim_female_f2 = rbind(sim_female_f2, d)
#     }

# saveRDS(sim_female_f2, file = 'related_projects/health_inequality_project/data/sim_female_f2.rds')


# create tables with results

# relative mobility

for (i in 1:4) {
    cmodels <- c('Base Model', 'Base Model + Covariates', 'Base Model', 'Base Model + Covariates')
    models <- list(get(paste0('f1_', i)),
                   get(paste0('f2_', i)),
                   get(paste0('m1_', i)),
                   get(paste0('m2_', i)))

    cnames <- list(z_relative_mob = paste0('Q', i))

    # screenreg(models)
    t = texreg(models,
                include.dic = TRUE, include.waic = TRUE,
                ci.test = FALSE,
                float.pos = "htp",
                caption = "Life Expectancy (40) Models",
                booktabs = TRUE,
                use.packages = FALSE,
                dcolumn = TRUE,
                caption.above = TRUE,
                scalebox = 0.65,
                label = "inla_models",
                # sideways = TRUE,
                digits = 2,
                custom.model.names = cmodels,
                custom.coef.map = cnames,
                # groups = list("Random Effects" = c(4:5)),
                custom.note = "95\\% credibility intervals.")

    assign(paste0('tab_', i), t)
    remove(t)
}

heading = '\\renewcommand{\\arraystretch}{1.2}\n
\\begin{table}[htp]\n
\\begin{threeparttable}\n
\\caption{Estimates of association between life expectancy at age 40
  \\newline and relative income mobility\\tnote{1} (N = 1508 counties)}\\label{inla_models}\n
\\centering\n
\\setlength{\\tabcolsep}{1pt}\n
\\scriptsize\n
\\begin{tabular}{l D{.}{.}{5.11} D{.}{.}{5.11} D{.}{.}{5.11} D{.}{.}{5.11} }\n
\\hline\n
\\addlinespace\n
& \\multicolumn{2}{c}{Women} & \\multicolumn{2}{c}{Men} \\\\
Income Quartile & \\multicolumn{1}{c}{Base model\\tnote{2}} & \\multicolumn{1}{c}{Additional covariates\\tnote{3}}
& \\multicolumn{1}{c}{Base model} & \\multicolumn{1}{c}{Additional covariates} \\\\
\\addlinespace\n
\\hline'

heading =  gsub("\n\n", "\n", heading)

bottom = '\\addlinespace[5pt]\n
\\hline\n
\\end{tabular}\n
\\begin{tablenotes}[flushleft]\n
\\scriptsize\n
\\item [1] Four separated models (one per income quartile). Standardized coefficients and 95\\% credibility intervals in brackets.\n
\\item [2] Baseline model adjusts for log population and log income.\n
\\item [3] Social indicators model adjusts for log population, log income, log crime rate, log \\% Black, log \\% Hispanic, log unemployment, z-score income segregation, z-score \\% uninsured, and z-score Medicare expenses.\n\\end{tablenotes}\n\\end{threeparttable}\n
\\end{table}'

bottom =  gsub("\n\n", "\n", bottom)

sep = NA
for (i in 1:4) {
  sep[i] = "\n\\addlinespace\n"
}

tabs = list(tab_1, tab_2, tab_3, tab_4)

out = list()
for (i in 1:4) {
     out[[i]] = strsplit(tabs[[i]], '\\midrule')[[1]][2]
     out[[i]] = gsub('\n|\n\\\\', '', out[[i]])
}


# export table
cat(heading,
    sep[[1]], out[[1]],
    sep[[2]], out[[2]],
    sep[[3]], out[[3]],
    sep[[4]], out[[4]],
    bottom,
    file = 'related_projects/health_inequality_project/output/tables/main_table_inla_models.tex')

# supplementary table with covariates

# end

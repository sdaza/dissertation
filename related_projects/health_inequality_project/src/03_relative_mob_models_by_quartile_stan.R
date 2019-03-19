##############################
# stan models by quartile
# relative mobility
# author: sebastian daza
##############################

# libraries
library(brms)
library(data.table)
library(ggplot2)
library(texreg)
library(stringr)

# functions
source('related_projects/health_inequality_project/src/utils/extract_stan.R')
# source('related_projects/health_inequality_project/src/utils/simulation_no_random_effects.R')
# source('related_projects/health_inequality_project/src/utils/simulation_no_random_effects.R')

# load data
df = readRDS('related_projects/health_inequality_project/data/le_cov_sel.rds')
ncounties = length(unique(df$county))

# auxiliary variables
df[, state := .GRP, statename]
df[, income_qr := .GRP, income_q]

# check
table(df[, .(income_qr, income_q)])

# reverse sign of relative mobility
df[, z_relative_mob := z_relative_mob * -1.0]

female = df[gender=='F']
male = df[gender=='M']

# run models by gender and quartile
# baseline mode
# male

# run models per income quartile
for (i in 1:4) {
    print(paste0(':::: model male quartile ', i))
    fit = brm(le ~ z_relative_mob + z_gini + log_population + log_income +
              (1|state),
              data = male[income_qr==i],
              family = gaussian())
    model_name = paste0('m1_', i)
    assign(model_name, fit)
}

# female
# run models per income quartile
for (i in 1:4) {
    print(paste0(':::: model female quartile ', i))
    fit = brm(le ~ z_relative_mob + z_gini + log_population + log_income +
              (1|state),
              data = female[income_qr==i],
              family = gaussian())
    model_name = paste0('f1_', i)
    assign(model_name, fit)
}

# model with adjustments
# male
for (i in 1:4) {
    print(paste0(':::: model male adjusted quartile ', i))
    fit = brm(le ~ z_relative_mob + z_gini + log_population + log_income +
              z_segregation_income + log_pct_black + log_pct_hispanic +
              log_unemployment + z_uninsured + z_medicare_expenses +
              (1|state),
              data = male[income_qr==i],
              family = gaussian())
    model_name = paste0('m2_', i)
    assign(model_name, fit)
}

# female
for (i in 1:4) {
    print(paste0(':::: model female adjusted quartile ', i))
    fit = brm(le ~ z_relative_mob + z_gini + log_population + log_income +
              z_segregation_income + log_pct_black + log_pct_hispanic +
              log_unemployment + z_uninsured + z_medicare_expenses +
              (1|state),
              data = female[income_qr==i],
              family = gaussian())
    model_name = paste0('f2_', i)
    assign(model_name, fit)
}

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
    t = texreg(models,,
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

heading = paste0('\\renewcommand{\\arraystretch}{1.2}\n
\\setlength{\\tabcolsep}{11pt}\n
\\begin{table}[htp]\n
\\begin{threeparttable}\n
\\caption{Estimates of association between life expectancy at age 40
  \\newline and relative income mobility\\tnote{1} (N = ', ncounties, ' counties)}\\label{inla_models}\n
\\centering\n
\\scriptsize\n
\\begin{tabular}{l D{.}{.}{5.11} D{.}{.}{5.11} D{.}{.}{5.11} D{.}{.}{5.11} }\n
\\hline\n
\\addlinespace\n
& \\multicolumn{2}{c}{Women} & \\multicolumn{2}{c}{Men} \\\\
Income Quartile & \\multicolumn{1}{c}{Base model\\tnote{2}} & \\multicolumn{1}{c}{Additional covariates\\tnote{3}}
& \\multicolumn{1}{c}{Base model} & \\multicolumn{1}{c}{Additional covariates} \\\\
\\addlinespace\n
\\hline')

heading =  gsub("\n\n", "\n", heading)

bottom = '\\addlinespace[5pt]\n
\\hline\n
\\end{tabular}\n
\\begin{tablenotes}[flushleft]\n
\\scriptsize\n
\\item [1] Four separated robust models (one per income quartile). Standardized coefficients and 95\\% credibility intervals in brackets.\n
\\item [2] Baseline model adjusts for log population and log income.\n
\\item [3] Social indicators model adjusts for log population, log income, log \\% Black, log \\% Hispanic, log unemployment, z-score income segregation, z-score \\% uninsured, and z-score Medicare expenses.\n\\end{tablenotes}\n\\end{threeparttable}\n
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
    file = 'related_projects/health_inequality_project/output/tables/stan_relative_mob_models.tex')

# supplementary table with covariates
cmodels <- c('Women Q1', 'Women Q4', 'Men Q1', 'Men Q4')
models = list(f2_1, f2_4, m2_1, m2_4)

cnames <- list(
               z_relative_mob = 'Relative income mobility (z)',
               z_gini = 'Gini (z)',
               log_population = 'Log population',
               log_income = 'Log household income',
               # log_crime_rate = 'Log crime rate',
               z_segregation_income = 'Income segregation (z)',
               log_pct_black = 'Log percent Afroamerican',
               log_pct_hispanic = 'Log percent Hispanic',
               log_unemployment = 'Log unemployment rate',
               z_uninsured = 'Percent uninsured (z)',
               z_medicare_expenses = 'Medicare expenses (z)',
               "sd(Intercept)" = "SD states")

texreg(models,
       include.r2 = TRUE,
       ci.test = FALSE,
       float.pos = "htp",
       caption = paste0("Estimates of association between life expectancy at age 40
       \\newline and relative income mobility (N = ", ncounties, " counties)"),
       booktabs = TRUE,
       use.packages = FALSE,
       dcolumn = TRUE,
       caption.above = TRUE,
       scalebox = 0.65,
       label = "inla_models_cov",
       # sideways = TRUE,
       digits = 2,
       custom.model.names = cmodels,
       custom.coef.map = cnames,
       groups = list("\\addlinespace\n\\textit{Random Effects}" = 11),
       custom.note = "95\\% credibility intervals in brackets. z = standardized values.",
       file = 'related_projects/health_inequality_project/output/tables/stan_relative_mob_models_cov.tex')

# create counterfactual scenarios

options(digits=3)

# men

# prob male
# unadjusted
p_bottom = predict(m1_1, newdata=male[income_qr==1], re_formula = NA,
        summary=TRUE)
p_top = predict(m1_4, newdata=male[income_qr==1], re_formula = NA,
        summary=TRUE)

male_pred_baseline = fixed(quantile(p_top[, 1]-p_bottom[, 1],  prob = c(0.025, .5, 0.975)), digits=2)
male_pred_baseline

# counterfactual
cmale = copy(male)
cmale = cmale[income_qr==1]
cmale[, z_relative_mob := max(z_relative_mob)]

c_bottom = predict(m1_1, newdata=cmale, re_formula = NA,
        summary=TRUE)
c_top = predict(m1_4, newdata=male[income_qr==1], re_formula = NA,
        summary=TRUE)

male_con_baseline = round(quantile(c_top[, 1]-c_bottom[, 1],  prob = c(0.025, .5, 0.975)), 3)
male_con_baseline

male_diff_baseline = round(quantile((c_top[, 1]-c_bottom[, 1]) -  (p_top[, 1]-p_bottom[, 1]),
         prob =c(0.025, .5, 0.975)), 3)
male_diff_baseline

# adjusted
p_bottom = predict(m2_1, newdata=male[income_qr==1], re_formula = NA,
        summary=TRUE)
p_top = predict(m2_4, newdata=male[income_qr==1], re_formula = NA,
        summary=TRUE)

male_pred_adjusted= quantile(p_top[, 1]-p_bottom[, 1],  prob = c(0.025, .5, 0.975))
male_pred_adjusted

# counterfactual
cmale = copy(male)
cmale = cmale[income_qr==1]
cmale[, z_relative_mob := max(z_relative_mob)]

c_bottom = predict(m2_1, newdata=cmale, re_formula = NA,
        summary=TRUE)
c_top = predict(m2_4, newdata=male[income_qr==1], re_formula = NA,
        summary=TRUE)

male_con_adjusted= round(quantile(c_top[, 1]-c_bottom[, 1],  prob = c(0.025, .5, 0.975)), 3)
male_con_baseline

male_diff_adjusted= round(quantile((c_top[, 1]-c_bottom[, 1]) -  (p_top[, 1]-p_bottom[, 1]),
         prob =c(0.025, .5, 0.975)), 3)
male_diff_adjusted

paste0(

male_row = c(paste0(male_pred_baseline[2], ' [', male_pred_baseline[1], '; ', male_pred_baseline[3], ']'),
             paste0(male_con_baseline[2], ' [', male_con_baseline[1], '; ', male_con_baseline[3], ']'),
             paste0(male_diff_baseline[2], ' [', male_diff_baseline[1], '; ', male_diff_baseline[3], ']'),
             paste0(male_pred_adjusted[2], ' [', male_pred_adjusted[1], '; ', male_pred_adjusted[3], ']'),
             paste0(male_con_adjusted[2], ' [', male_con_adjusted[1], '; ', male_con_adjusted[3], ']'),
             paste0(male_diff_adjusted[2], ' [', male_diff_adjusted[1], '; ', male_diff_adjusted[3], ']')
              )
male_row = paste0(male_row, collapse = ' & ')



# adjusted
p_bottom = predict(m2_1, newdata=male[income_qr==1], re_formula = NA,
        summary=TRUE)
p_top = predict(m2_4, newdata=male[income_qr==1], re_formula = NA,
        summary=TRUE)

quantile(p_top[, 1]-p_bottom[, 1],  prob = c(0.025, .5, 0.975))

# counterfactual
cmale = copy(male)
cmale = cmale[income_qr==1]
cmale[, z_relative_mob := max(z_relative_mob)]

c_bottom = predict(m2_1, newdata=cmale, re_formula = NA,
        summary=TRUE)
c_top = predict(m2_4, newdata=male[income_qr==1], re_formula = NA,
        summary=TRUE)
quantile(c_top[, 1]-c_bottom[, 1],  prob = c(0.025, .5, 0.975))

quantile((p_top[, 1]-p_bottom[, 1]) - (c_top[, 1]-c_bottom[, 1]),
         prob =c(0.025, .5, 0.975))

# women
p_bottom = predict(f1_1, newdata=female[income_qr==1], re_formula = NA,
        summary=TRUE)
p_top = predict(f1_4, newdata=female[income_qr==1], re_formula = NA,
        summary=TRUE)

quantile(p_top[,1]-p_bottom[,1], prob = c(0.025, .5, 0.975))

cfemale = copy(female)
cfemale = cfemale[income_qr==1]
cfemale[, z_relative_mob := 5]

c_bottom = predict(f2_1, newdata=cfemale, re_formula = NA,
        summary=TRUE)
c_top = predict(f2_4, newdata=female[income_qr==4], re_formula = NA,
        summary=TRUE)
quantile(c_top[, 1]-c_bottom[, 1],  prob = c(0.025, .5, 0.975))

# end

##############################
# generative model income mobility and mortality
# exogenous IM exploration
# author: sebastian daza
##############################


library(data.table)
library(metafor)
library(texreg)
library(survival)
# library(lme4)
# library(coxme)
# library(AICcmodavg)

source("src/utils.R")
path = "models/MobHealthRecycling/output/verification/"

# no residential mobility
m = fread(paste0(path, "exogenousIM/mortality.csv"))
cty = fread(paste0(path, "exogenousIM/county.csv"))
p = fread(paste0(path, "exogenousIM/model_parameters.csv"))

print(paste0("Min pop: ", min(cty$population)))
print(paste0("Max time: ", max(cty$model_time)))

savepdf("output/plots/hist_rank_slope_1")
    hist(cty$rank_slope)
dev.off()

mtime = 990

# individual mortality
cox_models = list()
iterations = list(1:5, 6:10, 11:15)
f = formula("Surv(age, status) ~ total_rank_slope_exposure + lincome + county_lincome + (1|county)")
for (j in 1:3) {
    print(paste0("Iteration group: ", j))
    d = m[iteration %in% iterations[[j]]]
    d[, `:=`
        (status = 1,
        lincome = logIncome(income),
        county_lincome = logIncome(county_mean_income),
        replicate = iteration * 1000 + replicate
        )]
    replicates = sort(unique(d$replicate))
    cox_models[[j]] = coxModel(replicates, data = d, f = f)
}

cox_models_c = list()
f = formula("Surv(age, status) ~ county_rank_slope + lincome + county_lincome")
for (j in 1:3) {
    print(paste0("Iteration group: ", j))
    d = m[iteration %in% iterations[[j]]]
    d[, `:=`
        (status = 1,
        lincome = logIncome(income),
        county_lincome = logIncome(county_mean_income),
        replicate = iteration * 1000 + replicate
        )]
    replicates = sort(unique(d$replicate))
    cox_models_c[[j]] = coxModel(replicates, data = d, f = f, predictor = "county_rank_slope")
}

# county models
county_models = list()
f = formula("le ~ rank_slope + gini + lincome + lpopulation")
for (j in 1:3) {
    print(paste0("Iteration group: ", j))
    d = copy(cty[iteration %in% iterations[[j]] & model_time == mtime])
    d[, `:=`
        (lincome = logIncome(mean_income),
        lpopulation = logIncome(population),
        replicate = iteration * 1000 + replicate
        )]
    replicates = sort(unique(d$replicate))
    county_models[[j]] = linearModel(replicates, data = d, f = f)
}

models = list(cox_models, cox_models_c, county_models)
coeff_names = c("Individual total IM exposure (Cox)",
    "Individual County IM (Cox)",
    "County IM on LE (GLM)")

tab_list = list()
for (i in seq_along(models)) {
    tab_list[[i]] = texreg(models[[i]],
        custom.model.names = c("$\\beta$ = 0.5", "$\\beta$ = 0.3", "$\\beta$ = 0.0"),
        custom.coef.names = coeff_names[i],
        dcolumn = TRUE,
        booktabs = TRUE,
        float.pos = "htp",
        caption = "Estimates fake effect  $\\beta$  of IM on mortality",
        caption.above = TRUE
    )
}

# extract coefficients and create table
header = "
\\setlength{\\tabcolsep}{5pt}
\\renewcommand{\\arraystretch}{0.95}
\\begin{table}[htp]
\\scriptsize
\\caption{Estimates fake IM effect $\\beta$ on mortality}
\\label{ch04:exercise_01}
\\begin{center}
\\begin{tabular}{l D{.}{.}{3.9} D{.}{.}{3.9} D{.}{.}{3.8}}
\\toprule
 & \\multicolumn{1}{c}{$\\beta$ = 0.5} & \\multicolumn{1}{c}{$\\beta$ = 0.3} & \\multicolumn{1}{c}{$\\beta$ = 0.0} \\\\
\\midrule
"

bottom = "
\\bottomrule
\\multicolumn{4}{l}{\\tiny{$^*$ Null hypothesis value outside the confidence interval.}}
\\end{tabular}
\\end{center}
\\end{table}
"

tab = select_tab_coeff(tab_list, header, bottom)
cat(tab, file = "output/tables/exogenous_experiment_no_residential_mob.tex")

# residential mobility no segregation
m = fread(paste0(path, "exogenousIM-Moving/mortality.csv"))
cty = fread(paste0(path, "exogenousIM-Moving/county.csv"))
p = fread(paste0(path, "exogenousIM-Moving/model_parameters.csv"))

table(cty$county)
print(paste0("Min pop: ", min(cty$population)))
print(paste0("Max time: ", max(cty$model_time)))

savepdf("output/plots/hist_rank_slope_2")
    hist(cty$rank_slope)
dev.off()

# individual mortality
cox_models = list()
iterations = list(1:5, 6:10, 11:15)
f = formula("Surv(age, status) ~ total_rank_slope_exposure + lincome + county_lincome")
for (j in 1:3) {
    print(paste0("Iteration group: ", j))
    d = m[iteration %in% iterations[[j]]]
    d[, `:=`
        (status = 1,
        lincome = logIncome(income),
        county_lincome = logIncome(county_mean_income),
        replicate = iteration * 1000 + replicate
        )]
    replicates = sort(unique(d$replicate))
    cox_models[[j]] = coxModel(replicates, data = d, f = f)
}

cox_models_c = list()
f = formula("Surv(age, status) ~ county_rank_slope + lincome + county_lincome")
for (j in 1:3) {
    print(paste0("Iteration group: ", j))
    d = m[iteration %in% iterations[[j]]]
    d[, `:=`
        (status = 1,
        lincome = logIncome(income),
        county_lincome = logIncome(county_mean_income),
        replicate = iteration * 1000 + replicate
        )]
    replicates = sort(unique(d$replicate))
    cox_models_c[[j]] = coxModel(replicates, data = d, f = f, predictor = "county_rank_slope")
}

# county models
county_models = list()
f = formula("le ~ rank_slope + gini + lincome + lpopulation")
for (j in 1:3) {
    print(paste0("Iteration group: ", j))
    d = copy(cty[iteration %in% iterations[[j]] & model_time == mtime])
    d[, `:=`
        (lincome = logIncome(mean_income),
        lpopulation = logIncome(population),
        replicate = iteration * 1000 + replicate
        )]
    replicates = sort(unique(d$replicate))
    county_models[[j]] = linearModel(replicates, data = d, f = f)
}

models = list(cox_models, cox_models_c, county_models)
coeff_names = c("Individual total IM exposure (Cox)",
    "Individual County IM (Cox)",
    "County IM on LE (GLM)")

tab_list = list()
for (i in seq_along(models)) {
    tab_list[[i]] = texreg(models[[i]],
        custom.model.names = c("$\\beta$ = 0.5", "$\\beta$ = 0.3", "$\\beta$ = 0.0"),
        custom.coef.names = coeff_names[i],
        dcolumn = TRUE,
        booktabs = TRUE,
        float.pos = "htp",
        caption = "Estimates fake effect  $\\beta$  of IM on mortality",
        caption.above = TRUE
    )
}

tab = select_tab_coeff(tab_list, header, bottom)
cat(tab, file = "output/tables/exogenous_experiment_residential_mob.tex")

# residential mobility + segregation
m = fread(paste0(path, "exogenousIM-Seg/mortality.csv"))
cty = fread(paste0(path, "exogenousIM-Seg/county.csv"))
p = fread(paste0(path, "exogenousIM-Seg/model_parameters.csv"))

print(paste0("Min pop: ", min(cty$population)))
print(paste0("Max time: ", max(cty$model_time)))

savepdf("output/plots/hist_rank_slope_3")
    hist(cty$rank_slope)
dev.off()

# individual mortality
cox_models = list()
iterations = list(1:5, 6:10, 11:15)
f = formula("Surv(age, status) ~ total_rank_slope_exposure + lincome + county_lincome")
for (j in 1:3) {
    print(paste0("Iteration group: ", j))
    d = m[iteration %in% iterations[[j]]]
    d[, `:=`
        (status = 1,
        lincome = logIncome(income),
        county_lincome = logIncome(county_mean_income),
        replicate = iteration * 1000 + replicate
        )]
    replicates = sort(unique(d$replicate))
    cox_models[[j]] = coxModel(replicates, data = d, f = f)
}

cox_models_c = list()
f = formula("Surv(age, status) ~ county_rank_slope + lincome + county_lincome")
for (j in 1:3) {
    print(paste0("Iteration group: ", j))
    d = m[iteration %in% iterations[[j]]]
    d[, `:=`
        (status = 1,
        lincome = logIncome(income),
        county_lincome = logIncome(county_mean_income),
        replicate = iteration * 1000 + replicate
        )]
    replicates = sort(unique(d$replicate))
    cox_models_c[[j]] = coxModel(replicates, data = d, f = f, predictor = "county_rank_slope")
}

# county models
county_models = list()
f = formula("le ~ rank_slope + gini + lincome + lpopulation")
for (j in 1:3) {
    print(paste0("Iteration group: ", j))
    d = copy(cty[iteration %in% iterations[[j]] & model_time == mtime])
    d[, `:=`
        (lincome = logIncome(mean_income),
        lpopulation = logIncome(population),
        replicate = iteration * 1000 + replicate
        )]
    replicates = sort(unique(d$replicate))
    county_models[[j]] = linearModel(replicates, data = d, f = f)
}

models = list(cox_models, cox_models_c, county_models)
coeff_names = c("Individual total IM exposure (Cox)",
    "Individual County IM (Cox)",
    "County IM on LE (GLM)")

tab_list = list()
for (i in seq_along(models)) {
    tab_list[[i]] = texreg(models[[i]],
        custom.model.names = c("$\\beta$ = 0.5", "$\\beta$ = 0.3", "$\\beta$ = 0.0"),
        custom.coef.names = coeff_names[i],
        dcolumn = TRUE,
        booktabs = TRUE,
        float.pos = "htp",
        caption = "Estimates fake effect  $\\beta$  of IM on mortality",
        caption.above = TRUE
    )
}

tab = select_tab_coeff(tab_list, header, bottom)
cat(tab, file = "output/tables/exogenous_experiment_segregation.tex")

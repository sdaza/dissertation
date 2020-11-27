##############################
# generative model income mobility and mortality
# exogenous IM no segretation but residential mobility (0.05)
# author: sebastian daza
##############################


library(data.table)
library(metafor)
library(texreg)
library(lme4)
library(coxme)
library(AICcmodavg)

source("src/utils.R")

# no residential mobility

path = "models/MobHealthRecycling/output/verification/exogenousIM/"

m = fread(paste0(path, "mortality.csv"))
cty = fread(paste0(path, "county.csv"))
p = fread(paste0(path, "model_parameters.csv"))

summary(cty$nsi)

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

cox_models

cox_models_c = list()
f = formula("Surv(age, status) ~ county_rank_slope + lincome + county_lincome + (1|county)")
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

cox_models_c

# county models
county_models = list()
f = formula("le ~ rank_slope + gini + lincome + lpopulation + model_time + (1|county)")
for (j in 1:3) {
    print(paste0("Iteration group: ", j))
    d = copy(cty[iteration %in% iterations[[j]]])
    d[, `:=`
        (lincome = logIncome(mean_income),
        lpopulation = logIncome(population),
        model_time = factor(model_time),
        replicate = iteration * 1000 + replicate
        )]
    replicates = sort(unique(d$replicate))
    county_models[[j]] = linearModel(replicates, data = d, f = f)
}

a = texreg(cox_models,
    custom.model.names = c("$\\beta$ = 0.5", "$\\beta$ = 0.3", "$\\beta$ = 0.0"),
    custom.coef.names = "Individual total IM exposure (Cox-mixed-model)",
    dcolumn = TRUE,
    booktabs = TRUE,
    float.pos = "htp",
    caption = "Estimates fake effect  $\\beta$  of IM on mortality",
    caption.above = TRUE
)

b = texreg(cox_models_c,
    custom.model.names = c("$\\beta$ = 0.5", "$\\beta$ = 0.3", "$\\beta$ = 0.0"),
    custom.coef.names = "Individual County IM (Cox-mixed-model)",
    dcolumn = TRUE,
    booktabs = TRUE,
    float.pos = "htp",
    caption = "Estimates fake effect  $\\beta$  of IM on mortality",
    caption.above = TRUE
)

c = texreg(county_models,
    custom.model.names = c("$\\beta$ = 0.5", "$\\beta$ = 0.3", "$\\beta$ = 0.0"),
    custom.coef.names = "County IM on LE (Mixed-model)",
    dcolumn = TRUE,
    booktabs = TRUE,
    use.packages = FALSE,
    float.pos = "htp",
    caption = "Estimates fake effect  $\\beta$  of IM on mortality",
    caption.above = TRUE
)

county_models[[1]]

# extract coefficients
header = "
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
\\multicolumn{4}{l}{\\scriptsize{$^*$ Null hypothesis value outside the confidence interval.}}
\\end{tabular}
\\end{center}
\\end{table}
"

tab = list()
tab[[1]] = header
tab[[2]] = gsub("(.+midrule)(.+midrule)|\\\\bottomrule.+", "\\2", a)
tab[[3]] = gsub("(.+midrule)(.+midrule)|\\\\bottomrule.+", "\\2", b)
tab[[4]] = gsub("(.+midrule)(.+midrule)|\\\\bottomrule.+", "\\2", c)
tab[[5]] = bottom

tab = paste(tab, collapse = '')
tab = gsub("\\\\midrule", "", tab)

cat(tab, file = "output/tables/experiment_01.tex")
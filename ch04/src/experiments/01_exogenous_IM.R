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

# iterations
experiments = c("exogenous-IM-NoMob", "exogenous-IM-Mob", "exogenous-IM-Seg", "exogenous-IM-MobWeight")
iterations = list(1:5, 6:10, 11:15)
mtime = 990

# tables header and bottom
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

# iterate through each experiment
for (h in seq_along(experiments)) {

    print(paste0("Experiment : ", experiments[h], " ::::::::::::"))

    m = fread(paste0(path, experiments[h], "/mortality.csv"))
    cty = fread(paste0(path, experiments[h], "/county.csv"))
    p = fread(paste0(path, experiments[h], "/model_parameters.csv"))

    print(paste0("Min pop: ", min(cty$population)))
    print(paste0("Max time: ", max(cty$model_time)))
    print(paste0("Min IM: ", round(min(cty$rank_slope), 2), "; Max IM: ",
        round(max(cty$rank_slope), 2), "; Average: ", round(mean(cty$rank_slope), 2)))

    # individual mortality
    cox_models = list()
    f = formula("Surv(age, status) ~ total_rank_slope_exposure")
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
    f = formula("Surv(age, status) ~ county_rank_slope")
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
    f = formula("le ~ rank_slope")
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
    cat(tab, file = paste0("output/tables/", tolower(experiments[h]), ".tex"))
}

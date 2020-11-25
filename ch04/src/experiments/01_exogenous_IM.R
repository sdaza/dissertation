##############################
# generative model income mobility and mortality
# exogenous IM no segretation and residential mobility
# author: sebastian daza
##############################


library(data.table)
library(metafor)
library(texreg)
library(lme4)
library(coxme)
library(AICcmodavg)

source("src/utils.R")

# functions
coxModel = function(replicates, data,
    f = formula("Surv(age, status) ~ total_rank_slope_exposure + lincome + county_lincome + (1|county)"),
    predictor = "total_rank_slope_exposure") {

    yi = NULL
    sei = NULL
    for (i in replicates) {
        model = coxme::coxme(f, data = data[replicate == i])
        yi = c(yi, model$coefficients[predictor])
        sei = c(sei, AICcmodavg::extractSE(model)[predictor])
    }
    output = metafor::rma(yi = yi, sei = sei)

    return(output)
}

linearModel = function(replicates, data,
    f = formula("le ~ rank_slope + gini + lincome + lpopulation + model_time + (1|county)"),
    predictor = "rank_slope") {

    yi = NULL
    sei = NULL
    for (i in replicates) {
        model = lme4::lmer(f, data = data[replicate == i])
        yi = c(yi, fixef(model)[predictor])
        sei = c(sei, AICcmodavg::extractSE(model)[predictor])
    }
    output = metafor::rma(yi = yi, sei = sei)
    return(output)
}

path = "models/MobHealthRecycling/output/verification/exogenousIM/"

m = fread(paste0(path, "mortality.csv"))
cty = fread(paste0(path, "county.csv"))
p = fread(paste0(path, "model_parameters.csv"))


# individual mortality
cox_models = list()
iterations = list(1:5, 6:10, 11:15)
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
    cox_models[[j]] = coxModel(replicates, data = d)
}

# county models
county_models = list()
for (j in 1:3) {
    print(paste0("Iteration group: ", j))
    d = cty[iteration %in% iterations[[j]]]
    d[, `:=`
        (lincome = logIncome(mean_income),
        lpopulation = logIncome(population),
        model_time = factor(model_time),
        replicate = iteration * 1000 + replicate
        )]
    replicates = sort(unique(d$replicate))
    county_models[[j]] = linearModel(replicates, data = d)
}

county_models

str(county_models[[1]])

county_models

class(county_models[[1]])
as.vector(county_models[[1]]$b)
as.vector(county_models[[1]]$se)


extract.brms.select_coeff = function(model, include.r2 = TRUE, include.loo = FALSE,
                                     coeff_pattern = NULL,
                                     iteration = NULL) {
    s = summary(model)

    # fixed coefficients
    select = grepl(coeff_pattern, names(s$fixed[, 1]))
    coefficient.names = names(s$fixed[,1])[select]
    coefficients = s$fixed[, 1][select]
    ci.low = s$fixed[, "l-95% CI"][select]
    ci.upper = s$fixed[, "u-95% CI"][select]

    # rename coefficients
    coefficient.names = gsub(coeff_pattern, '', coefficient.names)

    # random
    if ('random' %in% names(s)) {
        r = s$random[[1]]
        random.names = stringr::str_replace_all(rownames(r), stringr::fixed('\\_'), "\\_")
        random.estimates = r[,1]
        random.lower = r[,'l-95% CI']
        random.upper = r[, 'u-95% CI']

        coefficient.names = c(coefficient.names, random.names)
        coefficients = c(coefficients, random.estimates)
        ci.low = c(ci.low, random.lower)
        ci.upper = c(ci.upper, random.upper)
    }

    gof = numeric()
    gof.names = character()
    gof.decimal = logical()

    if (iteration == 1) {

        gof = c(gof, s$nobs)
        gof.names = c(gof.names, "Num.\ obs.")
        gof.decimal = c(gof.decimal, FALSE)

        if ('ngrps' %in% names(s)) {
            for (i in seq_along(s$ngrps)) {
                gof = c(gof, s$ngrps[[i]])
                gof.names = c(gof.names, paste('Num.\ obs. ',
                stringr::str_replace_all(names(s$ngrps[i]), stringr::fixed('_'), '\\_')))
                gof.decimal = c(gof.decimal, FALSE)
            }
        }

        if (include.loo == TRUE) {
            loo = loo::loo(model, reloo=TRUE)
            gof = c(gof, loo$estimates[3,1])
            gof.names = c(gof.names, "LOO Information Criterion")
            gof.decimal = c(gof.decimal, FALSE)
        }

        if (include.r2 == TRUE) {
            r2 = brms::bayes_R2(model)
            gof = c(gof, round(r2[1, 1], 2))
            gof.names = c(gof.names, "Bayes $R^2$")
            gof.decimal = c(gof.decimal, TRUE)
        }

    }

    tr = texreg::createTexreg(coef.names = coefficient.names,
                              coef = coefficients,
                              ci.low = ci.low,
                              ci.up = ci.upper,
                              gof.names = gof.names,
                              gof = gof,
                              gof.decimal = gof.decimal
                              )
    return(tr)
}
#################################
# county income mobility and individual health
# auxiliary functions
# author: sebastian daza
################################


library(texreg)
library(ordinal)

# overwrite table and cor function to include missing data
table = function (...) base::table(..., useNA = 'ifany')
cor = function (...) stats::cor(..., use = "complete.obs")


# other functions
impute_locf = function(x) {
    output = zoo::na.locf(zoo::na.locf(x, FALSE), fromLast=TRUE)
    return(output)
}


impute_forward_backward = function(data,
                                   variabble,
                                   keys,
                                   new_name = paste0("imp_", variable)) {
    temp = data[!is.na(get(variable)), c(keys, variable), with = FALSE]
    setnames(temp, variable, new_name)
    temp = temp[data, on = keys, roll = TRUE, rollends = c(TRUE, TRUE)]
    return(temp)
}


fillWithFirstValue = function(x) {
    fv = head(na.omit(x), 1)
    x = ifelse(is.na(x), fv, x)
    return(x)
}


fillWithLastValue = function(x) {
    fv = tail(na.omit(x), 1)
    x = ifelse(is.na(x), fv, x)
    return(x)
}


renameColumns = function(dat, hash) {
    oldnames = as.vector(hash::keys(hash))
    newnames = as.vector(hash::values(hash))

    if (length(oldnames) != length(newnames)) {
        stop("Vector of names should have the same length")
    }

    setnames(dat, oldnames, newnames)
}


longText = function(text) {
    dtext = gsub("\n", "", text)
    dtext = gsub("\\s+", " ", dtext)
    return(dtext)
}


fillMissingColumns = function(dat, expression, expected) {
    varnames = names(dat)
    observed = grep(expression,  varnames, value = TRUE)
    if (length(observed) < length(expected)) {
        dat[, ( expected[!expected %in% observed] ) := NA]
    } else { print("No missing columns found")}
}


replaceMissing = function(x) {
    return(
        ifelse(x < 0, NA, x)
    )
}


getMax = function(x) {
    x = na.omit(x)
    if (length(x) == 0) {
        return(NA_real_)
    } else {
        return(max(x))
    }
}


getMin = function(x) {
    x = na.omit(x)
    if (length(x) == 0) {
        return(NA_real_)
    } else {
        return(min(x))
    }
}


getFirst = function(x) {
    x = na.omit(x)
    if (length(x) == 0) {
        return(NA_real_)
    } else {
        return(head(x, 1))
    }
}


createQuantiles = function(x, groups = 5) {
    output = cut(x,
                 breaks = quantile(x, probs = seq(0, 1, by = 1 / groups), na.rm = TRUE),
                 labels = 1:groups,
                 include.lowest = TRUE,
                 right = FALSE)
    return(output)
}


reverseScale = function(x) {
    max_x = getMax(x)
    return( (max_x - x) + 1 )
}


hashHHColumns = function(myhash, years, newheader) {

    old_names = NULL
    new_names = NULL
    mkeys = keys(myhash)

    for (i in seq_along(myhash)) {

        header = mkeys[i]
        vvalues = as.vector(values(myhash[header]))
        seqvalues = seq(1:length(vvalues))

        old_names = c(old_names, paste0(header, vvalues, "00"))
        new_names = c(new_names, paste0(newheader, seqvalues, "_", years[i]))

    }

    return(hash(old_names, new_names))
}


getCoefficients = function(micombine, position = 2, coeff = TRUE) {
    if (coeff) {
        v = summary(micombine)$results[position]
        names(v)
    } else { return ( summary(micombine)$se[position]) }
}


imputeAge = function(age, year) {
    if (any(is.na(age))) {
        min.age <- getMin(age)
        # ties
        position <- which(age == min.age)[1]
        if (!is.na(position)) {
            # initial values
            if (position > 1) {
                for (i in 1:(position-1)) {
                    age[position - i] <- age[position] - i
                }
            }
            # missing data position
            missing <- which(is.na(age))
            for (i in missing) {
                age[i] = age[i-1] + (year[i] - year[i-1])
            }
        }
        else { age = as.numeric(NA) }
    }
    return(age)
}


# combination of list for doparallel
comb = function(x, ...) {
    lapply(seq_along(x),
        function(i) c(x[[i]], lapply(list(...), function(y) y[[i]]))
    )
}

getSublist = function(mylist, name) {
    return(lapply(mylist, function(l) l[[name]]))
}

parlmice <- function(data, m = 5, seed = NA, cluster.seed = NA, n.core = NULL,
                     n.imp.core = NULL, cl.type = "PSOCK", ...) {

    # check form of data and m
    data <- check.dataform(data)
    m <- check.m(m)

    # check if data complete
    if (sum(is.na(data)) == 0){
        stop("Data has no missing values")
    }

    # check if arguments match CPU specifications
    if (!is.null(n.core)){
        if(n.core > parallel::detectCores()){
        stop("Number of cores specified is greater than the number of logical cores in your CPU")
        }
    }

    # determine course of action when not all arguments specified
    if (!is.null(n.core) & is.null(n.imp.core)){
        n.imp.core = m
        warning(paste("Number of imputations per core not specified: n.imp.core = m =", m, "has been used"))
    }
    if (is.null(n.core) & !is.null(n.imp.core)){
        n.core = parallel::detectCores() - 1
        warning(paste("Number of cores not specified. Based on your machine a value of n.core =", parallel::detectCores()-1, "is chosen"))
    }
    if (is.null(n.core) & is.null(n.imp.core)) {
        specs <- match.cluster(n.core = parallel::detectCores() - 1, m = m)
        n.core = specs$cores
        n.imp.core = specs$imps
    }
    if (!is.na(seed)){
        if(n.core > 1){
        warning("Be careful; the specified seed is equal for all imputations. Please consider specifying cluster.seed instead.")
        }
    }

    # create arguments to export to cluster
    args <- match.call(mice, expand.dots = TRUE)
    args[[1]] <- NULL
    args$m <- n.imp.core

    # make computing cluster
    cl <- parallel::makeCluster(n.core, type = cl.type)
    parallel::clusterExport(cl,
                            varlist = c("data", "m", "seed", "cluster.seed",
                                        "n.core", "n.imp.core", "cl.type",
                                        ls(parent.frame())),
                            envir = environment())
    parallel::clusterExport(cl,
                            varlist = "do.call")
    parallel::clusterEvalQ(cl, {library(mice); library(miceadds)})

    if (!is.na(cluster.seed)) {
        parallel::clusterSetRNGStream(cl, cluster.seed)
    }

    # generate imputations
    imps <- parallel::parLapply(cl = cl,
                                X = 1:n.core,
                                function(x) do.call(mice::mice, as.list(args),
                                                    envir = environment()))
    parallel::stopCluster(cl)

    # postprocess clustered imputation into a mids object
    imp <- imps[[1]]
    if (length(imps) > 1) {
       for (i in 2:length(imps)) {
          imp <- ibind(imp, imps[[i]])
       }
    }

    #let imputation matrix correspond to grand m
    for(i in 1:length(imp$imp)){
        colnames(imp$imp[[i]]) <- 1:imp$m
    }

    return(imp)
}


match.cluster <- function(n.core, m){
    cores <- 1:n.core
    imps <- 1:m
    out <- data.frame(results = as.vector(cores %*% t(imps)),
                      cores = cores,
                      imps = rep(imps, each = n.core))
    which  <- out[out[, "results"] == m, ]
    which[order(which$cores, decreasing = T), ][1, 2:3]
}

check.data <- function(data, method) {
  check.dataform(data)
}

check.dataform <- function(data) {

    if (!(is.matrix(data) || is.data.frame(data)))
        stop("Data should be a matrix or data frame", call. = FALSE)

    if (ncol(data) < 2)
        stop("Data should contain at least two columns", call. = FALSE)

    data <- as.data.frame(data)

    mat <- sapply(data, is.matrix)

    if (any(mat)) stop("Cannot handle columns with class matrix: ",
                       colnames(data)[mat])

    dup <- duplicated(colnames(data))
    if (any(dup)) stop("Duplicate names found: ",
                        paste(colnames(data)[dup], collapse = ", "))

    data
}

check.m <- function(m) {
  m <- m[1L]
  if (!is.numeric(m))
    stop("Argument m not numeric", call. = FALSE)
  m <- floor(m)
  if (m < 1L)
    stop("Number of imputations (m) lower than 1.", call. = FALSE)
  m
}

check.cluster <- function(data, predictorMatrix) {
    # stop if the cluster variable is a factor
    isclassvar <- apply(predictorMatrix == -2, 2, any)
    for (j in colnames(predictorMatrix)) {
        if (isclassvar[j] && lapply(data, is.factor)[[j]])
            stop("Convert cluster variable ", j, " to integer by as.integer()")
    }
    TRUE
}


bindImputationList = function(list_imputations) {
    imp = NULL
    for (i in 1:(length(list_imputations) - 1)) {
        if (i == 1) {
            imp = mice::ibind(list_imputations[[i]], list_imputations[[i+1]])
        }
        else if (i > 1) {
        imp = ibind(imp, list_imputations[[i]])
        }
    }
    return(imp)
}


extractImputations = function(imputations, sample_size = 10) {
    number_imputations = imputations$m
    if (number_imputations <= sample_size) {
        stop("Number of imputations is smaller than the sample size")
    }
    selection_imputations = sample(1:number_imputations, sample_size)
    list_imputations = list()
    for (i in seq_along(selection_imputations)) {
        list_imputations[[i]] = mice::complete(imp, selection_imputations[[i]])
    }
    print("Pooling imputations back!")
    pool =  miceadds::datlist2mids(list_imputations)
    return(pool)
}


truncateWeights = function(weights, level = 0.01) {
    tw = ifelse(weights < quantile(weights, probs = level),
                quantile(weights, probs = level), weights)
    tw = ifelse(weights > quantile(weights, probs = 1 - level),
                quantile(weights, probs = 1 - level), weights)
    return(tw)
}


unadjustedRegression = function(
    imputations,
    exposure_variable,
    exposure_type = "gaussian",
    id_var,
    time_var,
    max_time_exposure,
    outcomes,
    final_model_types,
    print_number_imputation = c(1, 5, 10, 15, 20),
    sampling_weight = NULL,
    strata = NULL,
    cluster = NULL,
    ntry = 5) {

    # auxiliary functions
    run_polr_model = function(formula, svy_design) {
        return(svyolr(formula, svy_design))
    }

    nimputations = order(unique(imputations$imp_num))

    output = list()
    for ( i in seq_along(outcomes) ){
        ll = list()
        for ( j in nimputations ) {
            ll[[j]] = list()
        }
        output[[i]] = ll
    }
    results = list()

    # loop over imputations

    for (i in nimputations) {

        # extract imputation
        dat = imputations[imp_num == i]

        # set order first
        setorderv(dat, c(id_var, time_var))

        dat[, max_time := max(get(time_var)), get(id_var)]
        last_obs = dat[get(time_var) == max_time]
        gdata = dat[get(time_var) <= max_time_exposure]

        gdata[, paste0("average_", exposure_variable) :=
            if (exposure_type == "gaussian") {
                mean(get(exposure_variable))
            }
            else if (exposure_type == "ordinal") {
                mean(as.numeric(as.character(get(exposure_variable))))
            },
            get(id_var)]

        setorderv(gdata, c(id_var, time_var))
        gdata = gdata[get(time_var) == max_time_exposure]
        gdata = gdata[, c(id_var,
                          paste0("average_", exposure_variable)),
                      with = FALSE]
        gdata[, (paste0("average_", exposure_variable)) :=
            scale(get(paste0("average_", exposure_variable)), scale = FALSE)]
        fdata = merge(last_obs, gdata, by = id_var)
        number_rows = nrow(fdata)

        fdata[, wt := 1]

        if (i %in% print_number_imputation) {
            print(paste0("Running models with imputation ", i, " with ", number_rows, " rows"))
        }

        if (is.null(sampling_weight)) {
            svy_design = svydesign(ids = ~ 1, weights = ~ wt, data = fdata)
        }
        else {
            svy_design = svydesign(ids = formula(paste0("~ ", cluster)),
                strata = formula(paste0("~ ", strata)),
                weights = formula(paste0("~ ", sampling_weight)),
                data = fdata, nest = TRUE)
        }

        for (h in seq_along(outcomes)) {

            final_model = formula(paste0(outcomes[h], " ~ ", paste0("average_", exposure_variable)))

            if (final_model_types[h] == "gaussian") {
                output[[h]][[i]] = svyglm(final_model, design = svy_design)
            }
            else if (final_model_types[h] == "binomial") {
                output[[h]][[i]] = svyglm(final_model, design = svy_design,
                    family = quasibinomial(link = "logit")
                )
            }
            else if (final_model_types[h] == "poisson") {
                output[[h]][[i]] = svyglm(final_model, design = svy_design,
                    family = quasipoisson(link = "log")
                )
            }
            else if (final_model_types[h] == "negative-binomial") {
                output[[h]][[i]] = sjstats::svyglm.nb(final_model,
                    design = svy_design
                )
            }
            else if (final_model_types[h] == "ordinal") {
                r = NULL
                attempt = 0
                while (is.null(r) && attempt <= ntry ) {
                    attempt = attempt + 1
                    try(
                        r <- run_polr_model(final_model, svy_design)
                    )
                }
                output[[h]][[i]] = r
            }
        }

    }

    print("Pooling results...")

    for (i in seq_along(outcomes)) {

        output_coeff = list()
        output_vcov = list()

        for (j in nimputations) {
            output_coeff[[j]] = coefficients(output[[i]][[j]])
            output_vcov[[j]] = vcov(output[[i]][[j]])
        }

        results[[outcomes[i]]] = mitools::MIcombine(
            output_coeff, output_vcov
        )

    }

    return(results)

}


ipwExposure = function(
    imputations,
    lag_variables,
    baseline_variables,
    numerator_time1 = "1",
    denominator_time1,
    numerator,
    denominator,
    exposure_variable,
    id_var,
    time_var,
    max_time_exposure,
    trim_p = 0.01,
    exposure_type = "gaussian",
    outcomes,
    predictors,
    final_model_types,
    print_weights = c(1, 5, 10, 15, 20),
    factor_columns = NULL,
    sampling_weight = NULL,
    strata = NULL,
    cluster = NULL,
    ntry = 5
    ) {

    # auxiliary functions
    run_clm_model = function(formula, data) {
        return(clm(formula, data = data))
    }
    run_polr_model = function(formula, svy_design) {
        return(svyolr(formula, design = svy_design))
    }

    # number of imputations
    nimputations = order(unique(imputations$imp_num))
    # create to lists to save output of the loop
    output = list()
    for ( i in seq_along(outcomes) ){
        ll <- list()
        for ( j in nimputations ) {
            ll[[j]] = list()
        }
        output[[i]] <- ll
    }
    final_weights = NULL
    results = list()

    # loop over imputations to compute weights
    for (i in nimputations) {

        # print(paste0("Number of imputation ", i))
        dat = imputations[imp_num == i]

        # set order first
        setorderv(dat, c(id_var, time_var))

        dat[, paste0("lag_", lag_variables) := lapply(.SD, shift), get(id_var),
            .SDcol = lag_variables]
        dat[, paste0("baseline_", baseline_variables) := lapply(.SD, getFirst), get(id_var),
           .SDcol = baseline_variables]

        if (!is.null(factor_columns)) {
            vars = lookvar(dat, factor_columns)
            dat[, (vars) := lapply(.SD, factor), .SDcol = vars]
        }

        # define working dataset
        dat[, max_time := max(get(time_var)), get(id_var)]
        last_obs = dat[get(time_var) == max_time]
        sdat = dat[get(time_var) <= max_time_exposure]

        # define temporal databases
        tempdata1 = sdat[get(time_var) == 1]
        tempdata2 = sdat[get(time_var) > 1]

        # define formulas of exposure models
        formula_1a = formula(paste0(exposure_variable, " ~ ", longText(numerator_time1)))
        formula_1b = formula(paste0(exposure_variable, " ~ ", longText(denominator_time1)))

        formula_2a = formula(paste0(exposure_variable, " ~ ", longText(numerator)))
        formula_2b = formula(paste0(exposure_variable, " ~ ", longText(denominator)))

        if (exposure_type == "gaussian") {

            # estimate weights for time == 1
            model1a = lm(formula_1a, data = tempdata1)
            model1b = lm(formula_1b,  data = tempdata1)

            kdens1 = dnorm(tempdata1[[exposure_variable]],
                           predict(model1a),
                           as.numeric(sd(model1a$residuals)))

            kdens2 = dnorm(tempdata1[[exposure_variable]],
                           predict(model1b),
                           as.numeric(sd(model1b$residuals)))

            weights_time_1 = kdens1 / kdens2
            rm(kdens1, kdens2)
            tempdata1[, ipw := weights_time_1]

            # estimate weights for time > 1 until max_time_exposure
            model2a = lm(formula_2a, data = tempdata2)
            model2b = lm(formula_2b, data = tempdata2)

            kdens1 = dnorm(tempdata2[[exposure_variable]],
                           predict(model2a),
                           as.numeric(sd(model2a$residuals)))

            kdens2 = dnorm(tempdata2[[exposure_variable]],
                           predict(model2b),
                           as.numeric(sd(model2b$residuals)))

            weights_time_2 = kdens1 / kdens2
            rm(kdens1, kdens2)
            tempdata2[, ipw := weights_time_2]

        } else if (exposure_type == "ordinal") {

            tempdata1[, (exposure_variable) := as.factor(get(exposure_variable))]
            tempdata2[, (exposure_variable) := as.factor(get(exposure_variable))]

            # estimate weights for time == 1
            r = NULL
            attempt = 0
            while( is.null(r) && attempt <= 5 ) {
                attempt = attempt + 1
                try(
                    r <- run_clm_model(formula_1a, tempdata1)
                )
            }
            model1a = r

            r = NULL
            attempt = 0
            while( is.null(r) && attempt <= ntry ) {
                attempt = attempt + 1
                try(
                    r <- run_clm_model(formula_1b, tempdata1)
                )
            }
            model1b = r

            probs1 = predict(model1a)$fit
            probs2 = predict(model1b)$fit

            weights_time_1 = probs1 / probs2
            rm(probs1, probs2)
            tempdata1[, ipw := weights_time_1]

            # estimate weights for time > 1 until max_time_exposure
            r = NULL
            attempt = 0
            while( is.null(r) && attempt <= ntry ) {
                attempt = attempt + 1
                try(
                    r <- run_clm_model(formula_2a, tempdata2)
                )
            }
            model2a = r

            r = NULL
            attempt = 0
            while( is.null(r) && attempt <= ntry ) {
                attempt = attempt + 1
                try(
                    r <- run_clm_model(formula_2b, tempdata2)
                )
            }
            model2b = r

            probs1 = predict(model2a)$fit
            probs2 = predict(model2b)$fit
            weights_time_2 =  probs1 / probs2

            rm(probs1, probs2)
            tempdata2[, ipw := weights_time_2]

        }

        gdata = rbind(tempdata1, tempdata2)
        gdata[, paste0("average_", exposure_variable) := if (exposure_type == "gaussian") {
                                                    mean(get(exposure_variable))
                                                 }
                                                 else if (exposure_type == "ordinal") {
                                                    mean(as.numeric(as.character(get(exposure_variable))))
                                                 },
                                                 get(id_var)]

        setorderv(gdata, c(id_var, time_var))
        gdata[, cipw := cumprod(ipw), get(id_var)]
        gdata = gdata[get(time_var) == max_time_exposure]
        gdata[, tcipw := truncateWeights(cipw, trim_p)]

        gdata = gdata[, c(id_var, "cipw", "tcipw",
                          paste0("average_", exposure_variable)),
                      with = FALSE]

        gdata[, (paste0("average_", exposure_variable)) :=
            scale(get(paste0("average_", exposure_variable)), scale = FALSE)]
        fdata = merge(last_obs, gdata, by = id_var)
        number_rows = nrow(fdata)
        final_weights = c(final_weights, fdata$cipw)

        if (mean(fdata$cipw) > 3) {
            stop(paste0(
                "Average of weights is too high in imputation ", i)
            )
        }


        if (i %in% print_weights) {
            print(paste0("Weights for imputation number ", i, " with ", number_rows, " rows"))
            print(paste0("Mean of weights: ", round(mean(fdata$cipw), 2)))
            print(paste0("Mean of weights (trunc): ", round(mean(fdata$tcipw), 2)))
        }

        if (is.null(sampling_weight)) {
            svy_design = svydesign(ids = ~ 1, weights = ~ tcipw, data = fdata)
        }
        else {
            fdata[, final_weight := tcipw * get(sampling_weight)]
            svy_design = svydesign(ids = formula(paste0("~ ", cluster)),
                strata = formula(paste0("~ ", strata)),
                weights = formula(paste0("~ ", "final_weight")),
                data = fdata, nest = TRUE)
        }

        for (h in seq_along(outcomes)) {

            # print(paste0(":::: Running ", outcomes[h]))

            final_model = formula(paste0(outcomes[h], " ~ ", predictors))

            if (final_model_types[h] == "gaussian") {
                output[[h]][[i]]  = svyglm(final_model, design = svy_design)
            }
            else if (final_model_types[h] == "binomial") {
                output[[h]][[i]] = svyglm(final_model, design = svy_design,
                    family = quasibinomial(link = "logit"))
            }
            else if (final_model_types[h] == "poisson") {
                output[[h]][[i]]  = svyglm(final_model, design = svy_design,
                     family = quasipoisson(link = "log"))
            }
            else if (final_model_types[h] == "negative-binomial") {
                output[[h]][[i]]  = sjstats::svyglm.nb(final_model, design = svy_design)
            }
            else if (final_model_types[h] == "ordinal") {
                r = NULL
                attempt = 0
                while( is.null(r) && attempt <= ntry ) {
                    attempt = attempt + 1
                    try(
                        r <- run_polr_model(final_model, svy_design)
                    )
                }
                output[[h]][[i]]  = r
            }
        }
    }

    print("Pooling results...")

    for (i in seq_along(outcomes)) {

        output_coeff = list()
        output_vcov = list()

        for (j in nimputations) {
            output_coeff[[j]] = coefficients(output[[i]][[j]])
            output_vcov[[j]] = vcov(output[[i]][[j]])
        }
        results[[outcomes[i]]] = mitools::MIcombine(
            output_coeff, output_vcov
        )

    }

    results[["weights"]] = final_weights

    return(results)

}


lookvar  = function(dat, varnames) {
    n  = names(dat)
    nn  = list()
        for (i in 1:length(varnames)) {
            nn[[i]]  = grep(varnames[i],n)
        }

    nn  = unlist(nn)

    if ( length(nn) >0 )
        {
         r  = n[nn]
         return(r)
        }
    else
    { return("No variables found")}
}


countmis = function(dat, vars = NULL, pct = TRUE, exclude.complete = TRUE) {

    if (is.null(vars)) {
        vars = names(dat)
    }

    mis = sort(sapply(dat[, vars, with = FALSE],
                       function(x) sum(is.na(x))), decreasing = TRUE)

    if (exclude.complete == TRUE) {
        mis = mis[ mis > 0]
    }

    if (pct == FALSE)
      { return(mis) }

    else if ( pct == TRUE ) {
        return( round(mis / nrow(dat), 3))
    }

}


savepdf = function(file, width=16, height=10) {

    fname = paste0(file, ".pdf")
    pdf(fname, width=width/2.54, height=height/2.54,
        pointsize=10)
    par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))

}


extract.MIcombine <- function(model, obs = 0) {

    s = summary(model)
    coefficient.names = rownames(s)
    coefficients = s$results
    standard.errors = s$se
    z = coefficients / standard.errors
    significance = 2 * pnorm(-abs(z))

    gof = numeric()
    gof.names = character()
    gof.decimal = logical()
    gof = c(obs, events)
    gof.names = c("Observations")
    gof.decimal = c(FALSEE)

    tr = createTexreg(
        coef.names = coefficient.names,
        coef = coefficients,
        se = standard.errors,
        pvalues = significance,
        gof.names = gof.names,
        gof = gof,
        gof.decimal = gof.decimal)
    return(tr)
}

setMethod("extract", signature = className("MIresult", "MItools"),
    definition = extract.MIcombine)


createModelTables = function(list_rows, row_names, row_labels, column_names,
                             observations = 0,
                             caption = "title",
                             label = "title",
                             arraystretch = 0.8,
                             tabcolsep = 10,
                             sideways = FALSE,
                             gofname = "Individuals",
                             comment = comment,
                             groups = NULL,
                             filename = "") {

    model_list = list()

    # loop to create texreg objects
    for (i in seq_along(column_names)) {
        coeff = NULL
        se = NULL
        for (h in seq_along(list_rows)) {
            sublist = summary(list_rows[[h]][[i]])
            vnames = rownames(sublist)
            position = grep(row_names[h], vnames)
            coeff = c(coeff, sublist$results[position])
            se = c(se, sublist$se[position])
            z = coeff / se
            significance = 1.96 * pnorm(-abs(z))
        }
        model_list[[column_names[[i]]]] = createTexreg(
            coef.names = row_labels,
            coef = coeff,
            se = se,
            pvalues = significance,
            gof.names = gofname,
            gof = observations,
            gof.decimal = FALSE)
        }

    tab = texreg(
        model_list,
        float.pos = "htp",
        caption = caption,
        booktabs = TRUE,
        use.packages = FALSE,
        dcolumn = TRUE,
        center = FALSE,
        caption.above = TRUE,
        label = label,
        sideways = sideways,
        digits = 2,
        custom.model.names = column_names,
        groups = if (!is.null(groups)) { groups },
        custom.note = ""
    )

    add_notes_table(tab,
                    tabcolsep = tabcolsep,
                    arraystretch = arraystretch,
                    comment = comment,
                    filename = filename)

}


add_notes_table = function(tab,
                           comment = "",
                           arraystretch = 0.8,
                           tabcolsep = 10,
                           filename = "",
                           header  = NULL,
                           header_replacement = NULL,
                           bottom = NULL,
                           bottom_replacement = NULL ,
                           closing = NULL,
                           closing_replacement = NULL) {

    if (is.null(header)) {
        header = "begin\\{table\\}\\[htp\\]\\n"
    }
    if (is.null(header_replacement)) {
        header_replacement = paste0("begin\\{table\\}\\[htp\\]\\\n",
                                    "\\\\setlength\\{\\\\tabcolsep\\}\\{",
                                    tabcolsep,
                                    "pt\\}\\\n\\\\renewcommand\\{\\\\arraystretch\\}\\{",
                                    arraystretch,
                                    "\\}\\\n",
                                    "\\\\begin\\{center\\}\\\n",
                                    "\\\\scriptsize\\\n",
                                    "\\\\begin\\{threeparttable\\}\\\n")
    }
    tab = gsub(header, header_replacement, tab)

    if (is.null(bottom)) {
        bottom = "end\\{tabular\\}\\n"
    }
    if (is.null(bottom_replacement)) {
        bottom_replacement = paste0("end\\{tabular\\}\\\n\\\\begin{tablenotes}\\\n\\\\scriptsize\\\n\\\\item ",
                                    comment,
                                    "\\\n\\\\end{tablenotes}\\\n")
    }
    tab = gsub(bottom, bottom_replacement, tab)

    if (is.null(closing)) {
        closing = "end\\{table\\}\\n"
    }
    if (is.null(closing_replacement)) {
        closing_replacement = paste0("end{threeparttable}\\\n",
                                     "\\\\end\\{center\\}\\\n",
                                     "\\\\end\\{table\\}\\\n"
                                    )
    }
    tab = gsub(closing, closing_replacement, tab)

    cat(tab, file = filename)

}


tableWeights = function(list_weights, model_names,
                        caption, label, comment,
                        filename,
                        tabcolsep = 10,
                        arraystretch = 1) {

    wmean = NULL
    wsd = NULL
    wp1 = NULL
    wp25 = NULL
    wp75 = NULL
    wp99 = NULL
    sumtab = NULL

    for (i in seq_along(list_weights)) {
        wmean = c(wmean, mean(list_weights[[i]]))
        wsd = c(wsd, sd(list_weights[[i]]))
        wp1 = c(wp1, quantile(list_weights[[i]], 0.01))
        wp25 = c(wp25, quantile(list_weights[[i]], 0.25))
        wp75 = c(wp75, quantile(list_weights[[i]], 0.75))
        wp99 = c(wp99, quantile(list_weights[[i]], 0.99))
    }

    sumtab = data.table(Weight = model_names,
                        Mean = wmean,
                        SD = wsd,
                        "1st" = wp1,
                        "25th" = wp25,
                        "75th" = wp75,
                        "99th" = wp99)

    tab = print(
        xtable::xtable(
            sumtab,
            caption = caption,
            label = label,
            align = "llcccccc"),
            include.rownames = FALSE,
            caption.placement = "top",
            table.placement = "htp",
            sanitize.text.function = identity
        )

    tab = gsub("begin\\{table\\}\\[htp\\]\\n",
               paste0("begin\\{table\\}\\[htp\\]\\\n\\\\centering\\\n\\\\footnotesize\\\n",
                      "\\\\setlength\\{\\\\tabcolsep\\}\\{", tabcolsep,
                      "pt\\}\\\n\\\\renewcommand\\{\\\\arraystretch\\}\\{",
                      arraystretch,
                      "\\}\\\n\\\\begin\\{threeparttable\\}\\\n"),
                 tab)

    tab = gsub("Weight & Mean & SD & 1st & 25th & 75th & 99th",
               "\\\\multicolumn\\{3\\}\\{c\\}\\{\\} & \\\\multicolumn\\{4\\}\\{c\\}\\{Percentiles\\} \\\\\\\\ \\\n \\\\cmidrule{4-7} \\\nWeight & Mean & SD & 1st & 25th & 75th & 99th", tab)

    tab = gsub("end\\{tabular\\}\\n",
                paste0("end\\{tabular\\}\\\n\\\\begin{tablenotes}\\\n\\\\footnotesize\\\n\\\\item ",
                       comment,
                       "\\\n\\\\end{tablenotes}\\\n\\\\end{threeparttable}\\\n"),
                tab)

    cat(tab, file = filename)

}

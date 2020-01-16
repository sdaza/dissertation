#################################
# individual mobility and health
# auxiliary functions
# author: sebastian daza
################################


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
    return(gsub("\n", "", text))
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
comb <- function(x, ...) {
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
                                function(x) do.call(mice, as.list(args),
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


truncateWeights = function(weights, level = 0.01) {
    tw = ifelse(weights < quantile(weights, probs = level),
                quantile(weights, probs = level), weights)
    tw = ifelse(weights > quantile(weights, probs = 1 - level),
                quantile(weights, probs = 1 - level), weights)
    return(tw)
}


ipwExposure = function(imputations,
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
                       final_model,
                       trim_p = 0.01,
                       exposure_type = "gaussian",
                       final_model_type = "gaussian") {

    # create to lists to save output of the loop
    output_coeff = list()
    output_vcov = list()

    # loop over imputations
    for (i in 1:imputations$m) {

        results = list()

        print(paste0("Number of imputation ", i))

        dat = data.table(mice::complete(imputations, i))
        dat[, age_interview_est := as.numeric(as.character(age_interview_est))]
        dat[, q_relative_mob := as.numeric(as.character(q_relative_mob))]
        dat[, q_gini := as.numeric(as.character(q_gini))]

        # set order first
        setorderv(dat, c(id_var, time_var))

        dat[, paste0("lag_", lag_variables) := lapply(.SD, shift), get(id_var),
            .SDcol = lag_variables]

        dat[, health := factor(health)]
        dat[, lag_health := factor(lag_health)]

        dat[, paste0("baseline_", baseline_variables) := lapply(.SD, getFirst), get(id_var),
           .SDcol = baseline_variables]

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
            model1a = polr(formula_1a, data = tempdata1)
            model1b = polr(formula_1b, data = tempdata1)

            probs1 = as.data.frame(predict(model1a, type = "probs"))
            probs2 = as.data.frame(predict(model1b, type = "probs"))

            v_numerator = rep(NA, nrow(tempdata1))
            v_denominator = rep(NA, nrow(tempdata1))

            for (j in unique(tempdata1[[exposure_variable]])) {
                flag = tempdata1[[exposure_variable]] == j
                v_denominator[flag] = probs2[flag, j]
                v_numerator[flag] = probs1[flag, j]
            }

            weights_time_1 = v_numerator / v_denominator
            rm(v_numerator, v_denominator)
            tempdata1[, ipw := weights_time_1]

            # estimate weights for time > 1 until max_time_exposure
            model2a = polr(formula_2a, data = tempdata2)
            model2b = polr(formula_2b, data = tempdata2)

            probs1 = as.data.frame(predict(model2a, type = "probs"))
            probs2 = as.data.frame(predict(model2b, type = "probs"))

            v_numerator = rep(NA, nrow(tempdata2))
            v_denominator = rep(NA, nrow(tempdata2))

            for (j in unique(tempdata2[[exposure_variable]])) {
                flag = tempdata2[[exposure_variable]] == j
                v_denominator[flag] = probs2[flag, j]
                v_numerator[flag] = probs1[flag, j]
            }

            weights_time_2 = v_numerator / v_denominator
            rm(v_numerator, v_denominator)
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
        fdata = merge(last_obs, gdata, by = id_var)

        print(paste0("Mean of weights: ", round(mean(fdata$cipw), 2)))
        print(paste0("Mean of weights (trunc): ", round(mean(fdata$tcipw), 2)))

        svy_design = svydesign(ids = ~ 1, weights = ~ tcipw, data = fdata)

        if (final_model_type == "gaussian") {
            output = svyglm(final_model, design = svy_design)
        }
        else if (final_model_type == "binomial") {
            output = svyglm(final_model, design = svy_design,
                            family = quasibinomial)
        }
        else if (final_model_type == "poisson") {
            output = svyglm(final_model, design = svy_design,
                            family = poisson)
        }
        else if (final_model_type == "ordinal") {
            output = svyolr(final_model, design = svy_design)
        }

        output_coeff[[i]] =  coefficients(output)
        output_vcov[[i]] = vcov(output)

    }

        return(mitools::MIcombine(output_coeff, output_vcov))

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


countmis  = function(dat, vars = NULL, pct = TRUE, exclude.complete = TRUE) {

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

    return(mis)
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
                             caption = "title", label = "title",
                             fontsize = "scriptsize",
                             arraystretch = 0.8,
                             tabcolsep = 10,
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
            significance = 2 * pnorm(-abs(z))
        }
        model_list[[column_names[[i]]]] = createTexreg(
            coef.names =  row_labels,
            coef = coeff,
            se = se,
            pvalues = significance,
            gof.names = 'Observations',
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
        caption.above = TRUE,
        fontsize = fontsize,
        label = label,
        center = TRUE,
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

createModelTables(list_rows, column_names = column_names,
                  row_names = row_names, row_labels = row_labels,
                  filename = "ch03/output/test_table.tex",
                  comment = comment,
                  groups = groups
                  )

add_notes_table = function(tab,
                           comment = "",
                           arraystretch = 0.8,
                           tabcolsep = 10,
                           filename = "") {

    tab = gsub("begin\\{table\\}\\[htp\\]\\n",
                paste0("begin\\{table\\}\\[htp\\]\\\n\\\\centering\\\n",
                       "\\\\setlength\\{\\\\tabcolsep\\}\\{", tabcolsep,
                       "pt\\}\\\n\\\\renewcommand\\{\\\\arraystretch\\}\\{",
                       arraystretch,
                       "\\}\\\n\\\\begin\\{threeparttable\\}\\\n"),
                 tab)

    tab = gsub("end\\{tabular\\}\\n",
                paste0("end\\{tabular\\}\\\n\\\\begin{tablenotes}\\\n\\\\scriptsize\\\n\\\\item ",
                       comment,
                       "\\\n\\\\end{tablenotes}\\\n"),
                tab)

    tab = gsub("end\\{center\\}\\n",
                paste0("end\\{center\\}\\\n\\\\end{threeparttable}\\\n"),
                tab)

    cat(tab, file = filename)

}

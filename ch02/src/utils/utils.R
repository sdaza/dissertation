##########################################
# CDC mortality - income mobility paper
# utilities
# author: sebastian daza
##########################################


# load libraries
library(sdazar)
library(ggplot2)
library(ggridges)
library(ggthemes)
library(stringr)
library(INLA)
library(brinla)
library(texreg)

# redefine table base function
table = function (...) base::table(..., useNA = 'ifany')

# log transformation
logtrans = function(data, variables, center = TRUE) {
    data[, (paste0('log_', variables)) := lapply(.SD,
    function(x) scale(
        ifelse(x <= 0.0, log(0.001), log(x)),
        scale = FALSE, center = center)),
        .SDcols = (variables)
    ]
}

# standardization
zscores  = function(dat, variables) {
    for (i in 1:length(variables)) {
        dat[, paste0("z_", variables[i]) := scale(get(variables[i]))]
    }
    return(dat)
}

# function to check missing counties
checkCounties = function(dataA, dataB, states, county_var) {

  checkCountiesA = list()
  checkCountiesB = list()

  for (i in seq_along(vstates)) {
        a = dataA[state == vstates[i], .N, by = county_var][order(get(county_var)), get(county_var)]
        b = dataB[state == vstates[i], .N, by = county_var][
                order(get(county_var)), get(county_var)]
        c = b[which(! b %in% a)] # population
        d = a[which(! a %in% b)] # mortality
        if (length(c) > 0) { checkCountiesA[[vstates[i]]] = c }
        if (length(d) > 0) { checkCountiesB[[vstates[i]]] = d }
  }
  print('Counties A')
  print(checkCountiesA)
  print('Counties B ')
  print(checkCountiesB)
}

# inla.contrib.sd function contributed by Gianluca Baio
inla.contrib.sd = function(model, nsamples = 1000) {
    ## Computes the sd for the random effects in an INLA model
    ## 1. Defines the precision (generates a matrix with bins and
    ## density on the precision scale)
    ## 2. Simulates replications from the posterior distributions of
    ## the quantities of interest

    ## Names of the variables associated with structured effects
    rand.effs = names(model$marginals.hyperpar)
    for (i in 1:length(rand.effs)) {
        cmd = paste("prec.marg.",i,"=model$marginals.hyperpar$'",rand.effs[i],"'",sep = "")
        # marginal distribution of the precision, tau
        ## Simulation from the posterior marginal distribution for sigma = 1/sqrt(tau)
        eval(parse(text = cmd))
        cmd = paste("sigma.", i,
                     "= inla.rmarginal(nsamples,inla.tmarginal(function(x) 1/sqrt(x), prec.marg.",
                     i,"))",sep = "")
        eval(parse(text = cmd))
    }
    ## Outputs of the function
    mat = matrix(NA, nsamples, length(rand.effs))
    for (i in 1:length(rand.effs)) {
        cmd = paste("mat[,i] = sigma.",i,sep = "")
        eval(parse(text = cmd))
    }
    names2 = gsub("Precision","sd",rand.effs)
    colnames(mat) = names2

    tab = matrix(NA,length(rand.effs),4)
    for (i in 1:length(rand.effs)) {
        tab[i,] = c(mean(mat[,i]),sd(mat[,i]),quantile(mat[,i],.025),quantile(mat[,i],.975))
    }
    rownames(tab) = names2
    colnames(tab) = c("mean","sd","2.5%","97.5%")

    return (list(samples = mat, hyper = tab))
}


# plot distribution of fixed-effects
plot_fixed_coeff = function(model, coeff, coeff_labels = NULL, ylimits = NULL,
                            exponential = FALSE,
                            colors = c('#e34a33', '#2b8cbe', '#31a354'),
                            group_labels = NULL,
                            nsample = 1000) {

    if (class(model) == 'inla') { model = list(model) }
    if (is.null(group_labels)) { group_labels = rep('None', length(model)) }
    if (is.null(coeff_labels)) { coeff_labels = coeff }

    coefficient_values = list()

    for (i in seq_along(model)) {
        print(paste0('::::: Model ', i, ' :::::'))

        for (h in seq_along(coeff)) {
            samples = inla.rmarginal(nsample, model[[i]]$marginals.fixed[[coeff[h]]])
            coefficient_values[[paste0(i, '-', h)]] = data.table(group = group_labels[[i]],
                                                                 coefficient = coeff[h],
                                                                 value = samples)
        }
    }

    mf = rbindlist(coefficient_values)
    if (exponential) { mf[, value := exp(value)] }
    mf[, coefficient := factor(coefficient, levels = coeff, labels = coeff_labels)]
    mf[, group := factor(group, levels = rev(group_labels))]

    if (length(model) == 1) {
        p = ggplot(mf, aes(x = value, group = coefficient, fill = coefficient)) +
                   geom_density(alpha = 0.3, color = "white") +
                   geom_vline(xintercept = ifelse(exponential, 1, 0), color = 'gray', linetype = 3) +
                   xlab('\nMarginal posterior distribution of coefficients') +
                   ylab('Density\n') +
                   theme_classic() +
                   scale_colour_manual(values = colors) +
                   scale_fill_manual(values = colors) +
                   theme(axis.text.x = element_text(angle = 0, hjust = 1),
                         legend.position = "top",
                         legend.title = element_blank()) +
                   if (!is.null(ylimits)) {scale_x_continuous(limits = ylimits)}
        return(p)
    }
    else {
        p = ggplot(mf, aes(y = group)) +
                   geom_density_ridges(aes(x = value, fill = coefficient),
                                       alpha = 0.3, color = "white", scale = 0.8) +
                   xlab('\nMarginal posterior distribution') +
                   ylab('') +
                   theme_classic() +
                   scale_fill_cyclical(values = colors, guide= 'legend', name = '') +
                   geom_vline(xintercept = ifelse(exponential, 1, 0), size = 0.2, alpha = 0.3, linetype = 2) +
                   theme(legend.position = 'top')
        return(p)
   }
}


# get random fixed effect coefficients
get_coeff_fixed_random_effect = function(sim_data, fixed, random, prob = 0.95) {

    if (length(fixed) != length(random)) {
        stop('Fixed and random effects should have same length')
    }

    lprob = (1 - prob) / 2
    hprob = 1 - lprob

    nsim = length(sim_data)
    sim_names = rownames(sim_data[[1]]$latent)

    outputs = list()

    for (v in seq_along(fixed)) {

        lfixed  = paste0('^', fixed[v])
        lrandom = paste0('^', random[v])

        regex = paste0(c(lfixed, lrandom), collapse ='|')
        selected_names = grep(regex, sim_names, value = TRUE)

        values = list()
            for (i in 1:nsim) {
             vrandom = selected_names[grep(lfixed, selected_names, invert = TRUE)]
             vfixed = selected_names[grep(lfixed, selected_names, invert = FALSE)]
             values[[i]] =  sim_data[[i]]$latent[vfixed,] + sim_data[[i]]$latent[vrandom,]
        }

        sim_coeff = do.call(cbind, values)

        outputs[[v]] = data.table(
            id = seq_along(vrandom),
            m = apply(sim_coeff, 1, median),
            lo = apply(sim_coeff, 1, function(x) quantile(x, probs = lprob)),
            hi = apply(sim_coeff, 1, function(x) quantile(x, probs = hprob)))

    }

    if (length(fixed) == 1) {

        moutput = outputs[[1]]
        t = nrow(moutput)
        moutput[, name := rep(random, t)]

    } else {
          t = nrow(outputs[[1]])
          moutput = rbindlist(outputs)
          moutput[, id ]
          moutput[, name := rep(random, each = t)]
    }

    return(moutput)
}

# plot random and fixed effects using simulation
plot_random_fixed_effects_sim = function(sim_data, fixed = NULL, random = NULL,
                                         prob = 0.95, sorted = FALSE, exponential = FALSE,
                                         xlabel = 'x', ylabel = 'y',
                                         x_labels = NULL,
                                         effects_labels = NULL,
                                         colors = c('#e34a33', '#2b8cbe', '#31a354')) {

    dts = get_coeff_fixed_random_effect(sim_data, fixed = fixed, random = random,
                                        prob = prob)

    if (sorted) {
        setorder(dts, m)
        id_levels = unique(dts$id)
        name_levels = unique(dts$name)

    } else {
        id_levels = unique(dts$id)
        name_levels = unique(dts$name)
    }

    if (is.null(x_labels)) {
        x_labels = unique(as.character(dts$id))
    }

    if (sorted) { x_labels = x_labels[id_levels] }


    if (is.null(effects_labels)) {
        effects_labels = unique(as.character(dts$name))
    }

    if (exponential) {
        dts[, c('m', 'lo', 'hi') := lapply(.SD, exp), .SDcols = c('m', 'lo', 'hi')]

    }

    dts[, id := factor(id, levels = id_levels, labels = x_labels)]
    dts[, name := factor(name, levels = name_levels, labels = effects_labels)]
    num_eff = length(unique(dts$name))

    if (num_eff > 1) {
         p = ggplot(dts, aes(x = id, y = m)) +
                    geom_ribbon(aes(ymin = lo, ymax = hi, group = name, fill = name), alpha = 0.3)  +
                    geom_point(aes(color = name), size = 0.9, alpha = 0.3)  +
                    geom_line(aes(color = name, group = name), size = 0.4, alpha = 0.3)  +
                    theme_classic() +
                    scale_color_manual(values = colors) +
                    scale_fill_manual(values = colors) +
                    labs(x = xlabel, y = ylabel) +
                    geom_hline(yintercept = ifelse(exponential, 1, 0),  size = 0.2, alpha = 0.3, linetype = 2) +
                    theme(axis.text.x = element_text(angle = 40, hjust = 1),
                    legend.position = "top",
                    legend.title = element_blank())
    }

    else if (num_eff == 1) {
        p = ggplot(dts, aes(x = id, y = m)) +
                   geom_ribbon(aes(ymin = lo, ymax = hi),  group = 1, fill = '#a6bddb', alpha = 0.3)  +
                   geom_point(size = 0.9, color = '#e34a33', alpha = 0.3)  +
                   geom_line(color = '#2b8cbe', size = 0.4, group = 1, alpha = 0.3)  +
                   theme_classic() +
                          labs(x = xlabel, y = ylabel) +
                          geom_hline(yintercept = ifelse(exponential, 1, 0),  size = 0.2, alpha = 0.3, linetype = 2) +
                   theme(axis.text.x = element_text(angle = 40, hjust = 1))
    }

    return(p)
}


# plot predicted p-values
hist_pred_pvalues = function(model, outcome, breaks = 10) {
    output = NA

    for (i in (1:length(outcome))) {
        output[i] = inla.pmarginal(q = outcome[i],
                                   marginal = model$marginals.fitted.values[[i]])
    }

    hist(output, main = "",
         breaks = breaks,
         xlab = "Posterior predictive p-value")
}


# plots LOO
plot_loo = function(model) {

    hist(model$cpo$pit, breaks = 10,
         xlab = 'PIT',
         main = paste("CPO Failures = ", round(sum(model$cpo$failure), 0)))

    qqplot(qunif(ppoints(length(model$cpo$pit))), model$cpo$pit,
       main = "Q-Q plot for Unif(0,1)",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       cex = .9)

    qqline(model$cpo$pit, distribution = function(p) qunif(p), prob = c(0.1, 0.9))
    }


# compute life expectancy counterfactuals
estimate_le_counterfactuals = function(sim_data,
                                       fixed = NULL,
                                       random = NULL,
                                       value_matrix = NULL,
                                       age_interval = 5,
                                       ax0 = 0.3,
                                       axv = 0.5) {

    nsim = length(sim_data)
    sim_names = rownames(sim_data[[1]]$latent)

    lfixed  = c(paste0('^', fixed), '(Intercept)')
    lrandom = paste0('^', random, ':')
    regex = paste0(c(lfixed, lrandom), collapse ='|')
    selected_names = grep(regex, sim_names, value = TRUE)

    output = list()

    for (i in 1:nsim) {

        d = sim_data[[i]]$latent
        d = data.table(values = d[selected_names,])
        d = transpose(d)
        setnames(d, selected_names)

        d = melt(d, measure = patterns(lrandom),
                 value.name = random, variable.name = 'age_group')

        clean_fixed_names = gsub('\\^', '', lfixed)
        setnames(d, paste0(clean_fixed_names, ':1'), clean_fixed_names)
        setnames(d, '(Intercept)', 'constant')

        # compute rates
        for (h in 1:nrow(value_matrix)) {
            d[, (rownames(value_matrix)[h]) :=
              exp(as.matrix(d[, c('constant', fixed, random), with = FALSE]) %*% value_matrix[h,])]
        }

        dt = data.table(age = d$age_group)

        # life table parameters
        n = rep(age_interval, nrow(d)) # interval
        n[length(n)] = NA
        ax = rep(axv, nrow(d))
        ax[1] = ax0
        ax[length(ax)] = NA

        for (j in rownames(value_matrix)) {
            mx = d[, j, with = FALSE][[1]]
            dt[, (j) := fmsb::lifetable2(mx, ax = ax, n = n)$ex]
        }
        output[[i]] = dt
    }

    return(rbindlist(output))
}


# plot le counterfactuals
plot_le_counterfactuals = function(dt, relative = FALSE,
                                   x_labels = NULL,
                                   xlabel = '\nLife expectancy at age',
                                   ylabel = 'Difference in years\n',
                                   name_groups = c('Mobility', 'Gini'),
                                   prob = 0.95,
                                   colors = c('#e34a33', '#2b8cbe', '#31a354')) {

    if (relative == TRUE) {
        ylabel = 'Relative difference\n'
    }

    lprob = (1 - prob) / 2
    hprob = 1  -lprob

    if (relative) {
      dt[, mob := (mob_1 - mob_0) / mob_0]
      dt[, gini := (gini_1 - gini_0) / gini_0]
    }
    else {
    dt[, mob := mob_1 - mob_0]
    dt[, gini := gini_1 - gini_0]
    }

    m = melt(dt, id.vars = 'age',
             measure.vars = c('mob', 'gini'))

    if (!is.null(name_groups)) {
      m[, variable := factor(variable, labels = name_groups)]
    }

    if (!is.null(x_labels)) {
      m[, age := factor(age, labels = x_labels)]
    }

    s = m[, .(m = median(value),
          lo = quantile(value, lprob),
          hi = quantile(value, hprob)),
          by = .(age, variable)]

    p = ggplot(s, aes(x = age, y = m)) +
               geom_ribbon(aes(ymin = lo, ymax = hi, group = variable, fill = variable), alpha = 0.3)  +
               geom_point(aes(color = variable), size = 0.9, alpha = 0.3)  +
               geom_line(aes(color = variable, group = variable), size = 0.4, alpha = 0.3)  +
               theme_classic() +
               scale_color_manual(values = colors) +
               scale_fill_manual(values = colors) +
               labs(x = xlabel, y = ylabel) +
               geom_hline(yintercept = 0,  size = 0.2, alpha = 0.3, linetype = 2) +
               theme(axis.text.x = element_text(angle = 40, hjust = 1),
               legend.position = "top",
               legend.title = element_blank())

      return(p)
}


# plot le differences
plot_le_differences = function(data,
                               ylim_hi = 0.35,
                               ylim_low = -2.0,
                               colors = c('#e34a33', '#2b8cbe', '#31a354')) {
    p = ggplot(data, aes(x = age, y = m)) +
               geom_ribbon(aes(ymin = lo, ymax = hi, group = group, fill = group), alpha = 0.1)  +
               geom_point(aes(color = group), size = 0.9, alpha = 0.3)  +
               geom_line(aes(color = group, group = group), size = 0.4, alpha=0.3)  +
               theme_classic() +
               scale_color_manual(values = colors) +
               scale_fill_manual(values= colors) +
               labs(x = '\nLife expectancy at age', y = 'Difference in years\n') +
               geom_hline(yintercept = 0, size = 0.2, alpha = 0.3, linetype = 2) +
               theme(axis.text.x = element_text(angle = 40, hjust = 1),
               legend.position="top",
               legend.title=element_blank()) +
               ylim(ylim_low, ylim_hi)
    return(p)
}


# texreg extract inla model
extract.inla = function(model, include.dic = FALSE,
                        include.waic = FALSE, ...) {

    fixed =  model$summary.fixed
    contrib = inla.contrib.sd(model)$hyper

    coefnames = c(rownames(fixed), rownames(contrib))
    coef = c(fixed[, "mean"], contrib[, "mean"])
    ci.low = c(fixed[, "0.025quant"], contrib[, "2.5%"])
    ci.up = c(fixed[, "0.975quant"], contrib[, "97.5%"])

    gof = numeric()
    gof.names = character()
    gof.decimal = logical()

      if (include.dic == TRUE) {
          gof = c(gof, model$dic$dic)
          gof.names = c(gof.names, "DIC")
          gof.decimal = c(gof.decimal, FALSE)
      }
      if (include.waic == TRUE) {
          gof = c(gof, model$waic$waic)
          gof.names = c(gof.names, "WAIC")
          gof.decimal = c(gof.decimal, FALSE)
      }

    tr = createTexreg(
                      coef.names = coefnames,
                      coef = coef,
                      ci.low = ci.low,
                      ci.up = ci.up,
                      gof.names = gof.names,
                      gof = gof,
                      gof.decimal = gof.decimal
    )
}

setMethod("extract", signature = className("inla", "inla"),
          definition = extract.inla)
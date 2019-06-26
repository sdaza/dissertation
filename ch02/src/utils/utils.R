##########################################
# CDC mortality - income Mobility paper
# utilities
# author: sebastian daza
##########################################


# libraries
library(sdazar)
library(ggplot2)
library(INLA)
library(texreg)

# redefine table base function
table = function (...) base::table(..., useNA = 'ifany')

# function to check counties
checkCounties = function(dataA, dataB, states, county_var) {

    checkCountiesA = list()
    checkCountiesB = list()

    for (i in seq_along(vstates)) {
        a = dataA[state == vstates[i], .N, by = county_var][
                  order(get(county_var)), get(county_var)]
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
zscores  <- function(dat, variables) {
    for (i in 1:length(variables)) {
        dat[, paste0("z_", variables[i]) := scale(get(variables[i]))]
    }
    return(dat)
}

# function to check counties
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

# plot distribution of fixed-effects
plot_fixed_coeff = function(model, coeff, coeff_labels=NULL, ylimits=NULL,
                            exponential=FALSE,
                            colors=c('#0072B21', '#D55E00', '#a1d99b')) {

    mf = dcast(data.table(melt(model$marginals.fixed)), Var1+L1 ~ Var2, value.var = 'value')
    mf = mf[L1 %in% coeff]

    if (is.null(coeff_labels)) {
        coeff_labels = coeff
    }
    mf[, L1 := factor(L1, levels=coeff, labels=coeff_labels)]
    if (exponential) {
      mf[, x := exp(x)]
    }

    p = ggplot(mf,aes(x=x,y=y, group=L1)) +
        geom_line(alpha=0.1) +
        geom_vline(xintercept=ifelse(exponential, 1, 0), color='gray', linetype=3) +
        geom_area(aes(fill=L1), alpha=0.3) +
        xlab('\nPosterior distribution of coefficients') +
        ylab('Density\n') +
        theme_classic() +
        scale_color_manual(values=colors) +
        scale_fill_manual(values=colors) +
        theme(axis.text.x = element_text(angle = 0, hjust = 1),
              legend.position="top",
              legend.title=element_blank()) +
        if (!is.null(ylimits)) {scale_x_continuous(limits=ylimits)}
    return(p)
}

# plot random effect distributions
plot_random_dist = function(model, terms, term_labels=NULL,
                            ylimits=NULL,
                            exponential=FALSE,
                            colors=c('#0072B2', '#D55E00', '#a1d99b')) {

    output = list()

    mh = model$marginals.hyperpar

    regex = paste0(paste0(terms, '$'), collapse='|')
    ef = grep(regex, names(mh), value=TRUE)
    for (i in ef) {
        if (exponential) {
           tp = inla.tmarginal(function(x) exp(1/sqrt(x)),
           mh[[i]])
      } else {
          tp = bri.hyper.sd(mh[[i]])
      }

      output[[i]] = data.table(term = i, tp)

    }

    mr = rbindlist(output)

    if(is.null(term_labels)) {
     term_labels = terms
    }

    mr[, term := factor(term, levels=ef, labels=term_labels)]

    p = ggplot(mr,aes(x=x,y=y, group=term)) +
        geom_line(alpha=0.1) +
        geom_vline(xintercept=ifelse(exponential, 1, 0), color='gray', linetype=3) +
        geom_area(aes(fill=term), alpha=0.3) +
        xlab('\nPosterior distribution of standard deviation') +
        ylab('Density\n') +
        theme_classic() +
        scale_color_manual(values=colors) +
        scale_fill_manual(values=colors) +
        theme(axis.text.x = element_text(angle = 0, hjust = 1),
              legend.position="top",
              legend.title=element_blank()) +
        if (!is.null(ylimits)) {scale_x_continuous(limits=ylimits)}
    return(suppressWarnings(print(p)))
}

# plot_random_effects_sim
get_coeff_fixed_random_effect = function(sim_data, fixed, random, prob = 0.95) {

    if (length(fixed) != length(random)) {
        stop('Fixed and random effects should have same length')
    }

    lprob = (1-prob)/2
    hprob = 1-lprob

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
             vrandom = selected_names[grep(lfixed, selected_names, invert=TRUE)]
             vfixed = selected_names[grep(lfixed, selected_names, invert=FALSE)]
             #print(sim_data[[i]]$latent[vfixed,])
             #print(sim_data[[i]]$latent[vrandom,])
             values[[i]] =  sim_data[[i]]$latent[vfixed,] + sim_data[[i]]$latent[vrandom,]
             #print(values[i])
        }

        sim_coeff = do.call(cbind, values)

        outputs[[v]] = data.table(
            id = seq_along(vrandom),
            m = apply(sim_coeff, 1, median),
            lo = apply(sim_coeff, 1, function(x) quantile(x, probs=lprob)),
            hi = apply(sim_coeff, 1, function(x) quantile(x, probs=hprob)))

    }

    if (length(fixed)==1) {

        moutput = outputs[[1]]
        t = nrow(moutput)
        moutput[, name := rep(random, t)]

    } else {
        t = nrow(outputs[[1]])
        moutput = rbindlist(outputs)
        moutput[, id ]
        moutput[, name := rep(random, each=t)]


    }

    return(moutput)
}

plot_random_fixed_effects_sim = function(sim_data, fixed=NULL, random=NULL,
                                         prob=0.95, sorted=FALSE, exponential=FALSE,
                                         xlabel='x', ylabel='y',
                                         x_labels=NULL,
                                         effects_labels=NULL) {

    dts = get_coeff_fixed_random_effect(sim_data, fixed=fixed, random=random,
                                        prob=prob)

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
        dts[, c('m', 'lo', 'hi') := lapply(.SD, exp), .SDcols=c('m', 'lo', 'hi')]

    }

    dts[, id := factor(id, levels=id_levels, labels=x_labels)]
    dts[, name := factor(name, levels=name_levels, labels=effects_labels)]
    num_eff = length(unique(dts$name))

    if (num_eff>1) {
         p = ggplot(dts, aes(x=id, y=m)) +
            geom_ribbon(aes(ymin=lo, ymax=hi, group=name, fill=name), alpha=0.3)  +
            geom_point(aes(color=name), size=0.9, alpha=0.3)  +
            geom_line(aes(color=name, group=name), size = 0.4, alpha=0.3)  +
            theme_classic() +
            scale_color_manual(values=c("#0072B2", "#D55E00")) +
            scale_fill_manual(values=c("#0072B2", "#D55E00")) +
            labs(x=xlabel, y=ylabel) +
            geom_hline(yintercept = ifelse(exponential, 1, 0),  size = 0.2, alpha = 0.3, linetype = 2) +
            theme(axis.text.x = element_text(angle = 40, hjust = 1),
            legend.position="top",
            legend.title=element_blank())

    }

    else if (num_eff==1) {
        p = ggplot(dts, aes(x=id, y=m)) +
            geom_ribbon(aes(ymin=lo, ymax=hi),  group=1, fill = '#a6bddb', alpha=0.3)  +
            geom_point(size=0.9, color='#e34a33', alpha=0.3)  +
            geom_line(color='#2b8cbe', size=0.4, group=1, alpha=0.3)  +
            theme_classic() +

            labs(x=xlabel, y=ylabel) +
            geom_hline(yintercept = ifelse(exponential, 1, 0),  size = 0.2, alpha = 0.3, linetype = 2) +
            theme(axis.text.x = element_text(angle = 40, hjust = 1))
          }

    return(p)
}

# get distribution fixed - random effects
get_dist_fixed_random_effect = function(sim_data, fixed, random) {

    if (length(fixed) != length(random)) {
        stop('Fixed and random effects should have same length')
    }

    nsim = length(sim_data)
    sim_names = rownames(sim_data[[1]]$latent)

    output = list()

    for (v in seq_along(fixed)) {

        lfixed  = paste0('^', fixed[v])
        lrandom = paste0('^', random[v])

        regex = paste0(c(lfixed, lrandom), collapse ='|')
        selected_names = grep(regex, sim_names, value = TRUE)

        values = list()
            for (i in 1:nsim) {
             vrandom = selected_names[grep(lfixed, selected_names, invert=TRUE)]
             vfixed = selected_names[grep(lfixed, selected_names, invert=FALSE)]
             #print(sim_data[[i]]$latent[vfixed,])
             #print(sim_data[[i]]$latent[vrandom,])
             values[[i]] =  sim_data[[i]]$latent[vfixed,] + sim_data[[i]]$latent[vrandom,]
             #print(values[i])
        }
        t = do.call(cbind, values)
        output[[v]] = data.table(rf=rownames(t), t)
      }
        return(rbindlist(output))
  }

# plot distribution of fixed effects using simulation
get_fixed_sim = function(sim_list,
    coeff=NULL, # max 2
    coeff_labels=NULL,
    groups_labels=NULL) {

  f = data.table()
  nsim = length(sim_list[[i]])

  for (i in 1:length(sim_list)) {

    print(paste0('::::: Model ', i, ' :::::'))

    output = list()

    for (j in 1:nsim) {
        d = sim_list[[i]][[j]]$latent
        output[[j]] = data.table('group'=groups_labels[i],
                  d[coeff[1],],
                  d[coeff[2],])
    }
    f = rbind(f,rbindlist(output))
  }

  setnames(f, names(f), c('group', coeff))

  # additional transformations
  f[, group := factor(group, levels=rev(groups_labels))]
  t = data.table::melt(f, id.vars='group', variable.name='effect')
  t[, effect := factor(effect, levels=coeff, labels=coeff_labels)]

  return(t)

  }


plot_fixed_sim = function(data,
    exponential=TRUE) {

    d = copy(data)

    if (exponential) {  d[, value := exp(value)]}

    p = ggplot(d, aes(y=group)) +
      geom_density_ridges(aes(x = value, fill = effect),
               alpha = 0.3, color = "white", scale = 0.8) +
      xlab('\nPosterior distribution') +
      ylab('') +
      theme_classic() +
      scale_fill_cyclical(values=c('#0072B2', '#D55E00'), guide= 'legend', name='') +
      geom_vline(xintercept = ifelse(exponential, 1, 0), size=0.2, alpha=0.3, linetype=2) +
      theme(legend.position = 'top')

      return(p)
  }

# plot distributions random-fixed sim
plot_dist_fixed_random_sim = function(sim_data, fixed, random,
  names_random=NULL, names_group=NULL, exponential=TRUE) {

    if (length(fixed) != length(random)) {
        stop('Fixed and random effects should have same length')
    }

    if (is.null(names_random)) {
      names_random = random
    }
    if (is.null(names_group)) {
      names_group = fixed
    }

    mf = get_dist_fixed_random_effect(sim_data,
      fixed=fixed,
      random=random)

    mf = melt(mf, id.vars='rf')
    #print(mf)
    mf[, cause := as.numeric(str_extract_all(rf, "[0-9]+"))]
    mf[, effect := str_extract_all(rf, paste0(random, collapse ='|'))]
    if (exponential) {
      mf[, value := exp(value)]
    }

    levels = rev(unique(sort(mf$cause)))
    labelsc = rev(names_group)
    mf[, cause := factor(cause, levels=levels, labels=labelsc)]
    levelse = unique(mf$effect)
    mf[, effect := factor(effect, levels=levelse, labels=names_random)]

    p = ggplot(mf, aes(y=cause)) +
      geom_density_ridges(aes(x = value, fill = effect),
               alpha = 0.3, color = "white", scale = 0.8) +
      xlab('\nPosterior distribution') +
      ylab('') +
      theme_classic() +
      scale_fill_cyclical(values=c('#0072B2', '#D55E00'), guide= 'legend', name='') +
      geom_vline(xintercept = ifelse(exponential, 1, 0),
        size=0.2, alpha=0.3, linetype=2) +
      theme(legend.position = 'top')

    return(p)
  }

# plot random effects
plot_random_effects = function(model, RE,
                              RE_labels = NULL,
                              x_labels = NULL,
                              xlabel='x', ylabel='y',
                              sorted=FALSE,
                              exponential=FALSE) {

    nvars = length(RE)

    rand = list()

    if (nvars>1) {
        for (i in 1:nvars) {
          ms = model$summary.random[[RE[i]]]
          rand[[i]] = data.table(RE_name=RE[i], ID=rownames(ms), ms)
        }

        len_RE = list()

        for (h in 1:nvars) {
          len_RE[[h]] = nrow(rand[[h]])
        }

        if (length(unique(unlist(len_RE)))>1) {
          stop('Random effects should have same length!')
        }
        df = rbindlist(rand)

    }
    else if (nvars==1) {
           ms = model$summary.random[[RE]]
           df = data.table(RE_name=RE, ID=rownames(ms), ms)
    }

    if (sorted) { setorder(df, '0.5quant') }

    levels = unique(df$ID)

    if (!is.null(x_labels)) {
        x_labels = x_labels[as.numeric(levels)]
        df[, ID := factor(ID, levels = levels, labels=x_labels)]
    } else {
        df[, ID := factor(ID, levels = levels)]
    }

    x = df$'ID'

    if (exponential) {
        m =  exp(df$'0.5quant')
        lo = exp(df$'0.025quant')
        hi = exp(df$'0.975quant')
    }
    else {
        m =  df$'0.5quant'
        lo = df$'0.025quant'
        hi = df$'0.975quant'
    }

    dat = data.table(RE_name=df$RE_name, x, m, lo, hi)

    if (is.null(RE_labels)) {
      dat[, RE_name := factor(RE_name, levels=RE)]
    }
    else {
      dat[, RE_name := factor(RE_name, levels = RE, labels = RE_labels)]
    }

    if (nvars==1) {
      p = ggplot(dat, aes(x=x, y=m)) +
          geom_ribbon(aes(ymin = lo, ymax = hi), group=1, fill = '#a6bddb', alpha=0.4)  +
          geom_point(size=0.7, color='#e34a33', alpha=0.4)  +
          geom_line(color='#2b8cbe', size=0.4, group=1)  +
          theme_classic() +
          labs(x=xlabel, y=ylabel) +
          geom_hline(yintercept = ifelse(exponential, 1, 0),  size=0.2, alpha = 0.3, linetype=2) +
          theme(axis.text.x = element_text(angle=60, hjust=1))
    }
    else if (nvars>1) {
      p = ggplot(dat, aes(x=x, y=m, fill=RE_name)) +
          geom_ribbon(aes(ymin=lo, ymax=hi, group=RE_name, fill=RE_name), alpha=0.4)  +
          geom_point(aes(color=RE_name), size=0.7, alpha=0.4)  +
          geom_line(aes(color=RE_name, group=RE_name), size = 0.4)  +
          theme_classic() +
          scale_color_manual(values=c("#0072B2", "#D55E00")) +
          scale_fill_manual(values=c("#0072B2", "#D55E00")) +
          labs(x=xlabel, y=ylabel) +
          geom_hline(yintercept = ifelse(exponential, 1, 0),  size = 0.2, alpha = 0.3, linetype = 2) +
          theme(axis.text.x = element_text(angle = 40, hjust = 1),
               legend.position="top",
               legend.title=element_blank())
    }
    return(p)
}



# plot predicted p-values
hist_pred_pvalues = function(model, outcome, breaks = 10) {
    output = NA

    for (i in (1:length(outcome))) {
        output[i] = inla.pmarginal(q=outcome[i],
                                   marginal = model$marginals.fitted.values[[i]])
    }

    hist(output, main="",
         breaks = breaks,
         xlab="Posterior predictive p-value")
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
                      fixed=NULL,
                      random=NULL,
                      value_matrix=NULL,
                      age_interval=5,
                      ax0 = 0.3,
                      axv=0.5) {

  nsim = length(sim_data)
  sim_names = rownames(sim_data[[1]]$latent)

  lfixed  = c(paste0('^', fixed), '(Intercept)')
  lrandom = paste0('^', random, ':')
  regex = paste0(c(lfixed, lrandom), collapse ='|')
  selected_names = grep(regex, sim_names, value = TRUE)
  # print(selected_names)

  output = list()

  for (i in 1:nsim) {

      d = sim_data[[i]]$latent
      d = data.table(values = d[selected_names,])
      d = transpose(d)
      setnames(d, selected_names)

      d = melt(d, measure=patterns(lrandom),
               value.name=random, variable.name='age_group')
      setnames(d, '(Intercept)', 'constant')

      # compute rates
      for (h in 1:nrow(value_matrix)) {
            d[, (rownames(value_matrix)[h]) :=
              exp(as.matrix(d[, c('constant', fixed, random), with=FALSE]) %*% value_matrix[h,])]
        }

      # print(d)

      dt = data.table(age=d$age_group)

      # life table parameters
      n = rep(age_interval, nrow(d)) # interval
      n[length(n)] = NA
      ax = rep(axv, nrow(d))
      ax[1] = ax0
      ax[length(ax)] = NA

      for (j in rownames(value_matrix)) {
            mx = d[, j, with=FALSE][[1]]
            dt[, (j) := fmsb::lifetable2(mx, ax=ax, n=n)$ex]
      }
      output[[i]] = dt
  }
      return(rbindlist(output))
}


# plot le counterfactuals
plot_le_counterfactuals = function(dt, relative=FALSE,
  x_labels=NULL,
  xlabel='\nLife expectancy at age',
  ylabel='Difference in years\n',
  name_groups = c('Mobility', 'Gini'),
  prob = 0.95) {

  if (relative==TRUE) {
    ylabel='Relative difference\n'
  }
  lprob = (1-prob)/2
  hprob = 1-lprob

  if (relative) {
    dt[, mob := (mob_1 - mob_0)/mob_0]
    dt[, gini := (gini_1 - gini_0)/gini_0]
  }
  else {
  dt[, mob := mob_1 - mob_0]
  dt[, gini := gini_1 - gini_0]
  }

  m = melt(dt, id.vars = 'age',
           measure.vars = c('mob', 'gini'))

  if (!is.null(name_groups)) {
    m[, variable := factor(variable, labels=name_groups)]
  }

  if (!is.null(x_labels)) {
    m[, age := factor(age, labels = x_labels)]
  }

  s = m[, .(m=median(value),
        lo=quantile(value, lprob),
        hi=quantile(value, hprob)),
        by = .(age, variable)]

  p = ggplot(s, aes(x=age, y=m)) +
          geom_ribbon(aes(ymin=lo, ymax=hi, group=variable, fill=variable), alpha=0.3)  +
          geom_point(aes(color=variable), size=0.9, alpha=0.3)  +
          geom_line(aes(color=variable, group=variable), size = 0.4, alpha=0.3)  +
          theme_classic() +
          scale_color_manual(values=c("#0072B2", "#D55E00")) +
          scale_fill_manual(values=c("#0072B2", "#D55E00")) +
          labs(x=xlabel, y=ylabel) +
          geom_hline(yintercept = 0,  size = 0.2, alpha = 0.3, linetype = 2) +
          theme(axis.text.x = element_text(angle = 40, hjust = 1),
          legend.position="top",
          legend.title=element_blank())

    return(p)
}

# texreg extract inla model
extract.inla <- function(model, include.dic = FALSE,
                         include.waic = FALSE, ...) {

    fixed <-  model$summary.fixed
    contrib <- inla.contrib.sd(model)$hyper

    coefnames <- c(rownames(fixed), rownames(contrib))
    coef <- c(fixed[, "mean"], contrib[, "mean"])
    ci.low <- c(fixed[, "0.025quant"], contrib[, "2.5%"])
    ci.up <- c(fixed[, "0.975quant"], contrib[, "97.5%"])

    gof <- numeric()
    gof.names <- character()
    gof.decimal <- logical()
      if (include.dic == TRUE) {
        gof <- c(gof, model$dic$dic)
        gof.names <- c(gof.names, "DIC")
        gof.decimal <- c(gof.decimal, FALSE)
      }
      if (include.waic == TRUE) {
        gof <- c(gof, model$waic$waic)
        gof.names <- c(gof.names, "WAIC")
        gof.decimal <- c(gof.decimal, FALSE)
      }

    tr <- createTexreg(
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
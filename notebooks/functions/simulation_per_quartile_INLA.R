# function to simulate predicted values
simulate_predictions_per_quartile = function(model, data, contrast, nsim = 1000) {
    
    # simulate posterior distribution
    simData <- inla.posterior.sample(n=nsim, result=model)
    
    # define matrix model
    f = formula(paste0('~ ', paste0(names(data), collapse = ' + ')))
    X = model.matrix(f, dat = data)
    N = nrow(data)
    Ysim = matrix(nrow = N, ncol = nsim) 
    
    # loop
    for (i in 1:nsim) {
        rnames <- rownames(simData[[i]]$latent)
        #rndQinc <- simData[[i]]$latent[grep('income_qr', rnames)]
        #rndIncCoef <- simData[[i]]$latent[grep(random, rnames)]
        coefNames <- rownames(model$summary.fixed)
        Betas = simData[[i]]$latent[-grep("^Pred|^state|^obs", rnames)]
        names(Betas) = coefNames
        mt = matrix(rep(Betas, N), ncol = length(coefNames), nrow = N, byrow = TRUE)
        for (j in 1:N) {
            Ysim[j, i] <- X[j, ] %*% mt[j,]
        }
    }
    
    t = data.table(Ysim)
    t[, (contrast) := rep(c(0.0, 1.0))]
    return(melt(t, id.vars = contrast))
}
##############################
# generative model income mobility and mortality
# microsim
# author: sebastian daza
##############################


library(data.table)
library(metafor)
library(texreg)
library(ggplot2)
library(patchwork)

source("src/utils.R")

# read data
path = "models/MobHealthRecycling/output/verification/microsimulation/"
# path = "models/MobHealthRecycling/output/verification/testing/"

p = readMultipleFiles("parameters", path, remove_files = TRUE)
e = readMultipleFiles("environment", path, remove_files = TRUE)

nrow(e)

# create plots of differences
iterations = list(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11,12))
title = c("No residential mobility and no uncertainty", 
        "No residential mobility and uncertainty", 
        "Random residential mobility and no uncertainty", 
        "Random residential mobility and uncertainty",
        "Segregation and no uncertainty",
        "Segregation and uncertainty")

plots = list()

for (i in seq_along(iterations)) {
    iter = iterations[[i]]
    t = copy(e[iteration %in% iter])
    
    nsi = mean(t$nsi) 
    replicates = max(t$replicate)
    rank_slope = mean(t$rank_slope)
    
    v = t[iteration == iter[2], le] - t[iteration == iter[1], le]
    plots[[i]] = ggplot(data.frame(v), aes(x=v)) + geom_histogram(bins = 10, color="black", fill="white") +
        labs(x = "Difference LE", y  = "Frequency",
            title = paste0(title[i]),
            subtitle = paste0("Mean = ", round(mean(v), 2), ",  CI = [", round(quantile(v, 0.025), 2), ";", round(quantile(v, 0.975), 2), "]"), 
            caption = paste0("Rank-rank slope = ", round(rank_slope, 2), ", NSI = ", round(nsi, 2), ", Replicates = ", replicates)) +
        theme_minimal() + theme(plot.margin = margin(0.1, 0.5, 0.5, 0.7, "cm"))
}

savepdf("output/plots/microsimulation", 25, 30)
wrap_plots(plots, ncol = 2)
dev.off()
 
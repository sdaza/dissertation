##############################
# generative model income mobility and mortality
# verify population dynamics
# author: sebastian daza
##############################


library(data.table)
library(ggplot2)
source("src/utils.R")
path = "models/MobHealthRecycling/output/population/"

# read files
par = readMultipleFiles("parameters", path)
dat = readMultipleFiles("environ", path)

sel = par[, .(iteration, replicate, counties, people_per_county, move_random,
    move_threshold, max_generation)]
dat = merge(dat, sel, by = c("iteration", "replicate"))

summary(dat$population)

# read RDS
saveRDS(dat, "output/data/population.rds")
dat = readRDS("output/data/population.rds")

# population
savepdf("output/plots/population")
ggplot(dat, aes(time, population, group = replicate)) + geom_line( alpha = 0.3, size = 0.1) +
    labs(x = "\nYear", y = "Population\n")  +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
    theme_minimal()
dev.off()

# age of death distribution
m = readMultipleFiles("mort", path)
saveRDS(m, "output/data/mortality.rds")
m = readRDS("output/data/mortality.rds")

savepdf("output/plots/age_death")
ggplot(m, aes(x = age)) + geom_histogram(binwidth = 1.2, color = "black", fill="white") +
    labs(x = "\nAge of death", y = "Frequency\n") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
theme_minimal()
dev.off()

# average number of kids
savepdf("output/plots/num_kids")
ggplot(m, aes(x = nkids)) + geom_histogram(binwidth = 1, color = "black", fill="white") +
    labs(x = "\nNumber of kids", y = "Frequency\n") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    geom_vline(aes(xintercept=mean(nkids)),
            color="gray", linetype="dashed", size = 0.4) +
theme_minimal()
dev.off()

# life expectancy
savepdf("output/plots/le")
ggplot(unique(dat[le > 0, .(le)]), aes(x = le)) + geom_histogram(color = "black", fill="white") +
    labs(x = "\nLife expectancy", y = "Frequency\n") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
    geom_vline(aes(xintercept=mean(le)),
            color="gray", linetype="dashed", size = 0.4) +
theme_minimal()
dev.off()
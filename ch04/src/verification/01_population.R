##############################
# generative model income mobility and mortality
# verify population dynamics
# author: sebastian daza
##############################


library(data.table)
library(ggplot2)
source("src/utils.R")
path = "models/MobHealthRecycling/output/"

# read files
# par = readMultipleFiles("parameters", path)
# dat = readMultipleFiles("environ", path)

# sel = par[, .(iteration, replicate, counties, people_per_county, move_random, move_threshold, max_generation)]

# dat = merge(dat, sel, by = c("iteration", "replicate"))
# temp = dat[, .(.N, avg_nsi = mean(nsi, na.rm = TRUE), min = min(nsi, na.rm = TRUE), max = max(nsi, na.rm = TRUE),
#         avg_time = mean(time), avg_pop = mean(population)),
#     .(iteration, move_random, move_threshold, max_generation)]

# saveRDS(dat, "output/data/nsi_21_25.rds")

# read RDS
dat = readRDS("output/data/nsi_21_25.rds")

savepdf("output/plots/population")
ggplot(dat[iteration == 2], aes(time, population, group = replicate)) + geom_line( alpha = 0.2, size = 0.1) +
    labs(x = "\nYear", y = "Population\n")  +
    theme_minimal()
dev.off()

savepdf("output/plots/le")
ggplot(dat[iteration == 2], aes(time, le, group = replicate)) + geom_line( alpha = 0.2, size = 0.1) +
    labs(x = "\nYear", y = "Life expectancy\n")  +
    theme_minimal()
dev.off()
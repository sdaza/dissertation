##############################
# generative model income mobility and mortality
# verify segregation
# author: sebastian daza
##############################


library(data.table)
library(ggplot2)
source("src/utils.R")
path = "models/MobHealthRecycling/output/"

# read files
par = readMultipleFiles("parameters", path)
dat = readMultipleFiles("environ", path)

sel = par[, .(iteration, replicate, counties, people_per_county, move_random, move_threshold, max_generation)]

dat = merge(dat, sel, by = c("iteration", "replicate"))
temp = dat[, .(.N, avg_nsi = mean(nsi, na.rm = TRUE), min = min(nsi, na.rm = TRUE), max = max(nsi, na.rm = TRUE), 
        avg_time = mean(time), avg_pop = mean(population)), 
    .(iteration, move_random, move_threshold, max_generation)]

saveRDS(dat, "output/data/nsi_21_25.rds")
dat = readRDS("output/data/nsi_21_25.rds")


savepdf("output/plots/nsi_21")
ggplot(dat, aes(time, nsi, group = replicate, color = iteration)) + geom_line( alpha = 0.4, size = 0.1) + 
    labs(x = "\
    nYear", y = "Neighborhood sorting index (NSI)\n")  + 
    theme_minimal()
dev.off()

savepdf("output/plots/nsi_25")
ggplot(dat[iteration == 2], aes(time, nsi, group = replicate)) + geom_line( alpha = 0.4, size = 0.1) + 
    labs(x = "\nYear", y = "Neighborhood sorting index (NSI)\n")  + 
    theme_minimal()
dev.off()


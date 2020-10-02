##############################
# generative model income mobility and mortality
# verify segregation
# author: sebastian daza
##############################


library(data.table)
library(ggplot2)
source("src/utils.R")
path = "models/MobHealthRecycling/output/"

# # read files
# par = readMultipleFiles("parameters", path)
# dat = readMultipleFiles("environ", path)

# sel = par[, .(iteration, replicate, counties, people_per_county, move_random, move_threshold, max_generation)]

# dat = merge(dat, sel, by = c("iteration", "replicate"))
# temp = dat[, .(.N, avg_nsi = mean(nsi, na.rm = TRUE), min = min(nsi, na.rm = TRUE), max = max(nsi, na.rm = TRUE),
#         avg_time = mean(time), avg_pop = mean(population)),
#     .(iteration, move_random, move_threshold, max_generation)]

# read RDS
# saveRDS(dat, "output/data/segregation.rds")
dat = readRDS("output/data/segregation.rds")

tab = dat[!is.na(nsi), .(nsi = mean(nsi), threshold = max(move_threshold), sd = sd(nsi)), iteration]
tab[, prop := sd / nsi]
tab

# parameters
unique(dat[, .(iteration, move_threshold)])

# create plots

plot_names = c("random", "19", "21", "22", "23", "25")

for (i in seq_along(plot_names)) {
    savepdf(paste0("output/plots/nsi_", plot_names[i]))
    print(
    ggplot(dat[!is.na(nsi) & iteration == i], aes(time, nsi, group = replicate)) + geom_line( alpha = 0.20, size = 0.1) +
        labs(x = "\nYear", y = "NSI\n")  +
        scale_y_continuous(limits = c(0, 1), breaks = scales::pretty_breaks(n = 6)) +
        scale_x_continuous(limits = c(0, 1550), breaks = scales::pretty_breaks(n = 8)) +
        theme_minimal()
    )
    dev.off()
}
##############################
# generative model income mobility and mortality
# verify segregation
# author: sebastian daza
##############################


library(data.table)
library(ggplot2)
source("src/utils.R")
path = "models/MobHealthRecycling/output/verification/segregation/"

# create data tables
m = readMultipleFiles("mortality", path)
p = readMultipleFiles("parameter", path)
e = readMultipleFiles("environ", path)

# save files
saveRDSFile(m, "output/data/verification/segregation/mortality.rds", FALSE)
saveRDSFile(p, "output/data/verification/segregation/parameter.rds", FALSE)
saveRDSFile(e, "output/data/verification/segregation/environment.rds", FALSE)

# remove csv files
files = list.files(path)
sapply(paste0(path, files), unlink)
 
# initial processing
sel = p[, .(iteration, replicate, counties, people_per_county, move_random,
    move_rate, move_threshold, max_generation)]
dat = merge(e[!is.na(nsi)], sel, by = c("iteration", "replicate"))
dat

dat[!is.infinite(nsi), .(mean(nsi)), iteration]

# read RDS
saveRDS(dat, "output/data/segregation.rds")
dat = readRDS("output/data/segregation.rds")

tab = dat[!is.na(nsi), .(nsi = mean(nsi), threshold = max(move_threshold),
    sd = sd(nsi)), iteration]
tab[, prop := sd / nsi]

# parameters
sp = unique(dat[, .(iteration, move_threshold)])
v = as.character(round(sort(sp[move_threshold > 0, move_threshold]) * 100))

# create plots
plot_names = c("random", v)

for (i in seq_along(plot_names)) {
    savepdf(paste0("output/plots/nsi_", plot_names[i]))
    print(
    ggplot(e[!is.na(nsi) & iteration == i], aes(time, nsi, group = replicate)) + geom_line( alpha = 0.25, size = 0.1) +
        labs(x = "\nYear", y = "NSI\n")  +
        scale_y_continuous(limits = c(0, 1), breaks = scales::pretty_breaks(n = 6)) +
        scale_x_continuous(limits = c(0, 1550), breaks = scales::pretty_breaks(n = 8)) +
        theme_minimal()
    )
    dev.off()
}


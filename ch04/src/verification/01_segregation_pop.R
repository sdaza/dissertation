# ##############################
# # generative model income mobility and mortality
# # verify segregation and population dynamic
# # author: sebastian daza
# ##############################


# library(data.table)
# library(ggplot2)
# source("src/utils.R")

# sim_path = "models/MobHealthRecycling/output/verification/segregation/"
# data_path = "output/data/verification/segregation/"
# plots_path = "output/plots/verification/segregation/"

# # read excel file
# m = fread(paste0(sim_path, "mortality.csv"))
# dim(m)

# e = fread(paste0(sim_path, "environment.csv"))
# p = fread(paste0(sim_path, "model_parameters.csv"))

# anyDuplicated(m[, al_id])
# anyDuplicated(m[, .(iteration, replicate,id)])

# # create data tables
# # m = readMultipleFiles("mortality", sim_path)
# # p = readMultipleFiles("parameter", sim_path)
# # e = readMultipleFiles("environ", sim_path)

# nrow(p)
# nrow(unique(m[, .(iteration, replicate)]))
# nrow(unique(e[, .(iteration, replicate)]))

# # save files
# overwrite = FALSE
# saveRDSFile(m, paste0(data_path, "mortality.rds"), overwrite)
# saveRDSFile(p, paste0(data_path, "parameter.rds"), overwrite)
# saveRDSFile(e, paste0(data_path, "environment.rds"), overwrite)

# # remove csv files
# files = list.files(sim_path)
# sapply(paste0(sim_path, files), unlink)
#  
# # initial processing
# sel = p[, .(iteration, replicate, counties, people_per_county, move_random,
#     move_rate, move_threshold, max_generation)]
# dat = merge(e[!is.na(nsi)], sel, by = c("iteration", "replicate"))


# # segregation
# tab = dat[!is.na(nsi), .(nsi = mean(nsi), threshold = max(move_threshold),
#     sd = sd(nsi)), iteration]
# tab[, prop := sd / nsi]

# # number of moves
# m[, .(moves = mean(nmoves), moves_kid = mean(nmoves_kid)), iteration]

# # parameters
# sp = unique(dat[, .(iteration, move_threshold)])
# v = as.character(round(sort(sp[move_threshold > 0, move_threshold]) * 100))

# # create plots
# plot_names = c("random", v)

# nsi_plots = list.files(plots_path, pattern = "+.pdf")
# sapply(paste0(plots_path, nsi_plots), unlink)

# for (i in seq_along(plot_names)) {
#     savepdf(paste0(plots_path, "nsi_", plot_names[i]))
#     print(
#     ggplot(e[!is.na(nsi) & time > 18 & iteration == i], aes(time, nsi, group = replicate)) + geom_line( alpha = 0.25, size = 0.1) +
#         labs(x = "\nYear", y = "NSI\n")  +
#         scale_y_continuous(limits = c(0, 1), breaks = scales::pretty_breaks(n = 6)) +
#         scale_x_continuous(limits = c(0, 1550), breaks = scales::pretty_breaks(n = 8)) +
#         theme_minimal()
#     )
#     dev.off()
# }


# # population
# e[, group := iteration * 100 + replicate]
# savepdf(paste0(plots_path, "population"))
# ggplot(e[iteration == 6], aes(time, population, group = group)) + geom_line( alpha = 0.3, size = 0.1) +
#     labs(x = "\nYear", y = "Population\n")  +
#     scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
#     scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
#     theme_minimal()
# dev.off()

# # age of death distribution

# savepdf(paste0(plots_path, "age_death"))
# ggplot(m, aes(x = age)) + geom_histogram(binwidth = 1.2, color = "black", fill="white") +
#     labs(x = "\nAge of death", y = "Frequency\n") +
#     scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
#     scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
# theme_minimal()
# dev.off()

# # average number of kids
# summary(m$nkids)

# savepdf(paste0(plots_path, "num_kids"))
# ggplot(m[iteration == 6], aes(x = nkids)) + geom_histogram(binwidth = 1, color = "black", fill="white") +
#     labs(x = "\nNumber of kids", y = "Frequency\n") +
#     scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
#     scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
#     geom_vline(aes(xintercept=mean(nkids)),
#             color="gray", linetype="dashed", size = 0.4) +
# theme_minimal()
# dev.off()

# # life expectancy
# savepdf(paste0(plots_path, "le"))
# ggplot(unique(dat[le > 0, .(le)]), aes(x = le)) + geom_histogram(color = "black", fill="white") +
#     labs(x = "\nLife expectancy", y = "Frequency\n") +
#     scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
#     scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
#     geom_vline(aes(xintercept=mean(le)),
#             color="gray", linetype="dashed", size = 0.4) +
# theme_minimal()
# dev.off()
##############################
# generative model income mobility and mortality
# microsim
# author: sebastian daza
##############################


library(data.table)
library(metafor)
library(texreg)

source("src/utils.R")

# read data
path = "models/MobHealthRecycling/output/verification/microsimulation/"
# path = "models/MobHealthRecycling/output/verification/testing/"


p = readMultipleFiles("parameters", path, remove_files = TRUE)
e = readMultipleFiles("environment", path, remove_files = TRUE)

nrow(e)



summary(e[iteration == 1, le])
summary(e[iteration == 2, le])

e[, mean(nsi), iteration]
t = e[iteration == 2, le] - e[iteration == 1, le]
hist(t)
summary(t)

quantile(t, 0.025)
quantile(t, 1 - 0.025)

t = e[iteration == 3, le] - e[iteration == 1, le]
hist(t, breaks = 15)
quantile(t, 0.025)
quantile(t, 1 - 0.025)

t = e[iteration == 5, le] - e[iteration == 4, le]
t = e[iteration == 6, le] - e[iteration == 4, le]
quantile(t, 0.025)
quantile(t, 1 - 0.025)

t = e[iteration == 8, le] - e[iteration == 7, le]
t = e[iteration == 9, le] - e[iteration == 7, le]
quantile(t, 0.025)
quantile(t, 1 - 0.025)

hist(t)

t = e[iteration == 5, le] - e[iteration == 4, le]
t = e[iteration == 6, le] - e[iteration == 4, le]

hist(t)
summary(t)
quantile(t, 0.025)
quantile(t, 1 - 0.025)



e[iteration == 6]

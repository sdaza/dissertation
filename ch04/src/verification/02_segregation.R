##############################
# generative model income mobility and mortality
# verify segregation
# author: sebastian daza
##############################


library(data.table)
source("src/utils.R")
path = "models/MobHealthRecycling/output/"

# read files
par = readMultipleFiles("parameters", path)
dat = readMultipleFiles("environ", path)

dat[, mean(nsi), iteration]
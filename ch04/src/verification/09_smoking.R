##############################
# generative model income mobility and mortality
# verify smoking distribution
# author: sebastian daza
##############################


library(data.table)
source("src/utils.R")
path = "models/MobHealthRecycling/output/verification/smoking/"

# read files
p = readMultipleFiles("parameters", path, remove_files = FALSE)
e = readMultipleFiles("environment", path, remove_files = FALSE)

parameters = c("smoking_rank_slope_exp_coeff")
setorderv(p, parameters)

p[, niteration := .GRP, by = parameters]
p[, nreplicate := 1:.N, by = niteration]
np = p[, c("iteration", "replicate", "niteration", "nreplicate", parameters), with = FALSE]

# create columns from arrays
vars = paste0("income", 1:5)
e = extractColumns(e, "prop_income_type",  vars)
vars = paste0("le", 1:5)
e = extractColumns(e, "le_income_type",  vars)
vars = paste0("smoking",  1:5)
e = extractColumns(e, "smoking_income_type",  vars)

e = merge(e, np, by = c("iteration", "replicate"))
setorderv(e, parameters)

mean(e[niteration == 1, smokers])
mean(e[niteration == 2, smokers])

mean(e[niteration == 1, smoking5])
mean(e[niteration == 2, smoking5])

mean(e[niteration == 1, smoking1])
mean(e[niteration == 2, smoking1])

a = mean(e[niteration == 1, le5])
b = mean(e[niteration == 2, le5])
a - b

a = mean(e[niteration == 1, le1])
b = mean(e[niteration == 2, le1])
a - b

library(data.table)

sw = readRDS("output/data/psid_sampling_weights.rds")
mm = readRDS("output/data/psid_data_ready_for_imputation.rds")

mm = mm[head_wife == 1]
length(unique(mm$pid))

test = merge(sw, mm, on = "pid")
dim(test)
names(test)
summary(test[, .(stratum, cluster, sweight)])

table(test[is.na(sweight), type])

library(data.table)

dat <- fread("/Users/sdaza/Google Drive/00Dissertation/Chapters/ch03/data/nlsy97/individual/nlsy97.csv")
setnames(dat, names(dat), tolower(names(dat)))

dim(dat)

save(dat, file = "/Users/sdaza/Google Drive/00Dissertation/Chapters/ch03/output/rdata/nlsy97/nsly97selection.Rdata")

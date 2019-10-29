###################################
# individual income mobility paper
# psid well-being data
# author: sebastian daza
# version: 0.01
###################################

library(haven)
library(data.table)
library(hash)

source("ch03/src/utils.R")


# wellbeing data
wb = data.table(read_stata('ch03/data/psid/wellbeing.dta'))
setnames(wb, names(wb), tolower(names(wb)))
setnames(wb, "wb16age", "age")
wb[, pid := er30001 * 1000 + er30002]
anyDuplicated(wb, by = "pid")

# respondents (identify newborns)\
data = data.table(read_stata('ch03/data/psid/respondents.dta'))
setnames(data, names(data), tolower(names(data)))

# unique id
data[, pid := er30001 * 1000 + er30002]
anyDuplicated(data[, pid])

# sequence of years
years = c(1968:1997, seq(1999, 2017, 2))

# sequence number
oldvars = c("er30021", "er30044", "er30068", "er30092", "er30118", "er30139", "er30161",
            "er30189", "er30218", "er30247", "er30284", "er30314", "er30344", "er30374",
            "er30400", "er30430", "er30464", "er30499", "er30536", "er30571", "er30607",
            "er30643", "er30690", "er30734", "er30807", "er33102", "er33202", "er33302",
            "er33402", "er33502", "er33602", "er33702", "er33802", "er33902", "er34002",
            "er34102", "er34202", "er34302", "er34502")

sn_vars = paste0("sn", years[-1])
renameColumns(data, hash(oldvars, sn_vars))

data[, sn1968 := NA]
sn_vars = c("sn1968", sn_vars)

# person number
setnames(data, "er30002", "pn")

# type of individual
oldvars = c("er30017", "er30040", "er30064", "er30088", "er30114", "er30135", "er30157",
            "er30185", "er30214", "er30243", "er30280", "er30310", "er30340", "er30370",
            "er30396", "er30426", "er30460", "er30495", "er30532", "er30567", "er30603",
            "er30639", "er30684", "er30728", "er30801", "er30862", "er33126", "er33282",
            "er33324", "er33436", "er33544", "er33635", "er33738", "er33846", "er33948",
            "er34043", "er34152", "er34266", "er34411", "er34648")

type_vars = paste0("type", years)
renameColumns(data, hash(oldvars, type_vars))

# long format
ldata = melt(data, id.vars = c('pid', 'pn'),
             measure = list(sn_vars, type_vars),
             value.name = c("sn", "type"),
             variable = "wave")

years_data = data.table(wave = 1:40, year = years)
ldata = merge(ldata, years_data, by = "wave")

setorder(ldata, pid, year)

ldata[, lag_sn := shift(sn), pid]
ldata[, lag_type := shift(type), pid]

table(ldata$lag_type)

ldata[type == 9]
table(ldata$lag_type)
nb = ldata[sn > 0 & lag_type == 9 & (pn %in% 30:169)]

setorder(nb, pid, year)
unb = unique(nb, by = "pid")
snb = unb[year > 1970 & year < 1986]
snb
table(snb$year)

tt = merge(snb, wb, by = "pid")
tt
table(tt$year)
table(snb$year)
##############################
# individual mobility and health
# get county data for NLSY97
# author: sebastian daza
# version 0.01
##############################

# library
library(data.table)
library(haven)

# read location files
loc = fread("ch03/data/nlsy97/location/Location_R16.csv")

setnames(loc, names(loc), tolower(names(loc)))
years = c(1997:2011, 2013)
renameColumns(loc, hash("r0000100", "id"))

# county
ovars = c("r1242900", "r2602200","r3927400","r5521300","r7283100",
          "s1609400", "s2072500","s5251600","s5451900","s7553400",
          "t0000300", "t2000200","t3621700", "t5221700","t6673800",
          "t8144300")
nvars = paste0("county", years)
renameColumns(loc, hash(ovars, nvars))

# state
ovars = c("r1243000","r2602300","r3927500","r5521400","r7283200","s1609500",
          "s2072600","s5251700","s5452000","s7553500","t0000400","t2000300",
          "t3621800","t5221800","t6673900","t8144400")
nvars = paste0("state", years)
renameColumns(loc, hash(ovars, nvars))

# long format
vars = lookvar(loc, "id|county|state")
loc = loc[, ..vars]

pattern_names = hash(
    "^county" = "county",
    "^state" = "state"
)


lloc = melt(
    data =loc,
    measure.vars = patterns(hash::keys(pattern_names)),
    value.name = hash::values(pattern_names),
    variable.name = "time"
)

lloc[, time := as.numeric(time)]
lloc[, coding := ifelse(time < 8, 1990, 2000)]

lloc = lloc[, lapply(.SD, replaceMissing)]

# create fips
lloc[, fips := state * 1000 + county]
lloc[, flag12  := 0]

length(unique(lloc$fips)) # 1645
length(unique(lloc$id)) # 8984

lloc = lloc[!is.na(fips)]
length(unique(lloc$id)) # 8984

lloc[, moves := rleid(fips), id][, moves := moves - 1]
lloc[id == sample(unique(lloc$id), 1)]

# test = lloc[, .(moves = max(moves)), id]
# table(test$moves)
# hist(test$moves)
# prop.table(table(test$moves > 0)) # 62% changes of county at least once


#####################
# get data at age 12
#####################

loc12 = fread("ch03/data/nlsy97/individual/age_12_variables/Age_12_variables_R16.csv")

setnames(loc12, names(loc12), tolower(names(loc12)))
setnames(loc12, names(loc12), c("id", "county", "state", "cbsa", "match"))

loc12 = loc12[, lapply(.SD, replaceMissing)]

loc12[, flag12 := 1]
loc12[, time := 0]

# loc12 = loc12[, .(id, county, state, flag12)]
loc12[, fips := state * 1000 + county]
loc12[, coding := 1990]

loc12 = loc12[, .(id, fips, flag12, coding, time)]
locwaves = lloc[, .(id, fips, flag12, coding, time)]
lloc = rbind(loc12, locwaves)

setorder(lloc, id, time)
ids = unique(lloc$id)
lloc[id == sample(ids, 1)]

# recode fips
lloc[coding == 1990 & fips == 12086, fips :=  12025]
lloc[coding == 2000 & fips == 12086, fips :=  12025]

# save rds
saveRDS(lloc, file = "ch03/output/data/nlsy97_location.rds")
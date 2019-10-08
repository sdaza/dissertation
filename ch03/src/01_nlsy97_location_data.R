##############################
# individual mobility and health
# get county data for NLSY97
# author: sebastian daza
# version 0.01
##############################

# library
library(data.table)
library(sdazar)
library(haven)

# read location files
loc = fread("ch03/data/nlsy97/location/Location_R16.csv")
names(loc)

setnames(loc, names(loc), tolower(names(loc)))

years = c(1997:2011, 2013)

setnames(loc, "r0000100", "id")

# county
ovars = c("r1242900", "r2602200","r3927400","r5521300","r7283100",
          "s1609400", "s2072500","s5251600","s5451900","s7553400",
          "t0000300", "t2000200","t3621700", "t5221700","t6673800",
         "t8144300")

nvars = paste0("county", years)
setnames(loc, ovars, nvars)
table(loc$county1997)

# state
ovars = c("r1243000","r2602300","r3927500","r5521400","r7283200","s1609500",
           "s2072600","s5251700","s5452000","s7553500","t0000400","t2000300",
           "t3621800","t5221800","t6673900","t8144400")

nvars = paste0("state", years)
setnames(loc, ovars, nvars)
table(loc$state1999)

vars = lookvar(loc, "id|county|state")
loc = loc[, vars, with = FALSE]
patt = c("^county", "^state")
vnames = c("county", "state")

lloc = melt(loc, measure.vars = patterns(patt),
      value.name = vnames,
      variable.name = "time")

lloc[, time := as.numeric(time)]
lloc[, coding := ifelse(time < 8, 1990, 2000)]

summary(lloc)
lloc[, county := ifelse(county < 0, NA, county)]
lloc[, state := ifelse(state < 0, NA, state)]

# create fips
lloc[, fips := state * 1000 + county]
lloc[, flag12  := 0]
summary(lloc$fips)

length(unique(lloc$fips)) # 1645
length(unique(lloc$id)) # 8984

lloc = lloc[!is.na(fips)]
length(unique(lloc$id)) # 8984
summary(lloc$fips)

lloc[, moves := rleid(fips), id][, moves := moves - 1]
lloc[id == sample(unique(lloc$id), 1)]
test = lloc[, .(moves = max(moves)), id]
table(test$moves)
hist(test$moves)
prop.table(table(test$moves > 0)) # 62% changes of county at least once


#####################
# get data at age 12
#####################

loc12 = fread("ch03/data/nlsy97/individual/Age_12_variables_R16.csv")

setnames(loc12, names(loc12), tolower(names(loc12)))
setnames(loc12, names(loc12), c("id", "county", "state", "cbsa", "match"))

loc12[, county := ifelse(county < 0, NA, county)]
loc12[, state := ifelse(state < 0, NA, state)]

loc12[, flag12 := 1]
loc12[, time := 0]
summary(loc12)

# loc12 = loc12[, .(id, county, state, flag12)]
loc12[, fips := state * 1000 + county]
loc12[, coding := 1990]

loc12 = loc12[, .(id, fips, flag12, coding, time)]
locwaves = lloc[, .(id, fips, flag12, coding, time)]
lloc = rbind(loc12, locwaves)

setorder(lloc, id, time)
ids = unique(lloc$id)
lloc[id == sample(ids, 1)]

# getting income mobility data
# get counties from chetty data and fix differences
c = data.table(read_dta("ch02/data/cty_full_covariates.dta"))

c = c[, .(cty, statename, county_name, gini99, e_rank_b, s_rank)]
cs = unique(c$cty)
length(cs)

# check codes and adjust according
cn90 = unique(lloc[coding == 1990, fips])
cn00 = unique(lloc[coding == 2000, fips])


# correct coding of fips
lloc[coding == 1990 & fips == 12086, fips :=  12025]
lloc[coding == 2000 & fips == 12086, fips :=  12025]

cn90 = unique(lloc[coding == 1990, fips])
cn00 = unique(lloc[coding == 2000, fips])

cn00[!cn00 %in% cs] # 36518 and 13912 weird
cn90[!cn90 %in% cs] # to transform?

setkey(lloc, fips)
setnames(c, "cty", "fips"); setkey(c, fips)

lloc = c[lloc]
summary(lloc)

countmis(lloc) # 2%
table(lloc$time)

saveRDS(lloc, file = "ch03/output/data/nlsy97_location.rd")

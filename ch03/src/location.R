##############################
# get county data for NLSY97
# author: sebastian daza
# version 0.01
##############################

# library
library(data.table)
library(sdazar)
library(haven)

# read location files
loc <- fread("/Users/sdaza/Documents/Workplace/Data/NLSY/NLSY97 data/Location/Location_R16.csv")
setnames(loc, names(loc), tolower(names(loc)))

years <- c(1997:2011, 2013)

setnames(loc, "r0000100", "id")

# county
ovars <- c("r1242900","r2602200","r3927400","r5521300","r7283100",
           "s1609400","s2072500","s5251600","s5451900","s7553400","t0000300",
           "t2000200","t3621700", "t5221700","t6673800","t8144300")

nvars <- paste0("county", years)
setnames(loc, ovars, nvars)

table(loc$county1997, useNA = "ifany")

# state
ovars <- c("r1243000","r2602300","r3927500","r5521400","r7283200","s1609500",
           "s2072600","s5251700","s5452000","s7553500","t0000400","t2000300",
           "t3621800","t5221800","t6673900","t8144400")

nvars <- paste0("state", years)
setnames(loc, ovars, nvars)
table(loc$state1999, useNA = "ifany")


vars <- lookvar(loc, "id|county|state")
loc <- loc[, vars, with = FALSE]
patt <- c("^county", "^state")
vnames <- c("county", "state")

lloc <- melt(loc, measure.vars = patterns(patt),
      value.name = vnames,
      variable.name = "time")

lloc[, time := as.numeric(time)]
lloc[, coding := ifelse(time < 8, 1990, 2000)]

summary(lloc)
lloc[, county := ifelse(county < 0, NA, county)]
lloc[, state := ifelse(state < 0, NA, state)]

# create fips
lloc[, fips := state * 1000 + county]
summary(lloc$fips)

length(unique(lloc$fips)) # 1645
length(unique(lloc$id)) # 8984

lloc <- lloc[!is.na(fips)]
length(unique(lloc$id)) # 8984
summary(lloc$fips)

lloc[, moves := rleid(fips), id][, moves := moves - 1]
lloc[id == sample(unique(lloc$id), 1)]
test <- lloc[, .(moves = max(moves)), id]
table(test$moves, useNA = "ifany")
hist(test$moves)
prop.table(table(test$moves > 0)) # 62% changes of county at least once
# the next question is when? before 18 years old
# next step, only capture changes before 18 years old

# check codes and adjust according
cn90 <- unique(lloc[coding == 1990, fips])
cn00 <- unique(lloc[coding == 2000, fips])

# get counties from chetty data and fix differences
c <- data.table(read_dta("/Users/sdaza/Google Drive/00Dissertation/Data/chetty/cty_full_covariates.dta"))
names(c)
c <- c[, .(cty, statename, county_name, gini99, e_rank_b, s_rank)]
cs <- unique(c$cty)
length(cs)

lloc[coding == 1990 & fips == 12086, fips :=  12025]
lloc[coding == 2000 & fips == 12086, fips :=  12025]

cn90 <- unique(lloc[coding == 1990, fips])
cn00 <- unique(lloc[coding == 2000, fips])
cn00[!cn00 %in% cs] # 36518 and 13912 weird
cn90[!cn90 %in% cs] # to transform?

setkey(lloc, fips)
setnames(c, "cty", "fips"); setkey(c, fips)

lloc <- c[lloc]
summary(lloc)

countmis(lloc) # 2%
lloc[id == sample(unique(lloc$id), 1)]

#####################
# get data age 12
#####################

loc12 <- fread("/Users/sdaza/Documents/Workplace/Data/NLSY/NLSY97 data/Age_12_variables/Age_12_variables_R16.csv")
setnames(loc12, names(loc12), tolower(names(loc12)))

setnames(loc12, names(loc12), c("id", "county", "state", "cbsa", "match"))
loc12

loc12[, county := ifelse(county < 0, NA, county)]
loc12[, state := ifelse(state < 0, NA, state)]

summary(loc12)
loc12 <- loc12[, .(id, county, state)]
loc12[, fips12 := state * 1000 + county]

cn90 <- unique(loc12$fips)
cn90[!cn90 %in% cs] # none
loc12 <- loc12[, .(id, fips12)]
setkey(loc12, id)
setkey(lloc, id)

lloc <- loc12[lloc]
lloc

# years
for (i in seq_along(years)) {
lloc[time == i, year := years[i]]
}

# save data
loc <- lloc
save(loc, file = "/Users/sdaza/Google Drive/00Dissertation/Chapters/ch03/data/nlsy97/location_nlsy97.Rdata")

# end



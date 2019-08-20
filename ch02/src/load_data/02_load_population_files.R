########################################
# CDC mortality - income mobility paper
# load population county data
# author: sebastian daza
########################################


# utils
source("src/utils/utils.R")

# libraries
library(readr)
library(ggthemes)

# set parameters for plots
options(repr.plot.width = 5, repr.plot.height = 4)

# read fips codes
codes = read_csv('data/fips_codes.csv')
codes = data.table(codes)
codes = codes[, c(1,2,3,6), with = FALSE]
setnames(codes, c("abr", "state", "county", "name"))
states = codes[, .N , by = .(abr, state)][, N := NULL]


# process population datafile

# read population data
start = c(1,5,7,9,12,14,15,16,17,19)
lc    = c(4,2,2,3,2,1,1,1,2,8)
end   = start + (lc-1)

names = c("year", "state", "state_fips", "county_fips", "registry",
          "race", "origin", "sex", "age", "population")

popn = data.table(read_fwf(
                  file = "data/population/us.1990_2015.19ages.adjusted.txt",
                  fwf_positions(start, end, names)))

nrow(popn) # 15 million

# select years of interest
popn = popn[year >= 2000 & year <= 2014]
popn[, .N, by = year][order(year)]
popn[, .N, by = sex][order(sex)]

popn[, population := as.numeric(population)]
popn[, age := as.numeric(age)]
popn[age == 0, age := 1] # combine first age groups 0-5
popn[, .N, by = age][order(age)] # 18 groups

# race
# 1 = White
# 2 = Black
# 3 = American Indian/Alaska Native
# 4 = Asian or Pacific Islander

# 0 = Non-Hispanic
# 1 = Hispanic

popn[, .N, by = race][order(race)]
popn[, .N, by = origin][order(origin)]

popn[origin == 1, nrace := 4] # hispanic
popn[origin == 0 & race == 1, nrace := 1] # white
popn[origin == 0 & race == 2, nrace := 3] # black
popn[origin == 0 & race %in% 3:4, nrace := 2] # other

popn[, .N, by = origin][order(origin)]
popn[, .N, by = nrace][order(nrace)]
table(popn[, .(origin, nrace)])

names(popn)
popn = popn[, .(year, state_fips, county_fips, sex, age, nrace, population)]
setnames(popn, names(popn),
         c("year", "state", "county", "sex", "age", "race", "pop"))

# explore
popn[, .N, by = state][order(state)]
popn = popn[state %in% states$state]
popn[, .N, by = state][order(state)]

# aggregate population (mainly race here)
pop = copy(popn[, .(pop = sum(pop)), by = .(year, state, county, sex, age, race)])
summary(pop$pop)

# check counties

# load mortality data
mort = readRDS("output/mortality.rds")

# rename geographic variables mortality
setnames(mort, c("state_resid_fips", "county_resid_fips"), c("state", "county"))
setkey(mort, state, county)
setkey(pop, state, county)

vstates = sort(unique(mort$state, pop$states))
length(vstates)

checkCounties(mort, pop, vstates, 'county')

# explore specific cases

# mort[state == "02" & county == "232", .N, by = .(year)]
# mort[state == "02" & county == "105", .N, by = .(year)]
# mort[state == "02" & county == "230", .N, by = .(year)]
#
# pop[state == "02" & county == "232", .N, by = .(year)]
# pop[state == "02" & county == "105", .N, by = .(year)]
# mort[state == "02" & county == "230", .N, by = .(year)]
#
# pop[state == "51" & county == "019", .N, by = .(year)]
# pop[state == "51" & county == "515", .N, by = .(year)]
# pop[state == "51" & county == "917", .N, by = .(year)]
#
# pop[state == "51" & county == "560", .N, by = .(year)]
# pop[state == "51" & county == "005", .N, by = .(year)]
# mort[state == "51" & county == "005", .N, by = .(year)]
# mort[state == "51" & county == "560", .N, by = .(year)]
#
# pop[state == "12" & county == "025", .N, by = .(year)]
# pop[state == "12" & county == "086", .N, by = .(year)]
# mort[state == "12" & county == "086", .N, by = .(year)]
# mort[state == "12" & county == "025", .N, by = .(year)]
#
# pop[state == "30" & county == "113", .N, by = .(year)]
# pop[state == "30" & county == "031", .N, by = .(year)]
# mort[state == "30" & county == "113", .N, by = .(year)]
# mort[state == "30" & county == "031", .N, by = .(year)]
#
# pop[state == "08" & county == "911", .(pop = sum(pop)), by = .(year)]
# pop[state == "08" & county == "001", .(pop = sum(pop)), by = .(year)]
# mort[state == "08" & county == "911", .N, by = .(year)]
# mort[state == "08" & county == "001", .N, by = .(year)]
#
# pop[state == "08" & county == "912", .(pop = sum(pop)), by = .(year)]
# pop[state == "08" & county == "013", .(pop = sum(pop)), by = .(year)]
# mort[state == "08" & county == "912", .N, by = .(year)]
# mort[state == "08" & county == "013", .N, by = .(year)]
#
# pop[state == "08" & county == "913", .(pop = sum(pop)), by = .(year)]
# pop[state == "08" & county == "059", .(pop = sum(pop)), by = .(year)]
# mort[state == "08" & county == "913", .N, by = .(year)]
# mort[state == "08" & county == "059", .N, by = .(year)]
#
# pop[state == "08" & county == "914", .(pop = sum(pop)), by = .(year)]
# pop[state == "08" & county == "123", .(pop = sum(pop)), by = .(year)]
# mort[state == "08" & county == "914", .N, by = .(year)]
# mort[state == "08" & county == "123", .N, by = .(year)]

#########################################
# adjustments
#########################################

# virginia
# effective July 1, 2001, Clifton Forge city, Virginia, formerly an independent
# city, merged with Alleghany county (FIPS code=51-005).
mort[state == "51" & county == "560", county := "005"]
mort[state == "51" & county %in% c("515", "019"), county := "917"]

# Florida
mort[state == "12" & county == "025", county := "086"]

# Montana
mort[state == "30" & county == "113", county := "031"]

# Colorado
pop[state == "08" & county == "911", county := "001"]
pop[state == "08" & county == "912", county := "013"]
pop[state == "08" & county == "913", county := "059"]
pop[state == "08" & county == "914", county := "123"]

# # old adjustments
# # alaska
# pop[state == "02" & county %in% c("105", "230"), county := "232"]
# pop[state == "02" & county %in% c("198"), county := "201"]
# pop[state == "02" & county %in% c("275", "195"), county := "280"]

# mort[state == "02" & county %in% c("105", "230"),
#             county := "232"]
# mort[state == "02" & county %in% c("198"),
#             county := "201"]
# mort[state == "02" & county %in% c("275", "195"),
#             county := "280"]

# pop[state == "02" & county %in% c("105", "230", "198", "275", "195",
                                   # "232", "201", "280"), .(pop = sum(pop)),
                                    # by = .(year, county)]

# setkey(mort, state, county, year)
# mort[state == "02" & county %in% c("105", "230", "198",
                                # "275", "195", "232", "201", "280"),
                    # .N, by = .(year, county)]

# mort[state == "51" & county %in% c("019"),
                    # .N, by = .(year, county)]

# pop[state == "51" & county %in% c("019"),
                    # .(pop = sum(pop)), by = .(year, county)]

# it seems to be right
# mort[county %in% c("232", "201", "280"), .N, by = .(year, county)]
# pop[county %in% c("232", "201", "280"), .(pop = sum(pop)), by = .(year, county)]

# check again
checkCounties(mort, pop, vstates, 'county')

# same number of counties? yes!
print(paste0('Number of counties population dataset: ', nrow(pop[, .N, by = .(state, county)]))) # 3145
print(paste0('Number of counties mortality dataset: ', nrow(mort[, .N, by = .(state, county)]))) # 3145

# some additional adjustments to the CDC mortality data

# age groups

summary(mort$nage) # 4725 cases missing
values = list(0:4,5:9,10:14,15:19,20:24,25:39,30:34,35:39,40:44,45:49,50:54,
               55:59,60:64,65:69,70:74,75:79,80:84,85:150) # keep the same for merging
length(values)

sum(is.na(mort$nage))/nrow(mort) * 100 # 1.2 percent of missing data
table(mort$nage > 150)

# recode age values (inefficient)
newvalues = seq_along(values) # 17 groups
for (i in seq_along(values)) {
    mort[nage %in% values[[i]], gage := newvalues[i]]
}

mort[, .N, by = gage][order(gage)] # 4725 missing data

# race
# race3 hispanic
# 1           ...   Mexican
# 2           ...   Puerto Rican
# 3           ...   Cuban
# 4           ...   Central or South American
# 5           ...   Other or unknown Hispanic
# 6           ...   Non - Hispanic white
# 7           ...   Non - Hispanic black
# 8           ...   Non - Hispanic other races
# 9           ...   Hispanic origin unknown (?)

# race2 three-category race/ethnicity
# 1     ...  White
# 2     ...  Races other than White or Black
# 3     ...  Black

mort[race3 %in% 1:5, race := 4] # hispanic
mort[race3 == 6, race := 1] # white
mort[race3 == 7, race := 3] # black
mort[race3 == 8, race := 2] # other
mort[is.na(race), race := as.numeric(race2)] # imputed value

mort[, .N, by = race][order(race),]
mort[, .N, by = causes39][order(causes39)]
mort[, .N, by = sex][order(sex)]

vars = c("year", "sex", "gage", "race", "causes39", "state",
         "county")

# only select key variables
mort = mort[, vars, with = FALSE]
setnames(mort, names(mort), c("year", "sex", "age", "race", "causes39", "state",
                              "county"))

mort[, .N, by = state][order(state)] # set order by state

# create groups of causes of deaths
table(mort$causes39)
mort[, cause := as.numeric(causes39)]
length(unique(mort$cause))

mort[cause %in% c(1,2,3,27), ncause := 1] # infectious
mort[cause %in% c(4:26,28:36), ncause := 2] # chronic
mort[cause %in% c(38:42), ncause := 3] # suicides and accidents
mort[cause %in% c(37), ncause := 4] # residual

mort[, .N, ncause]
table(mort$ncause)

# define different types of deaths by cause
mort[, deaths1 := ifelse(ncause == 1, 1, 0)]
mort[, deaths2 := ifelse(ncause == 2, 1, 0)]
mort[, deaths3 := ifelse(ncause == 3, 1, 0)]
mort[, deaths4 := ifelse(ncause == 4, 1, 0)]

summary(mort$deaths1)
summary(mort$deaths2)
summary(mort$deaths3)
summary(mort$deaths4)

# aggregate mortality data by cause
ma = mort[!is.na(age), .(deaths = .N, # individual records (sum)
                         deaths1 = sum(deaths1),
                         deaths2 = sum(deaths2),
                         deaths3 = sum(deaths3),
                         deaths4 = sum(deaths4)),
                         .(year, sex, age, race, state, county)]

print(paste0('Total number of deaths: ', sum(ma$deaths))) # 37062064

# the sum should be the same as the total number of deaths
print(paste0('Total number of deaths is equal to the sum of deaths by cause: ',
             (sum(ma$deaths) == (sum(ma$deaths1) + sum(ma$deaths2) +
              sum(ma$deaths3) + sum(ma$deaths4)))))

table(ma$sex)
table(ma$age)
table(ma$race)

# crude mortality rate plot by age
plot_data = list()

names(ma)
table(ma$year)

female_rates = NULL
male_rates = NULL
for (i in 2000:2014) {
    female_rates = c(female_rates, sum(ma[year==i & sex==2 , deaths]) /
        sum(as.numeric(pop[year == i & sex==2, as.numeric(pop)])) * 1000 )
    male_rates = c(male_rates, sum(ma[year==i & sex==1 , deaths]) /
        sum(as.numeric(pop[year == i & sex==1, as.numeric(pop)])) * 1000 )
}


df = data.table(year = rep(2000:2014, 2),
                sex = c(rep(1, length(2000:2014)),
                        rep(2, length(2000:2014))),
                rates =c(male_rates, female_rates))

ggplot(df, aes(x=year, y=rates, color=factor(sex, labels=c('Men', 'Women')))) +
       geom_line() + theme_minimal() +
       ylim(0,9) +
       theme(legend.position='top') +
       labs(color='')

# merge data!
mort = merge(ma, pop, all = TRUE, by=c("year", "sex", "age", "race",
                                       "state", "county"))

vars = c("year", "sex", "age", "race", "state", "county")
anyDuplicated(mort[, vars, with = FALSE])

# summary(mort$deaths)
# summary(mort$deaths1)
# summary(mort$deaths2)
# summary(mort$deaths3)
# summary(mort$deaths4)

summary(mort$pop) # missing data on pop 6811 cells, or pop = 0
mort[is.na(pop), pop := 0]

summary(mort$deaths) # a lot of missing cells
mort[is.na(deaths), deaths := 0]
mort[is.na(deaths1), deaths := 0]
mort[is.na(deaths2), deaths := 0]
mort[is.na(deaths3), deaths := 0]
mort[is.na(deaths4), deaths := 0]

summary(mort[pop == 0, deaths]) # 5 max, okey not that much
nrow(mort[pop == 0 & deaths > 0])
nrow(mort[pop == 0 & deaths1 > 0])
nrow(mort[pop == 0 & deaths2 > 0])
nrow(mort[pop == 0 & deaths3 > 0])
nrow(mort[pop == 0 & deaths4 > 0])

test = mort[pop == 0 & deaths > 0]
table(test$race)
table(test$age)
table(test$year)
table(test$sex)
table(test$state)

test[deaths == 5]
test[deaths == 1]

sum(test$deaths) # 7061 deaths with population 0 (?)
remove(test)

# remove temporary files
unlink('output/mortality.rds')

# save data
saveRDS(mort, file = 'output/mortality_population.rds')

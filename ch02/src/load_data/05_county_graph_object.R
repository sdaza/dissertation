########################################
# CDC mortality - income mobility paper
# create country graph object
# author: sebastian daza
########################################


# load libraries
library(sdazar)
library(ggplot2)
library(ggthemes)

library(USAboundaries)
library(maptools)
library(spdep)
library(sp)

library(INLA)

# read data
data = readRDS('output/cdc_chetty.rds')
print(paste0('Number of rows: ', nrow(data)))

# remove alaska and hawaii (only continental US)
data = data[!state %in% c('02','15')]
print(paste0('Number of rows after removing Alaska/Hawaii: ', nrow(data)))

# remove missing records
variables = c('z_relative_mob', 'z_absolute_mob', 'z_gini',
              'z_medicare_expenses', 'log_unemployment', 'z_uninsured',
              'log_pct_black', 'log_pct_hispanic')

data = data[complete.cases(data[, variables, with = FALSE])]
countmis(data)

# get counties, US 2000
counties <- us_counties("2000-07-01")
counties <- fortify(counties)
length(unique(data$fips)) / length(counties$fips)
counties = counties[counties$fips %in% unique(data$fips),]

all(as.character(counties$fips) %in% unique(data$fips))
all(unique(data$fips) %in% as.character(counties$fips))

ordered_fips = as.character(unique(counties$fips))
length(ordered_fips)

# order data based on counties
data = data[order(match(fips, ordered_fips))]

# plot county coverage

# number of counties
ncounties = length(unique(data$fips))
print(paste0('Number of valid counties: ', ncounties))

# filter sf object by counties available
counties = counties[counties$fips %in% unique(data$fips),]

# create plot of county coverage (map)
savepdf("output/county_coverage")
print(ggplot(counties) +
          geom_sf(colour = 'black', fill = '#addd8e', size = 0.2) +
          theme_map() +
          labs(x = NULL, y = NULL)
)
dev.off()

# create county graph object
temp <- poly2nb(counties)
nb2INLA("data/county.graph", temp)

# save descriptive information
descriptive_info = list(number_counties = ncounties)
saveRDS(descriptive_info, 'output/descriptive_info.rds')

# save data
saveRDS(data, paste0('output/cdc_chetty_', ncounties, '_counties.rds'))

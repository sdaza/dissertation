#######################################
# CDC mortality - income mobility paper
# load CDC mortality micro-data
# author: sebastian daza
#######################################


# libraries
library(readr)
library(sdazar)

# read fips codes
codes = read_csv('data/fips_codes.csv')
codes = data.table(codes)
codes = codes[, c(1,2,3,6), with = FALSE]
setnames(codes, c("abr", "state", "county", "name"))
states = codes[, .N , by = .(abr, state)][, N := NULL]

# read all mortality data files
dict_mort = readRDS('output/dict_mortality_files.rds')
mort_data = list()

# loop to load the data
for (f in seq_along(dict_mort)) {
    print(paste0(':::::: Processing file: ', f))
    d = data.table(read_fwf( file = paste0('data/mortality/', names(dict_mort)[f]),
                   fwf_positions(dict_mort[[f]]$start,
                                 dict_mort[[f]]$end,
                                 dict_mort[[f]]$names)))

    # recode some variables
    if (f < 4) {
    # age
        d[, nage := as.numeric(age)]
        d[nage > 199 & nage < 999, nage := 0]
        d[nage == 999, nage := NA]
    } else if (f >= 4 & f < 9) {
      # age
        d[, nage := as.numeric(age)]
        d[nage < 2001, nage := (age - 1000)]
        d[nage >= 2001 & nage <= 6999, nage := 0]
        d[nage == 9999, nage := NA]

        # sex
        d[, nsex := ifelse(sex == "F", 2, 1)][, sex := NULL][, sex := nsex][, nsex := NULL]

    } else  {
        # age
        d[, nage := as.numeric(age)]
        mvalues = c(0,1,2,4,5,6,9) * 1000.0 + 999.0
        d[nage %in%  mvalues, nage := NA]
        d[nage < 2000, nage := age - 1000]
        d[nage >= 2000 & nage <= 6999, nage := 0]

        # sex
        d[, nsex := ifelse(sex == "F", 2, 1)][, sex := NULL][, sex := nsex][, nsex := NULL]
    }
    # gender
    mort_data[[f]] = d
    remove(d)
}


# combine mortality files
print(':::::: Combining files')

mm = rbindlist(mort_data, fill = TRUE)
remove(mort_data)

# mm[, .N, by = year][order(year)] # check years
# mm[, .N, by = state_resid_fips][order(state_resid_fips)]

# recode state (very inefficient)
for (i in seq_along(states$state)) {
  mm[state_resid_fips == states$abr[i], state_resid_fips := states$state[i]]
}

# mm[, .N, by = state_resid_fips][order(state_resid_fips)] # state code

# remove residual codes
mm = mm[!state_resid_fips %in% c("00", "72", "AS", "GU", "MP", "VI", "ZZ",
                                  "XX", "ON", "QC", "XX")]

# mm[, .N, by = state_resid_fips][order(state_resid_fips)]

# save data
saveRDS(mm, file = 'output/mortality.rds')
remove(mm)
unlink('output/dict_mortality_files.rds')
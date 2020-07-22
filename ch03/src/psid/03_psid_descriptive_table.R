##############################
# county income mobility and individual health
# PSID descriptive table
# author: sebastian daza
##############################


# libraries
library(hash)
library(forcats)
library(ggplot2)
source("src/create_descriptive_tables.R")
source("src/utils.R")

# load data with missing records
ldat = readRDS("output/data/psid_data_ready_for_imputation_county_info.rds")

# select only head and wife respondents
ldat = ldat[head_wife == 1]
ldat[, race := factor(race, labels = c("White", "Black", "Other"))]

# variable adjustments
ldat[, max_age := getMax(imp_age), pid]
vars = c("q_relative_mob", "q_relative_mob_resid", "q_absolute_mob",
    "q_absolute_mob_resid", "q_gini", "gini_resid", "q_gini_resid")
ldat[, (vars) := lapply(.SD, as.numeric), .SDcols = vars]

# descriptive function
getDescriptives = function(x) {
    x = as.numeric(x)
    m = mean(x, na.rm = TRUE)
    sd = sd(x, na.rm = TRUE)
    min = getMin(x)
    max = getMax(x)
    pm = mean(is.na(x))
    n = length(x)
    return(c(m, sd, min, max, pm, n))
}

# descriptive function publication version
getDescriptivesPaper = function(x) {
    if (is.factor(x)) {
    	x = as.numeric(as.character(x))
    }
    m = mean(x, na.rm = TRUE)
    sd = sd(x, na.rm = TRUE)
    pm = mean(is.na(x))
    n = length(x)
    return(c(m, sd, pm, n))
}

# colnames
colnames =  c("Mean", "SD", "Min", "Max", "\\% Missing", "Observations")
colnamesPaper =  c("Mean", "SD", "\\% Missing", "Observations")

# time invariant variables
varnames = list(
    c("male", "max_age", "first_year", "race", "weight_less_55", "mother_marital_status", "mother_age"),
    c("move_once"),
    c("famsize", "head_marital_status", "head_education", "head_owns_house", "head_working_binary",
        "log_income_adj", "log_county_income", "log_population", "prop_black","nmoves"),
    c("relative_mob", "q_relative_mob", "relative_mob_resid",  "q_relative_mob_resid",
        "absolute_mob", "q_absolute_mob", "absolute_mob_resid", "q_absolute_mob_resid",
        "gini", "q_gini", "gini_resid", "q_gini_resid"),
    c("rev_health", "bmi", "depression", "smoking", "smoking_number")
)

varlabels = list(
    c("Male", "Age last interview", "Birth year", "Race-Ethnicity",
        "Weighted less than 55 oz", "Mother marital status at birth",
        "Mother's age at birth of respondent"),
    c("Proportion moved to a different county"),
    c("Family size", "Respondent living with any parent", "Parent's years of education",
        "Parent is working",
        "Parent is married", "Log household income", "County log income",
        "County log population", "County proportion Black",
        "Cumulative number of county moves"),
    c("County rank-rank correlation (original)",
        "Quintile county rank-rank correlation (original)",
        "Residualized county rank-rank correlation",
        "Quintile residualized county rank-rank correlation",
        "County upward mobility (original)",
        "Quintile county upward mobility (original)",
        "Residualized county upward mobility",
        "Quintile residualized county upward mobility",
        "County Gini coefficient (original)",
        "Quintile county Gini coefficient (original)",
        "Residualized county Gini coefficient",
        "Quintile Residualized county Gini coefficient"),
    c("Self-reported health", "BMI", "Depressive symptoms", "Current smoking", "Number of cigarettes")
)

# databases

# individual
individuals = ldat[!duplicated(ldat$pid)]

# timevariant
timevariant = ldat[time <= 20]
vars = c("absolute_mob", "relative_mob", "prop_black")
timevariant[, (vars) := lapply(.SD, function(x) {x / 100}), .SDcols = vars]

# moves
prop_moves = timevariant[, .(move_once = as.numeric(any(nmoves > 0))), pid]

# outcomes
outcomes = ldat[, max_time := max(time), pid]
outcomes = outcomes[max_time == time]

datalist = list("Time-invariant covariates" = individuals,
    prop_moves,
    "Time-variant covariates" = timevariant,
    "Exposure variables" = timevariant,
    "Outcomes" = outcomes)

note = longText("Note: Statistics based on non-imputed data. SD = Standard deviation. Observations correspond to respondents
    in the case of time-invariant and outcome variables, and person-years (N times exposure) for
    time-variant variables. Outcomes were measured in 2017.")

title = "PSID descriptive statistics of covariates and outcomes"
label = "tab:psid_descriptives"
digits = c(2, 2, 2, 2, 2, 0)
digitsPaper = c(2, 2, 2, 0)

createDescriptiveTable(datalist,
    summary_function = getDescriptives,
    column_names = colnames,
    variable_names = varnames,
    variable_labels = varlabels,
    align = NULL,
    arraystretch = 0.75,
    tabcolsep = 3,
    notesize = "scriptsize",
    digits = digits,
    note = note,
    title = title ,
    label = "tab:psid_descriptives",
    file = "output/tables/psid_descriptive_stats_complete.tex")

createDescriptiveTable(datalist,
    summary_function = getDescriptivesPaper,
    column_names = colnamesPaper,
    variable_names = varnames,
    variable_labels = varlabels,
    align = NULL,
    arraystretch = 0.75,
    tabcolsep = 3,
    notesize = "scriptsize",
    digits = digitsPaper,
    note = note,
    title = title ,
    label = "tab:psid_descriptives",
    file = "output/tables/psid_descriptive_stats.tex")


# create plot of mobility against population + data coverage
observed_counties = unique(readRDS("output/data/psid_data_ready_for_imputation_county_info.rds")[
    head_wife == 1 & first_year >= 1975, imp_fips])
head(observed_counties)
county = readRDS("output/data/chetty_county_data.rds")

names(county)
summary(county$log_population)
county[, matched := factor(ifelse(imp_fips %in% observed_counties, "PSID sample", "No PSID sample"))]
table(county$matched)
countmis(county)

table(county[matched == "PSID sample", statename])

savepdf("output/plots/psid_county_sample_relative_mob")
print(
ggplot(county, aes(log_population, z_relative_mob, color = matched, fill = matched)) +
    geom_point(alpha = 0.25) + scale_color_manual(values = c("#2b8cbe", "#f03b20")) +
    labs(x = "\nLog population (centered)", y = "Relative mobility (z-score)\n") +
    theme_minimal() +
    theme(legend.position = "top", legend.title=element_blank())
)
dev.off()

savepdf("output/plots/psid_county_sample_absolute_mob")
print(
ggplot(county, aes(log_population, z_absolute_mob, color = matched, fill = matched)) +
    geom_point(alpha = 0.25) + scale_color_manual(values = c("#2b8cbe", "#f03b20")) +
    labs(x = "\nLog population (centered)", y = "Absolute mobility (z-score)\n") +
    theme_minimal() +
    theme(legend.position = "top", legend.title=element_blank())
)
dev.off()

savepdf("output/plots/psid_county_sample_gini")
print(
ggplot(county, aes(log_population, z_gini, color = matched, fill = matched)) +
    geom_point(alpha = 0.25) + scale_color_manual(values = c("#2b8cbe", "#f03b20")) +
    labs(x = "\nLog population (centered)", y = "Gini coefficient (z-score)\n") +
    theme_minimal() +
    theme(legend.position = "top", legend.title=element_blank())
)
dev.off()
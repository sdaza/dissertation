##############################
# county income mobility and individual health
# descriptive table
# author: sebastian daza
##############################


# libraries
library(hash)
library(forcats)
library(ggplot2)
devtools::source_gist("c4d1089a501d3567be9fb784b1c5a6ab")
source("src/utils.R")

# load data with missing records
ldat = readRDS("output/data/nlsy97_data_ready_for_imputation.rds")

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

# colnames
colnames =  c("Mean", "SD", "Min", "Max", "\\% Missing", "Valid observations")

# time invariant variables
varnames = list(
    c("male", "min_age", "max_age", "ethnicity",
        "asvab_score", "parent_education", "mother_age_at_birth",
        "residential_moves_by_12"
    ),
    c("move_once"),
    c("hhsize", "imp_living_any_parent", "imp_parent_employed",
        "imp_parent_married", "log_income_adj",
        "log_county_income", "log_population", "prop_black",
        "nmoves"),
    c("relative_mob", "q_relative_mob", "relative_mob_resid",  "q_relative_mob_resid",
        "absolute_mob", "q_absolute_mob", "absolute_mob_resid", "q_absolute_mob_resid",
        "gini", "q_gini", "gini_resid", "q_gini_resid"
    ),
    c("rev_health", "bmi", "depression", "smoking", "smoking_days")
)

varlabels = list(
    c("Male", "Age first interview", "Age last interview",
        "Race-Ethnicity", "ASVAB Test Score",
        "Parent's Education (years)",
        "Mother's age at birth of respondent",
        "Number of residential moves by age 12"
    ),
    c("Proportion moved to a different county"),
    c("Family size", "Respondent living with any parent", "Parent is working",
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
        "Quintile Residualized county Gini coefficient"
    ),
    c("Self-reported health", "BMI", "Depressive symptoms",
        "Current smoking", "Days smoked")
)

# databases

# individual
ldat[, max_age := getMax(age_interview_est), id]
individuals = ldat[!duplicated(ldat$id)]
individuals[, ethnicity := factor(ethnicity,
    labels = c("White", "Black", "Hispanic", "Mixed"), levels = c(4, 1, 2, 3))]

# timevariant
timevariant = ldat[stime <= 8]
vars = c("absolute_mob", "relative_mob", "prop_black")
timevariant[, (vars) := lapply(.SD, function(x) {x/100}), .SDcols = vars]

# moves
prop_moves = timevariant[, .(move_once = as.numeric(any(nmoves > 0))), id]

# outcomes
outcomes = ldat[, max_time := max(stime), id]
outcomes = outcomes[max_time == stime]

datalist = list("Time-invariant covariates" = individuals,
     prop_moves,
    "Time-variant covariates" = timevariant,
    "Exposure variables" = timevariant,
    "Outcomes" = outcomes)

names(datalist)
note = "Note: Statistics based on non-imputed data. SD = Standard deviation.
    Observations correspond to respondents
    in the case of time-invariant and outcome variables, and person-years (N times exposure) for
    time-variant variables. Outcomes were measured in 2015."

title = "NLSY97 descriptive statistics of covariates and outcomes"
label = "tab:nlsy97_descriptives"
digits = c(2, 2, 2, 2, 2, 0)

createDescriptiveTable(datalist,
    summary_function = getDescriptives,
    column_names = colnames,
    variable_names = varnames,
    variable_labels = varlabels,
    align = NULL,
    arraystretch = 1.0,
    tabcolsep = 3,
    digits = digits,
    note = note,
    title = title ,
    label = "tab:descriptive",
    file = "output/tables/nlsy97_descriptive_stats.tex")

# create plots of mobility against population + data covarage
observed_counties = unique(readRDS("output/data/nlsy97_data_ready_for_imputation.rds")$imp_fips)
head(observed_counties)
county = readRDS("output/data/chetty_county_data.rds")
county[, matched := factor(ifelse(imp_fips %in% observed_counties, "NLSY97 sample",
    "No NLSY97 sample"), levels = c("No NLSY97 sample", "NLSY97 sample"))]
table(county$matched)

table(county[matched == "NLSY97 sample", statename])

colors = c("#2b8cbe", "#f03b20")
savepdf("output/plots/nlsy97_county_sample_relative_mob")
print(
ggplot(county, aes(log_population, z_relative_mob, color = matched, fill = matched)) +
    geom_point(alpha = 0.25) + scale_color_manual(values = colors) +
    labs(x = "\nLog population (centered)", y = "Relative mobility (z-score)\n") +
    theme_minimal() +
    theme(legend.position = "top", legend.title = element_blank())
)
dev.off()

savepdf("output/plots/nlsy97_county_sample_absolute_mob")
print(
ggplot(county, aes(log_population, z_absolute_mob, color = matched, fill = matched)) +
    geom_point(alpha = 0.25) + scale_color_manual(values = colors) +
    labs(x = "\nLog population (centered)", y = "Absolute mobility (z-score)\n") +
    theme_minimal() +
    theme(legend.position = "top", legend.title = element_blank())
)
dev.off()

savepdf("output/plots/nlsy97_county_sample_gini")
print(
ggplot(county, aes(log_population, z_gini, color = matched, fill = matched)) +
    geom_point(alpha = 0.25) + scale_color_manual(values = colors) +
    labs(x = "\nLog population (centered)", y = "Gini coefficient (z-score)\n") +
    theme_minimal() +
    theme(legend.position = "top", legend.title = element_blank())
)
dev.off()

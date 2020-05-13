##############################
# county income mobility and individual health
# descriptive table
# author: sebastian daza
##############################


# libraries
library(data.table)
library(xtable)
library(hash)
library(forcats)
library(ggplot2)
source("src/utils.R")

# load data with missing records
ldat = readRDS("output/data/psid_data_ready_for_imputation_county_info.rds")
names(ldat)
table(ldat$race)

# select only head and wife respondents
ldat = ldat[head_wife == 1]

# descriptives time invariant
ldat[, max_age := getMax(imp_age), pid]
temp = ldat[!duplicated(ldat$pid)]
length(unique(temp$pid))
nrow(temp)

temp[, white := ifelse(race == "white", 1, 0)]
temp[, black := ifelse(race %in% c("black", "other"), 1, 0)]

# descriptive function
getDescriptives = function(x) {
    if (is.factor(x)) {
    	  x = as.numeric(as.character(x))
    }
    m = mean(x, na.rm = TRUE)
    sd = sd(x, na.rm = TRUE)
    #min = getMin(x)
    #max = getMax(x)
    pm = mean(is.na(x))
    n = length(x)
    return(c(m, sd,
    # min, max,
    pm, n))
}

# time invariant variables
var_names = c("male", "max_age", "first_year",
              "white", "black",
              "weight_less_55", "mother_marital_status",
              "mother_age")

var_labels = c("Male", "Age last interview", "Birth year",
               "White", "Black or Other",
               "Weighted less than 55 oz", "Mother marital status at birth",
               "Mother's age at birth of respondent")

var_labels = paste0("\\quad ", var_labels)

tab_invariant = temp[, lapply(.SD, getDescriptives), .SDcols = var_names]
tab_invariant = t(tab_invariant)

colnames(tab_invariant) = c("Mean", "SD",
    # "Min", "Max",
    "% Missing", "Observations")
rownames(tab_invariant) = var_labels

# time variant
temp = ldat[time <= 20]
vars = c("absolute_mob", "relative_mob", "prop_black")
temp[, (vars) := lapply(.SD, function(x) {x/100}), .SDcols = vars]

var_names = c("famsize", "head_marital_status", "head_education",
    "head_owns_house", "head_working_binary", "log_income_adj",
    "log_county_income", "log_population", "prop_black",
    "nmoves",
    "relative_mob",  "q_relative_mob", "relative_mob_resid", "q_relative_mob_resid",
    "absolute_mob", "q_absolute_mob", "absolute_mob_resid", "q_absolute_mob_resid",
    "gini", "q_gini", "gini_resid", "q_gini_resid"
)

var_labels = c("Family size", "Head household married (yes=1)",
               "Head household education (years)",
               "Head household owns house (yes=1)",
               "Head household working (yes=1)",
               "Log household income",
               "County log income",
               "County log population",
               "County proportion Black",
               "Cumulative number of county moves",
               "County rank-rank correlation (original)",
               "Quintile county rank-rank correlation (original)",
               "Residualized county rank-rank correlation",
               "Quintile residualized county rank-rank correlation",
               "County upward mobility (original)",
               "Quintile county upward mobility (original)",
               "Residualized county upward mobility",
               "Quintile residualized county upward mobility",
               "County Gini coefficient",
               "Quintile county Gini coefficient (original)",
               "Residualized county Gini coefficient",
               "Quintile residualized county Gini coefficient"
               )

var_labels = paste0("\\quad ", var_labels)

tab_variant = temp[, lapply(.SD, getDescriptives), .SDcols = var_names]
tab_variant = t(tab_variant)

colnames(tab_variant) = c("Mean", "SD",
    #"Min", "Max",
    "% Missing", "Observations")
rownames(tab_variant) = var_labels

prop_moves = temp[, .(move_once = as.numeric(any(nmoves > 0))), pid]
tab_moves = t(prop_moves[, lapply(.SD, getDescriptives), .SDcols = "move_once"])
colnames(tab_moves) = c("Mean", "SD",
    #"Min", "Max",
    "% Missing", "Observations")
rownames(tab_moves) = paste0("\\quad ", "Proportion moved to a different county")

# outcome
ldat[, max_time := max(time), pid]
temp = ldat[max_time == time]

temp[, rev_health := factor(rev_health)]
temp[, rev_health_c := fct_collapse(
    rev_health, "1" = c("1", "2"), "2" = "3", "3" = "4", "4" = "5")]
table(is.na(temp$smoking_number), temp$smoking)
var_names = c("rev_health_c", "bmi", "depression", "smoking", "smoking_number")
var_labels = c("Self-reported health", "BMI", "Depressive symptoms",
               "Current smoking", "Number of ciggarretes smoked last month")
var_labels = paste0("\\quad ", var_labels)

tab_outcome = temp[, lapply(.SD, getDescriptives), .SDcols = var_names]
tab_outcome = t(tab_outcome)

colnames(tab_outcome) = c("Mean", "SD",
    #"Min", "Max",
     "% Missing", "Observations")
rownames(tab_outcome) = var_labels

# combined outputs and create latex table
list_tabs = list(tab_invariant,
                 tab_moves,
                 tab_variant, tab_outcome)
tab = do.call(rbind, list_tabs)

total_rows = dim(tab)[1]

addtorow = list()
addtorow$pos = list(-1, 0, 9, 19, 31, total_rows)
addtorow$command = c(
"\\hline
\\addlinespace
& Mean & SD & \\% Missing & Observations \\\\
\\addlinespace
",
"\\addlinespace
\\multicolumn{5}{l}{\\textit{Time-invariant covariates}} \\\\
\\addlinespace
",
"\\addlinespace
\\multicolumn{5}{l}{\\textit{Time-variant covariates}} \\\\
\\addlinespace
",
"\\addlinespace
\\multicolumn{5}{l}{\\textit{Exposure variables}} \\\\
\\addlinespace
",
"\\addlinespace
\\multicolumn{5}{l}{\\textit{Outcomes}} \\\\
\\addlinespace
",
"\\addlinespace
\\hline
\\addlinespace
")

caption = paste0('PSID descriptive statistics of covariates and outcomes')

output = print(xtable(tab, caption = caption,
                      label='tab:psid_descriptives',
                      align ="lrrrr",
                      digits=c(0, 2, 2, 2, 0)),
                caption.placement='top',
                hline.after=c(-1),
                align ="lrrrrrr",
                table.placement='htp',
                add.to.row = addtorow,
                include.rownames = TRUE,
                include.colnames = FALSE,
                type='latex',
                sanitize.text.function=identity
                )

mycomment = longText("Note: Statistics based on non-imputed data. SD = Standard deviation. Observations correspond to respondents
                     in the case of time-invariant and outcome variables, and person-years (N times exposure) for
                     time-variant variables. Outcomes were measured in 2017.")

arraystretch = 0.8
tabcolsep = 10
header_replacement = paste0("begin\\{table\\}\\[htp\\]\\\n\\\\scriptsize\\\n",
                            "\\\\setlength\\{\\\\tabcolsep\\}\\{",
                            tabcolsep,
                            "pt\\}\\\n\\\\renewcommand\\{\\\\arraystretch\\}\\{",
                            arraystretch,
                            "\\}\\\n\\\\begin\\{threeparttable\\}\\\n")
add_notes_table(output,
                 comment = mycomment,
                 header_replacement = header_replacement,
                 closing = "end\\{tablenotes\\}\\n",
                 closing_replacement = "end\\{tablenotes\\}\\\n\\\\end{threeparttable}\\\n",
                 filename = "output/tables/psid_descriptive_stats.tex")


# create plot of mobility against population + data coverage
observed_counties = unique(readRDS("output/data/psid_data_ready_for_imputation_county_info.rds")$imp_fips)
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
    geom_point(alpha = 0.25) + scale_color_manual(values = c("#f03b20", "#2b8cbe")) +
    labs(x = "\nLog population (centered)", y = "Relative mobility (z-score)\n") +
    theme_minimal() +
    theme(legend.position = "top", legend.title=element_blank())
)
dev.off()

savepdf("output/plots/psid_county_sample_absolute_mob")
print(
ggplot(county, aes(log_population, z_absolute_mob, color = matched, fill = matched)) +
    geom_point(alpha = 0.25) + scale_color_manual(values = c("#f03b20", "#2b8cbe")) +
    labs(x = "\nLog population (centered)", y = "Absolute mobility (z-score)\n") +
    theme_minimal() +
    theme(legend.position = "top", legend.title=element_blank())
)
dev.off()

savepdf("output/plots/psid_county_sample_gini")
print(
ggplot(county, aes(log_population, z_gini, color = matched, fill = matched)) +
    geom_point(alpha = 0.25) + scale_color_manual(values = c("#f03b20", "#2b8cbe")) +
    labs(x = "\nLog population (centered)", y = "Gini coefficient (z-score)\n") +
    theme_minimal() +
    theme(legend.position = "top", legend.title=element_blank())
)
dev.off()


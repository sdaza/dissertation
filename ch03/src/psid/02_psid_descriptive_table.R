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
source("ch03/src/utils.R")

# load data with missing records
ldat = readRDS("ch03/output/data/psid_data_ready_for_imputation.rds")

# M, SD, Missing % N 
table(ldat$race)

# select only head and wife respondents
ldat = ldat[head_wife == 1]
# descriptives time invariant

ldat[, max_age := getMax(imp_age), pid]
temp = ldat[!duplicated(ldat$pid)]
length(unique(temp$pid))
nrow(temp)

temp[, white := ifelse(race == "white", 1, 0)]
temp[, black := ifelse(race == "black", 1, 0)]
temp[, other := ifelse(race == "other" , 1, 0)]

getDescriptives = function(x) {
    m = mean(x, na.rm = TRUE)
    sd = sd(x, na.rm = TRUE)
    min = getMin(x)
    max = getMax(x)
    pm = mean(is.na(x))
    n = length(x)
    return(c(m, sd, min, max, pm, n))
}

# time invariant variables
var_names = c("male", "max_age", "first_year", 
              "white", "black", "other", 
              "weight_less_55", "mother_marital_status", 
              "mother_age")

var_labels = c("Male", "Age last interview", "Birth year",
               "White", "Black", "Other", 
               "Weighted less than 55 oz", "Mother marital status at birth", 
               "Mother's age at birth of respondent")

var_labels = paste0("\\quad ", var_labels)                    

tab_invariant = temp[, lapply(.SD, getDescriptives), .SDcols = var_names]
tab_invariant = t(tab_invariant)

colnames(tab_invariant) = c("Mean", "SD", "Min", "Max", "% Missing", "Observations")
rownames(tab_invariant) = var_labels

tab_invariant

# time variant
temp = ldat[stime <= 8]
vars = c("absolute_mob", "relative_mob", "prop_black")
temp[, (vars) := lapply(.SD, function(x) {x/100}), .SDcols = vars]

var_names = c("hhsize", "imp_living_any_parent", "imp_parent_employed", 
              "imp_parent_married", "log_income_adj",  
              "log_county_income", "log_population", "prop_black", 
              "nmoves", 
              "relative_mob", "absolute_mob", "gini")

var_labels = c("Family size", "Respondent living with any parent", "Parent is working", 
               "Parent is married", "Log household income", "County log income", 
               "County log population", "County proportion Black", 
               "Cumulative number of county moves", 
               "County rank-rank correlation", 
               "County upward mobility", "County Gini coefficient"
               )
var_labels = paste0("\\quad ", var_labels)    

tab_variant = temp[, lapply(.SD, getDescriptives), .SDcols = var_names]
tab_variant = t(tab_variant)

colnames(tab_variant) = c("Mean", "SD", "Min", "Max", "% Missing", "Observations")
rownames(tab_variant) = var_labels

prop_moves = temp[, .(move_once = as.numeric(any(nmoves > 0))), id]
tab_moves = t(prop_moves[, lapply(.SD, getDescriptives), .SDcols = "move_once"])
colnames(tab_moves) = c("Mean", "SD", "Min", "Max", "% Missing", "Observations")
rownames(tab_moves) = paste0("\\quad ", "Proportion moved to a different county")

# outcome
ldat[, max_time := max(stime), id]
temp = ldat[max_time == stime]
names(temp)
nrow(temp)

table(is.na(temp$smoking_30), temp$smoking)     
var_names = c("rev_health", "bmi", "depression", "smoking", "smoking_30")
var_labels = c("Self-reported health", "BMI", "Depressive symptoms", 
               "Current smoking", "Number of days smoked last month")
var_labels = paste0("\\quad ", var_labels)   

tab_outcome = temp[, lapply(.SD, getDescriptives), .SDcols = var_names]
tab_outcome = t(tab_outcome)

colnames(tab_outcome) = c("Mean", "SD", "Min", "Max", "% Missing", "Observations")
rownames(tab_outcome) = var_labels

# combined outputs and create latex table
list_tabs = list(tab_invariant, tab_moves, tab_variant, tab_outcome)
tab = do.call(rbind, list_tabs)

total_rows = dim(tab)[1]

addtorow = list()
addtorow$pos = list(-1, 0, 12, 21, 24, total_rows)
addtorow$command = c(
"\\hline
\\addlinespace
& Mean & SD & Min & Max & \\% Missing & Observations \\\\ 
\\addlinespace
",
"\\addlinespace
\\multicolumn{7}{l}{\\textit{Time-invariant covariates}} \\\\
\\addlinespace
",
"\\addlinespace
\\multicolumn{7}{l}{\\textit{Time-variant covariates}} \\\\
\\addlinespace
",
"\\addlinespace
\\multicolumn{7}{l}{\\textit{Exposure variables}} \\\\
\\addlinespace
",
"\\addlinespace
\\multicolumn{7}{l}{\\textit{Outcomes}} \\\\
\\addlinespace
",
"\\addlinespace
\\hline
\\addlinespace
")

caption = paste0('NLSY97 descriptive statistics of covariates and outcomes')

output = print(xtable(tab, caption = caption, 
                      label='tab:nlsy97_descriptives', 
                      align ="lrrrrrr",
                      digits=c(0, 2, 2, 2, 2, 2, 0)),
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
                     time-variant variables. Outcomes were measured in 2015.")

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
                 filename = "ch03/output/tables/nlsy97_descriptive_stats.tex")
##############################
# county income mobility and individual health
# PSID model tables
# author: sebastian daza
##############################


library(data.table)
library(hash)
library(texreg)
library(mitools)
source("src/utils.R")

# read output
unadjusted = readRDS("output/data/psid_unadjusted_models.rds")
adjusted = readRDS("output/data/psid_adjusted_models.rds")
models = c(unadjusted, adjusted)

n_imputations = 20

dat = readRDS("output/data/psid_data_ready_for_imputation_county_info.rds")
N = length(unique(dat[head_wife == 1 & first_year >= 1975, pid]))
rm(dat)

# continous exposure table
model_names = c("unadjusted_relative_mob_resid_results",
    "unadjusted_absolute_mob_resid_results",
    "unadjusted_gini_resid_results",
    "adjusted_relative_mob_resid_results",
    "adjusted_absolute_mob_resid_results",
    "adjusted_gini_resid_results")

list_rows = list()
for (i in seq_along(model_names)) {
    list_rows[[i]] = models[[model_names[i]]]
}

row_names = c("average_relative_mob_resid",
    "average_absolute_mob_resid",
    "average_gini_resid",
    "average_relative_mob_resid",
    "average_absolute_mob_resid",
    "average_gini_resid"
)

row_labels = c("Rank-rank",
               "Upward mobility $\\times$ -1",
               "Gini",
               "Rank-rank",
               "Upward mobility $\\times$ -1",
               "Gini"
               )

column_names = c("Health status", "BMI", "Depression", "Smoking", "Cigarettes smoked")
caption = "Estimates of average residualized continuous exposure on health indicators, PSID"
label = "tab:psid_zr_models"

comment = longText(paste0(
    paste0("Each coefficient represents a model. Coefficients and standard errors are combined estimates from ", n_imputations, " multiple imputed datasets. "),
        "Analyses based on exposure from 1 to 20 years old. We estimate different models depending on the outcome: Ordinal regression (self-reported health), General linear model (BMI, depression), 
        Logistic regression (smoking), Quasi-Poisson regression (cigarettes smoked). $^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$"
))

groups = list("Unadjusted models" = 1:3,
              "Adjusted models" = 4:6
)

createModelTables(
    list_rows,
    caption = caption,
    label = label,
    column_names = column_names,
    row_names = row_names,
    row_labels = row_labels,
    filename = "output/tables/psid_zr_models.tex",
    comment = comment,
    groups = groups,
    observations = N
)

# categorical exposure table
model_names = c("unadjusted_q_relative_mob_resid_results",
    "unadjusted_q_absolute_mob_resid_results",
    "unadjusted_q_gini_resid_results",
    "adjusted_q_relative_mob_resid_results",
    "adjusted_q_absolute_mob_resid_results",
    "adjusted_q_gini_resid_results")

list_rows = list()
for (i in seq_along(model_names)) {
    list_rows[[i]] = models[[model_names[i]]]
}

row_names = c("average_q_relative_mob_resid",
    "average_q_absolute_mob_resid",
    "average_q_gini_resid",
    "average_q_relative_mob_resid",
    "average_q_absolute_mob_resid",
    "average_q_gini_resid"
)

row_labels = c("Rank-rank",
               "Upward mobility $\\times$ -1",
               "Gini",
               "Rank-rank",
               "Upward mobility $\\times$ -1",
               "Gini"
               )

column_names = c("Health status", "BMI", "Depression", "Smoking", "Cigarettes smoked")
caption = "Estimates of average residualized categorical (quintile) exposure on health indicators, PSID"
label = "tab:psid_qr_models"

groups = list("Unadjusted models" = 1:3,
              "Adjusted models" = 4:6
)

createModelTables(
    list_rows,
    caption = caption,
    label = label,
    column_names = column_names,
    row_names = row_names,
    row_labels = row_labels,
    filename = "output/tables/psid_qr_models.tex",
    comment = comment,
    groups = groups,
    observations = N
)
##############################
# county income mobility and individual health
# model tables
# author: sebastian daza
##############################


library(data.table)
library(xtable)
source("src/utils.R")

# read output
models = readRDS("output/data/nlsy97_adjusted_models.rds")
n_imputations = 20

# continous exposure table
model_names = c("adjusted_relative_mob_resid_results",
    "adjusted_absolute_mob_resid_results",
    "adjusted_gini_resid_results",
    "adjusted_q_relative_mob_resid_results",
    "adjusted_q_absolute_mob_resid_results",
    "adjusted_q_gini_resid_results")

list_weights = list()
for (i in seq_along(model_names)) {
    list_weights[[i]] = models[[model_names[i]]][["weights"]]
}

comment = paste0("Analyses based on exposure from 12 to 20 years old. ",
    paste0("Statistics based on  ", n_imputations, " multiple imputed datasets.")
)

tableWeights(list_weights,
    model_names = c("\\quad Rank-rank", "\\quad Upward mobility", "\\quad Gini",
        "\\quad Rank-rank", "\\quad Upward mobility", "\\quad Gini"),
    caption  = "NLSY97 Stabilized treatment weights",
    label = "tab:nlsy97_ipt_weigths",
    comment = comment,
    filename = "output/tables/nlsy97_ipt_weights.tex",
    tabcolsep = 10,
    arraystretch = 1)
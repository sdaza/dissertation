##############################
# imputation PSID data
# author: sebastian daza
# version: 0.01
##############################

# libraries
library(data.table)
library(hash)
library(mice)
library(micemd)

source("ch03/src/utils.R")

# read data
mm = readRDS("ch03/output/data/psid_data_ready_for_imputation.rds")
mm[, age := imp_age]

center_vars = c("bmi", "depression", "life_satisfaction", "imp_age",
                "famsize",  "head_education", "age_mother")
mm[, (center_vars) := lapply(.SD, scale, scale = FALSE), .SDcol = center_vars]

summary(mm)
mm = mm[head_wife == 1]

# mm[, health := as.factor(health)]
mm[, race := factor(as.numeric(race))]
mm[, first_year := as.factor(first_year)]
# mm[, head_education := as.factor(head_education)]
# mm[, imp_age2 := imp_age ^ 2]

# hist(mm$imp_age)
# mm = mm[, .(pid, year, imp_age, year_born, first_year, male, race, log_income_adj,
#             weight_less_55, marital_status_mother, age_mother,
#             bmi, depression)]

# mm[, weight_less_55 := as.factor(weight_less_55)]
# mm[, marital_status_mother := as.factor(marital_status_mother)]

names(mm)

# independent variable z_relative_mob and z_gini
ini = mice(mm, maxit = 0)
head(ini$loggedEvent)
pred = ini$pred
meth = ini$meth
pred[,] = 0

tt = fluxplot(mm)
# countmis(mm[head_wife == 1])

# 3226
# length(unique(mm[relation_head %in% c(1, 10, 2, 20, 22), pid]))

# explore some respondents
# ids = unique(mm[head_wife == 1, pid])
# mm[pid == sample(ids, 1), .(pid, year, imp_age, relation_head, whynoresp, depression)]


# fx = fluxplot(mm)

# set up methods and prediction matrix
methods = hash(
               # not to impute
               "year_born" = "",
               "smoking_ever" = "",
               "individual_health" = "2l.pmm",
               "individual_working_binary" = "",
               # time invariant covariates
               "race" = "2lonly.function",
               "weight_less_55" = "2lonly.function",
               "marital_status_mother" = "2lonly.function",
               "age_mother" = "2lonly.pmm",
               # time variant covariates
               "log_income_adj" = "2l.pmm",
               "head_marital_status" = "2l.pmm",
               "head_education" = "2l.pmm",
               "head_owns_house" = "2l.pmm",
               "head_working_binary" = "2l.pmm",
               "famsize" = "2l.pmm",
               # outcomes
               "life_satisfaction" = "",
               "depression" = "2l.pmm",
               "bmi" = "2l.pmm",
               "smoking" = "2l.pmm",
               "smoking_number" = "2l.pmm",
               "health_binary" = ""
               )

meth[keys(methods)] = values(methods)

imputationFunction = list("race" = "polyreg",
                          "weight_less_55" = "logreg",
                          "marital_status_mother" = "logreg"
                          )

cluster_var =  list("race" = "pid",
                    "weight_less_55" = "pid",
                    "marital_status_mother" = "pid"
                    )

pred[,] = 0
predictors = hash(
     # time-invariante coviarates
     "pid" = -2,
     "male" = 1,
     "race" = 1,
     "imp_age" = 1,
     "first_year" = 1,
     "weight_less_55" = 1,
     "marital_status_mother" = 1,
     "age_mother" = 1,
     # time-varying covariates
     "log_income_adj" = 1,
     "head_marital_status" = 1,
     "head_education" = 1,
     "head_owns_house" = 1,
     "head_working_binary" = 1,
     "famsize" = 1,
     # outcomes
     "depression" = 1,
     "bmi" = 1,
     "life_satisfaction" = 0,
     "smoking" = 1,
     "smoking_number" = 0,
     "health_binary" = 0,
     "individual_health" = 0
    )

# assign predictors
predictors_vectors = keys(predictors)[!keys(predictors) == "pid"]
for (i in seq_along(predictors_vectors)) {
    pred[predictors_vectors[i], keys(predictors)] = values(predictors)
}
# set diagonal of matrix to 0
diag(pred) = 0

# explore
pred["log_income_adj",]
pred["age_mother",]
pred["marital_status_mother",]
pred["weight_less_55",]
pred["individual_health",]
pred["race",]

pred["depression", ]

# adjustments
pred[c("depression", "bmi", "health_binary", "smoking"), c("life_satisfaction")] = 0
pred["life_satisfaction", c("bmi", "health_binary", "smoking")] = 0
pred["life_satisfaction", c("bmi", "health_binary", "smoking")] = 0


# run imputation
imp = mice::mice(mm,
                 predictorMatrix = pred,
                 method = meth,
                 m = 5,
                 maxit = 5,
                 imputationFunction = imputationFunction,
                 cluster_var = cluster_var)

# explore imputations
print(plot(imp, c("bmi", "depression", "individual_health")))
print(plot(imp, c("smoking", "smoking_number")))
print(plot(imp, c("log_income_adj", "age_mother")))
print(plot(imp, c("marital_status_mother", "weight_less_55", "famsize")))
print(plot(imp, c("head_marital_status", "head_education", "head_owns_house")))
print(plot(imp, c("head_working_binary", "head_education", "head_owns_house")))

print(densityplot(imp, ~ bmi + depression))
print(densityplot(imp, ~ individual_health + smoking_number + smoking))
print(densityplot(imp, ~ log_income_adj))
print(densityplot(imp, ~ age_mother))
print(densityplot(imp, ~ marital_status_mother + weight_less_55))
print(densityplot(imp, ~ head_education + head_owns_house))
print(densityplot(imp, ~ head_working_binary + famsize))

test = data.table(complete(imp, 1))
ids = unique(test$pid)

sid = sample(ids, 1)
mm[pid == sid, .(pid, year, age, depression, bmi, individual_health, smoking_number)]
test[pid == sid, .(pid, year, age, depression, bmi, individual_health, smoking_number)]\



nrow(mm[year == 2017 & head_wife == 1])
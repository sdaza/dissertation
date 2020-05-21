##############################
# county income mobility and individual health
# imputation psid data
# author: sebastian daza
##############################


print("::::::: imputing with absolute mob :::::::")

# create mice object
ini = mice(mm, maxit = 0)
head(ini$loggedEvent)
pred = ini$pred
meth = ini$meth

tt = fluxplot(mm)

# explore missing patterns
tt[, c(1,2)]

# countmis(mm[head_wife == 1])

# length(unique(mm[relation_head %in% c(1, 10, 2, 20, 22), pid]))
# explore some respondents
# ids = unique(mm[head_wife == 1, pid])
# mm[pid == sample(ids, 1), .(pid, year, imp_age, relation_head, whynoresp, depression)]

vars = c("z_relative_mob", "z_gini", "log_population", "log_county_income", "log_income_adj", 
         "imp_age", "first_year", "head_education", "head_marital_status", "head_owns_house", 
         "head_working_binary", "weight_less_55", "mother_age", 
         "mother_marital_status")

correlations = cor(mm[, ..vars])
correlations["log_county_income", "log_income_adj"]

test= correlations > .60 & correlations < 1.0
if (sum(test) > 0) print(test)

meth[1:length(meth)] = ""

methods = hash(
    # time invariant covariates
    "weight_less_55" = "2lonly.function",
    "mother_marital_status" = "2lonly.function",
    "mother_age" = "2lonly.function",
    # time variant covariates
    "log_income_adj" = "2l.pmm",
    "head_marital_status" = "2l.pmm",
    "head_education" = "2l.pmm",
    "head_owns_house" = "2l.pmm",
    "head_working_binary" = "2l.pmm",
    "famsize" = "2l.pmm",
    # outcomes
    "last_wave_depression" = "2lonly.function",
    "last_wave_bmi" = "2lonly.function",
    "last_wave_smoking" = "2lonly.function",
    "last_wave_smoking_number" = "2lonly.function",
    "last_wave_rev_health" = "2lonly.function"
)

meth[keys(methods)] = values(methods)

# custom imputation functions
imputationFunction = list(
    "mother_age"= "cart",
    "mother_marital_status" = "cart",
    "weight_less_55" = "cart",
    "last_wave_smoking" = "cart", 
    "last_wave_depression" = "cart",
    "last_wave_bmi" = "cart",
    "last_wave_smoking_number" = "cart",
    "last_wave_rev_health" = "cart"
)

cluster_var =  list(
    "mother_age" = "pid",
    "mother_marital_status" = "pid",
    "weight_less_55" = "pid",
    "last_wave_smoking" = "pid", 
    "last_wave_depression" = "pid",
    "last_wave_bmi" = "pid",
    "last_wave_smoking_number" = "pid",
    "last_wave_rev_health" = 'pid'
)

# predictors
predictors = hash(
    # time-invariant coviarates
    "pid" = -2,
    "male" = 1,
    "white" = 1,
    "imp_age" = 1,
    "last_wave_imp_age" = 0,
    "first_year" = 1,
    "weight_less_55" = 1,
    "mother_marital_status" = 1,
    "mother_age" = 1,
    # time-varying covariates
    "log_income_adj" = 1,
    "head_marital_status" = 1,
    "head_education" = 1,
    "head_owns_house" = 1,
    "head_working_binary" = 1,
    "famsize" = 1,
    "nmoves" = 1,
    # exposure variables
    "relative_mob_resid" = 0, 
    "absolute_mob_resid" = 1,
    "gini_resid" = 1,
    "log_county_income" = 1,
    "log_population" = 1,
    "z_prop_black" = 1,
    # outcomes
    "last_wave_depression" = 0,
    "last_wave_bmi" = 0,
    "last_wave_smoking" = 0,
    "last_wave_smoking_number" = 0,
    "last_wave_rev_health" = 0
)

cor(mm[, .(z_relative_mob, z_absolute_mob, z_gini, 
           log_population, log_county_income, z_prop_black)])

# assign predictors
vector_predictors = names(meth[meth != ""])

# checks
setdiff(keys(predictors), rownames(pred))

# reset prediction matrix
pred[,] = 0

# complex version
for (i in seq_along(vector_predictors)) {
    pred[vector_predictors[i], keys(predictors)] = values(predictors)
}

# set diagonal of matrix to 0
diag(pred) = 0

# remove race
vars = c("last_wave_rev_health", "head_working_binary", "head_owns_house", 
         "mother_age", "last_wave_smoking", "head_marital_status", 
         "last_wave_depression", "head_education")
remove_predictors = c("white")
pred[vars, remove_predictors] = 0

# remove age from time-invariant variables
vars = c("mother_marital_status", "weight_less_55")
pred[vars, "imp_age"] = 0

# add last wave age as contr
vars = c("last_wave_bmi", "last_wave_smoking", "last_wave_smoking_number", 
         "last_wave_rev_health", "last_wave_depression")
remove_predictors = c("imp_age")
pred[vars, remove_predictors ] = 0

# add group mean for time-variant variables
# vars = c("log_income_adj", "head_marital_status", "head_education",
#          "head_owns_house" , "head_working_binary", "famsize")
# group_mean_predictors = c("log_income_adj", "head_marital_status", "head_education",
#                           "head_owns_house" , "head_working_binary", "famsize", "nmoves",
#                           "z_relative_mob", "z_gini", "log_county_income", "log_population",
#                           "z_prop_black")
# pred[vars, group_mean_predictors] = 3
# 
# countmis(mm[year == 2017])
# mm[pid == sample(unique(mm$pid), 1),.(pid, imp_age, last_wave_imp_age, last_wave_depression)]

# explore
pred["log_income_adj",]
pred["mother_age",]
pred["mother_marital_status",]
pred["head_education",]
pred["head_marital_status",]
pred["weight_less_55",]
pred["last_wave_depression", ]
pred["last_wave_rev_health", ]

# testing imputation
imp = mice(
        mm,
        predictorMatrix = pred,
        method = meth,
        vis = "monotone",
        m = 20,
        maxit = 10,
        imputationFunction = imputationFunction,
        cluster_var = cluster_var,
        seed = 23734
    )

# # iteration imputation
# imputations_per_core = 2
# number_cores = 4
# 
# imp = parlmice(
#     mm,
#     predictorMatrix = pred,
#     method = meth,
#     vis = "monotone",
#     n.core = number_cores,
#     n.imp.core = imputations_per_core,
#     maxit = 10,
#     imputationFunction = imputationFunction,
#     cluster_var = cluster_var,
#     cluster.seed = 23750
# )

# explore imputations
savepdf("output/plots/psid_zr_absolute_mob_imp_iterations")
print(plot(imp, c("last_wave_bmi", "last_wave_depression", "last_wave_rev_health")))
print(plot(imp, c("last_wave_smoking", "last_wave_smoking_number")))
print(plot(imp, c("log_income_adj", "mother_age")))
print(plot(imp, c("mother_marital_status", "weight_less_55", "famsize")))
print(plot(imp, c("head_marital_status", "head_education", "head_owns_house")))
print(plot(imp, c("head_working_binary")))
dev.off()

# simp = extractImputations(imp)
simp = imp
savepdf("output/plots/psid_zr_absolute_mob_imp_values")
print(densityplot(simp, ~ last_wave_bmi +last_wave_depression))
print(densityplot(simp, ~ last_wave_rev_health + last_wave_smoking_number +last_wave_smoking))
print(densityplot(simp, ~ log_income_adj))
print(densityplot(simp, ~ mother_age))
print(densityplot(simp, ~ mother_marital_status + weight_less_55))
print(densityplot(simp, ~ head_education + head_owns_house))
print(densityplot(simp, ~ head_working_binary + famsize))
dev.off()

# set format some variables
print(imp$loggedEvents)

saveRDS(imp, file = "output/data/psid_zr_absolute_mob_imputations.rds")
print(paste0("Number of imputations: ", imp$m))

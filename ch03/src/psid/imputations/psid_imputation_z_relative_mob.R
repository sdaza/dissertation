##############################
# county income mobility and individual health
# imputation psid data
# author: sebastian daza
##############################

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

methods = hash(
    # time invariant covariates
    # "race" = "2lonly.function",
    "weight_less_55" = "2lonly.pmm",
    "mother_marital_status" = "2lonly.pmm",
    "mother_age" = "2lonly.pmm",
    # time variant covariates
    "log_income_adj" = "2l.pmm",
    "head_marital_status" = "2l.pmm",
    "head_education" = "2l.pmm",
    "head_owns_house" = "2l.pmm",
    "head_working_binary" = "2l.pmm",
    "famsize" = "2l.pmm",
    # outcomes
    "depression" = "2l.pmm",
    "bmi" = "2l.pmm",
    "smoking" = "2l.pmm",
    "smoking_number" = "2l.pmm",
    "rev_health" = "2l.pmm", 
    "individual_health" = ""
)

meth[keys(methods)] = values(methods)

# custom imputation functions
imputationFunction = list(
                          "weight_less_55" = "logreg",
                          "mother_marital_status" = "logreg"
                          )

cluster_var =  list(
                    "weight_less_55" = "pid",
                    "mother_marital_status" = "pid"
                    )

predictors = hash(
    # time-invariante coviarates
    "pid" = -2,
    "male" = 1,
    "race" = 1,
    "imp_age" = 1,
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
    # outcomes
    "depression" = 1,
    "bmi" = 1,
    "smoking" = 1,
    "smoking_number" = 0,
    "rev_health" = 1
)

# assign predictors
predictors_vectors = names(meth[meth != ""])

# checks
setdiff(keys(predictors), rownames(pred))

pred[,] = 0
for (i in seq_along(predictors_vectors)) {
    pred[predictors_vectors[i], keys(predictors)] = values(predictors)
}

# set diagonal of matrix to 0
diag(pred) = 0

# remove age from time-invariant variables
vars = c("mother_age", "mother_marital_status", "weight_less_55")
pred[vars, "imp_age"] = 0

# explore
pred["log_income_adj",]
pred["mother_age",]
pred["mother_marital_status",]
pred["weight_less_55",]
pred["race",]
pred["depression", ]

table(mm$time)

sessionInfo()

# testing imputation
test = mice(
    mm,
    predictorMatrix = pred, 
    method = meth, 
    m = 2, 
    maxit = 5
)

# iteration imputation
list_imputations = list()
random_seeds = c(1059, 1711, 1037, 1031, 1724, 1651, 1463, 1493, 1725, 1411)

for (i in 1:2) {
    print(paste0("iteration ", i))
    list_imputations[[i]] = mice(
        mm, 
        predictorMatrix = pred, 
        meth = meth, 
        m = 2, 
        maxtit = 3, 
        seed = random_seeds[i]

    )
}


# bind imputations
imp = NULL
for (i in 1:(length(list_imputations) - 1)) {
    if (i == 1) {
        imp = ibind(list_imputations[[i]], list_imputations[[i+1]])
    } 
    else if (i > 1) {
        imp = ibind(imp, list_imputations[[i]])  
    }
}

imp = parlmice(
    mm,
    predictorMatrix = pred,
    method = meth,
    n.core = number_cores,
    n.imp.core = imputations_per_core,
    maxit = 20,
    imputationFunction = imputationFunction,
    cluster_var = cluster_var
)

# explore imputations

savepdf("ch03/output/psid_relative_mob_iterations")
print(plot(imp, c("bmi", "depression", "individual_health")))
print(plot(imp, c("smoking", "smoking_number")))
print(plot(imp, c("log_income_adj", "mother_age")))
print(plot(imp, c("mother_marital_status", "weight_less_55", "famsize")))
print(plot(imp, c("head_marital_status", "head_education", "head_owns_house")))
print(plot(imp, c("head_working_binary")))
dev.off()

savepdf("ch03/output/psid_relative_mob_imp_values")
print(densityplot(imp, ~ bmi + depression))
print(densityplot(imp, ~ individual_health + smoking_number + smoking))
print(densityplot(imp, ~ log_income_adj))
print(densityplot(imp, ~ mother_age))
print(densityplot(imp, ~ mother_marital_status + weight_less_55))
print(densityplot(imp, ~ head_education + head_owns_house))
print(densityplot(imp, ~ head_working_binary + famsize))
dev.off()


# save results of imputation
saveRDS(imp, "ch03/output/data/psid_relative_mob_imputation.rds")


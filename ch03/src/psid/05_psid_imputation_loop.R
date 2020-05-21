# loop for multiple imputations


# imputation parameters
number_cores = 10
imputations_per_core = 2
iterations = 20

# create fluxplot
savepdf("output/plots/psid_fluxplot")
    print(fluxplot(mm))
dev.off()

# all exposure variables
all_exposure_vars = c("z_relative_mob", "z_absolute_mob", "z_gini",
    "relative_mob_resid", "absolute_mob_resid",
    "gini_resid", "q_relative_mob_resid",
    "q_absolute_mob_resid", "q_gini_resid",
    "q_relative_mob", "q_absolute_mob", "q_gini",
    "log_population", "log_county_income", "z_prop_black"
)

# imputation file names
filenames = hash(
    "psid_z_relative_mob_imp" = c("z_relative_mob", "z_gini"),
    "psid_z_absolute_mob_imp" = c("z_absolute_mob", "z_gini"),
    "psid_zr_relative_mob_imp" = c("relative_mob_resid", "gini_resid"),
    "psid_zr_absolute_mob_imp" = c("absolute_mob_resid", "gini_resid"),
    "psid_q_relative_mob_imp" = c("q_relative_mob", "q_gini"),
    "psid_q_absolute_mob_imp" = c("q_absolute_mob", "q_gini"),
    "psid_qr_relative_mob_imp" = c("q_relative_mob_resid", "q_gini_resid"),
    "psid_qr_absolute_mob_imp" = c("q_absolute_mob_resid", "q_gini_resid")
)

# create initial imputation object
ini = mice(mm, maxit = 0)
print(ini$logged)
pred_ini = ini$pred
meth_ini = ini$meth

# testing
# filenames = filenames[c("psid_z_relative_mob_imp")]

for (i in seq_along(filenames)) {
    print(paste0(":::: running imputation ", keys(filenames)[i], " ::::"))
    exposure = as.vector(values(filenames[keys(filenames)[i]]))

    # create prediction and method matrices
    meth = meth_ini
    pred = pred_ini
    pred[,] = 0

    methods = hash(
        # time invariant covariates
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
        "last_wave_depression" = "2lonly.pmm",
        "last_wave_bmi" = "2lonly.pmm",
        "last_wave_smoking" = "2lonly.pmm",
        "last_wave_smoking_number" = "2lonly.pmm",
        "last_wave_rev_health" = "2lonly.pmm"
    )

    meth[keys(methods)] = values(methods)

    # predictors
    predictors = hash(
        # time-invariant coviarates
        "pid" = -2,
        "male" = 1,
        "race" = 1,
        "imp_age" = 1,
        "first_year" = 1,
        "weight_less_55" = 1,
        "mother_marital_status" = 1,
        "mother_age" = 1,
        "csweight" = 1,
        # time-varying covariates
        "log_income_adj" = 1,
        "head_marital_status" = 1,
        "head_education" = 1,
        "head_owns_house" = 1,
        "head_working_binary" = 1,
        "famsize" = 1,
        "nmoves" = 1,
        # exposure variables
        "z_relative_mob" = 0,
        "z_absolute_mob" = 0,
        "z_gini" = 0,
        "relative_mob_resid" = 0,
        "absolute_mob_resid" = 0,
        "gini_resid" = 0,
        "q_relative_mob" = 0,
        "q_absolute_mob" = 0,
        "q_gini" = 0,
        "q_relative_mob_resid" = 0,
        "q_absolute_mob_resid" = 0,
        "q_gini_resid" = 0,
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

    # assign predictors
    print(exposure)
    predictors[exposure] = 1
    vector_predictors = names(meth[meth != ""])
    for (h in seq_along(vector_predictors)) {
        pred[vector_predictors[h], keys(predictors)] = values(predictors)
    }

    # set diagonal of matrix to 0
    diag(pred) = 0

    # # custom imputation functions
    # imputationFunction = list(
    #     "mother_age"= "cart",
    #     "mother_marital_status" = "cart",
    #     "weight_less_55" = "cart",
    #     "last_wave_smoking" = "cart", 
    #     "last_wave_depression" = "cart",
    #     "last_wave_bmi" = "cart",
    #     "last_wave_smoking_number" = "cart",
    #     "last_wave_rev_health" = "cart"
    # )

    # cluster_var =  list(
    #     "mother_age" = "pid",
    #     "mother_marital_status" = "pid",
    #     "weight_less_55" = "pid",
    #     "last_wave_smoking" = "pid", 
    #     "last_wave_depression" = "pid",
    #     "last_wave_bmi" = "pid",
    #     "last_wave_smoking_number" = "pid",
    #     "last_wave_rev_health" = 'pid'
    # )

    # time invariant variables without age as predictor
    vars = c("mother_marital_status", "weight_less_55", "mother_age")
    pred[vars, "imp_age"] = 0

    # don't predict smoking variables with each other
    pred["last_wave_smoking", "last_wave_smoking_number"] = 0
    pred["last_wave_smoking_number", "last_wave_smoking"] = 0

    # check variables being imputed are the right ones
    vars = setdiff(all_exposure_vars, exposure)
    if (sum(pred[vars, ]) > 0 & sum(pred[vector_predictors, vars]) > 0) {
        stop("Exposure variables are not right!")
    }
    if (any(pred[vector_predictors, exposure] == 0)) {
        stop("Some variables do not have exposure variables as predictors")
    }

    # run imputation
    imp = parlmice(
        mm,
        predictorMatrix = pred,
        method = meth,
        n.core = number_cores,
        n.imp.core = imputations_per_core,
        maxit = iterations,
        cluster.seed = seed
    )

    # save results of imputation
    saveRDS(imp, paste0("output/data/", keys(filenames)[i], ".rds"))
    # # send message to slack
    # slackr::text_slackr(paste0("Imputation ", keys(filenames)[i],
    #     " finished at ", Sys.time()))

    # explore quality of imputations
    savepdf(paste0("output/plots/", keys(filenames)[i], "_iterations"))
        print(plot(imp, c("last_wave_bmi", "last_wave_depression", "last_wave_rev_health")))
        print(plot(imp, c("last_wave_smoking", "last_wave_smoking_number")))
        print(plot(imp, c("log_income_adj", "mother_age")))
        print(plot(imp, c("mother_marital_status", "weight_less_55", "famsize")))
        print(plot(imp, c("head_marital_status", "head_education", "head_owns_house")))
        print(plot(imp, c("head_working_binary")))
    dev.off()

    savepdf(paste0("output/plots/", keys(filenames)[i], "_values"))
        print(densityplot(imp, ~ last_wave_bmi +last_wave_depression))
        print(densityplot(imp, ~ last_wave_rev_health + last_wave_smoking_number +last_wave_smoking))
        print(densityplot(imp, ~ log_income_adj))
        print(densityplot(imp, ~ mother_age))
        print(densityplot(imp, ~ mother_marital_status + weight_less_55))
        print(densityplot(imp, ~ head_education + head_owns_house))
        print(densityplot(imp, ~ head_working_binary + famsize))
    dev.off()

    # remove objects
    rm(imp, pred, meth)

}



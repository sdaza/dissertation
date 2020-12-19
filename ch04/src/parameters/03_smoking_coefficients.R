##################################
# National Health Interview Survey (NHIS) to get smoking income coefficients
# author: sebastian daza
##################################


library(data.table)
library(survey)
library(xtable)
library(reldist)
table = function (...) base::table(..., useNA = 'ifany')

# read NHIS 2019
h = fread("data/health_survey_adults.csv")
setnames(h, names(h), tolower(names(h)))
setnames(h, "wtfa_a", "wt")

# define smoking variables
table(h$smkev_a)
table(h$smknow_a)
table(h$srvy_yr)

h[, smoking := 0]
h[smkev_a == 1 & smknow_a %in% c(1, 2), smoking := 1]
h[smkev_a %in% c(7, 8, 9), smoking := NA]
h[smknow_a %in% c(7, 8), smoking := NA]
table(h$smoking)

# gender 
table(h$sex_a)
h[sex_a != 7, sex := ifelse(sex_a == 1, 1, 0)]

# age groups
summary(h$agep_a)
h[, age_group := ifelse(agep_a >= 30 & agep_a <= 50, 1, 0)]
h[age_group == 1, incomeType:= cut(faminctc_a, breaks = quantile(faminctc_a,
    probs = 0:5/5),
    labels = 1:5, right = TRUE, include.lowest = TRUE)]

hist(h[faminctc_a < quantile(h$faminctc_a, 0.33), faminctc_a])
gini(h$faminctc_a)
table(h[age_group == 1, incomeType])

# select only respondets between 30 and 50
s = h[age_group == 1]
dim(s)
setorder(s, incomeType)

weighted.mean(s[!is.na(smoking), smoking], s[!is.na(smoking), wt])

design = svydesign(ids= ~ hhx, weights = ~wt, data=s)
tab = s[, .(smoking_prop = weighted.mean(smoking, wt, na.rm = TRUE)), incomeType][!is.na(incomeType)]
setorder(tab, incomeType)
setnames(tab, names(tab), c("Income quintile", "Smoking proportion"))

sample_size = nrow(s)

# create table
print(xtable(tab,
    caption = paste0("Proportion smoking by income quintile respondents 30-50 years old, NHIS 2019, N = ", sample_size)), 
    table.placement = "htp", 
    caption.placement = "top", 
    include.rownames  = FALSE
)

# logistic model to get baseline income coefficients
m = svyglm(smoking ~ -1 + as.factor(incomeType), design = design, family = quasibinomial)
summary(m)
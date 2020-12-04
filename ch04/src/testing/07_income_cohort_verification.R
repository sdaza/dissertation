##############################
# generative model income mobility and mortality
# income cohort verification
# author: sebastian daza
##############################


library(data.table)
path = "models/MobHealthRecycling/output/verification/testing/"
income = fread(paste0(path, "income_generation.csv"))

# functions
perc.rank = function(x) trunc(rank(x))/length(x)
reg = function(kid_income, parent_income, relative = TRUE) {
    m = lm(kid_income ~ parent_income)
    c = coef(m)
    if (relative) { return(c[2])}
    else {
        return (c[1]  + 0.25 * c[2])
    }
}

addDiagonalProbs = function(parent_type, kid_type, index = 1) {
    diag(prop.table(table(parent_type, kid_type), 1))[index]
}

# read data
m = fread(paste0(path, "mortality.csv"))
cty = fread(paste0(path, "county.csv"))

dim(cty)
table(cty$model_time)
dim(m)

table(m$generation)


anyDuplicated(income$kid_id)
table(income$cohort)

cohort = 10
cohort + 40 + 19

ct10 = cty[model_time == cohort + 40 + 19 + 1]
c10 = income[cohort == 10]
summary(c10$birthdate + 19)

# 40%
prop.table(table(c10[, .(parent_type, kid_type)]), 1)

c10[, rank_parent := perc.rank(parent_income)]
c10[, rank_kid := perc.rank(kid_income)]
c10[, rank_parent_c := perc.rank(parent_income), county]
c10[, rank_kid_c := perc.rank(kid_income), county]

names(cty)
a = c10[, .(rank_slope_a = reg(rank_kid, rank_parent),
    rank_absolute_mob_a = reg(rank_kid, rank_parent, relative = FALSE),
    rank_correlation_a = cor(rank_parent_c, rank_kid_c),
    kid_income_a = mean(kid_income), parent_income_a = mean(parent_income)), county]
setorder(a, county)

setorder(ct10, county)
b = ct10[, .(county, rank_slope, rank_correlation, rank_absolute_mob, mean_income)]
test = merge(a, b, by = "county")
cor(test[ , .(rank_slope_a, rank_slope)])
cor(test[ , .(rank_correlation_a, rank_correlation)])
cor(test[ , .(rank_absolute_mob_a, rank_absolute_mob)])

cor(test[, .(mean_income, parent_income_a)])
cor(test[, .(mean_income, kid_income_a)])

# mortality
anyDuplicated(m$id)
names(m)

mc = m[birthdate > 10 & birthdate <= 10 + 20]
summary(mc$birthdate+19)
summary(mc$model_time)

table(cty$model_time)

b = cty[model_time == 150, .(county, le)]
setorder(b, county)
b
a = mc[, .(age_a = mean(age), .N), county]
setorder(a, county)
a

t = merge(a, b, by = "county")
cor(t[,.(le, age_a)])

# individual mortality
head(m[model_time > 500])
m[, lincome := logIncome(income)]
m[, county_lincome := logIncome(county_mean_income)]

summary(m[model_time > 500]$total_rank_slope_exposure)
m[, status := 1]

table(m$nmoves)
table(m$nmoves_kid)

m1 = coxph(Surv(age, status) ~ total_rank_slope_exposure + lincome + county_lincome, data = m[model_time > 500])
m2 = coxph(Surv(age, status) ~ county_rank_slope + lincome + county_lincome, data = m[model_time > 500])
m3 = coxph(Surv(age, status) ~ exposure_rank_slope + lincome + county_lincome, data = m[model_time > 500])

screenreg(list(m1, m2, m3))

# county level
ct  = cty[model_time > 500]
ct[, lincome := logIncome(mean_income)]
c1 = lmer(le ~ rank_slope + gini + lincome + as.factor(model_time)  + (1|county), data = ct)
screenreg(list(c1))

# income, transition matrix relationship
table(income$cohort)
prop.table(table(income[, .(parent_type, kid_type)]), 1)
test = income[cohort > 200]

test[, rank_parent := perc.rank(parent_income)]
test[, rank_kid := perc.rank(kid_income)]
test[, rm := reg(rank_kid, rank_parent), county]

test[, p1 := addDiagonalProbs(parent_type, kid_type), county]
test[, p2 := addDiagonalProbs(parent_type, kid_type, 2), county]
test[, p3 := addDiagonalProbs(parent_type, kid_type, 3), county]
test[, p4 := addDiagonalProbs(parent_type, kid_type, 4), county]
test[, p5 := addDiagonalProbs(parent_type, kid_type, 5), county]
test[, N := 1:.N, county]
ct = test[N == 1]
dim(ct)
hist(ct$rm)

cor(ct[, .(p1, rm)])
cor(ct[, .(p2, rm)])
cor(ct[, .(p3, rm)])
cor(ct[, .(p4, rm)])
cor(ct[, .(p5, rm)])

plot(ct[, .(p1, rm)])
plot(ct[, .(p5, rm)])




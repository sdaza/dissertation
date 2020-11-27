##############################
# generative model income mobility and mortality
# mortality and smoking differentials by income
# author: sebastian daza
##############################


library(data.table)
library(ggplot2)

path = "models/MobHealthRecycling/output/verification/testing/"
m = fread(paste0(path, "mortality.csv"))

# duplicates
anyDuplicated(m$id)
prop.table(table(m[, smoker]))

table(m$generation)

# age average by group
m[, mean(age), income_type]
m[, mean(age), .(smoker)]


test = m[, .(age = mean(age)), .(income_type, smoker)]
setorder(test,  income_type)

prop.table(table(m[,.(parent_income_type, income_type)]), 1)

# exposure association with smoking
summary(m[exposure_relative_income_mob == 0, age])
hist(m[age > 17, exposure_relative_income_mob])

table(m$smoker)

names(m)
hist(m$county_relative_income_mob)
hist(m$exposure_relative_income_mob)

ggplot(m[age >= 30], aes(exposure_relative_income_mob, county_relative_income_mob)) +
    geom_point(alpha = 0.3) +
    theme_minimal()

cor(m[, .(county_relative_income_mob, exposure_relative_income_mob)])

summary(m$income)
m[, lincome := ifelse(income == 0, log(1), log(income))]
summary(m$lincome)
summary(glm(smoker ~ exposure_relative_income_mob + as.factor(income_type), data = m, family = "binomial"))

odds = exp(0 + 0.22/8.6 * 10)
odds / (1 + odds)

odds = exp(0 + 0.22/0.086 * 0.10)
odds / (1 + odds)


# only two percent of people
nrow(m[age <30]) / nrow(m)

# county data
cty = fread("models/MobHealthRecycling/output/verification/testing/county.csv")
summary(cty$relative_income_mob)
hist(cty$relative_income_mob)


# pretty high NSI
summary(cty$nsi)
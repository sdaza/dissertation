###############################
# testing output
# author: sebastian daza
##############################


library(data.table)

# read family data
f = fread("models/MobHealthRecycling/output/family.csv")
names(f)

table(f$kid_age)

# check random distribution
prop.table(table(f[parent_generation == 0, parent_type]))

# rigth distribution
prop.table(table(f[parent_generation %in% c(1:10), parent_type]))

# event distribution
prop.table(table(f[kid_age > 0, .(parent_type, kid_type)]), 1)
prop.table(table(f[kid_age == 0, .(parent_type, kid_type)]), 1)

hist(f[parent_generation == 2, parent_age])

# read mortality data
m = fread("models/MobHealthRecycling/output/mortality.csv")
print(paste0("Date of simulation: ", m$date[1]))

prop.table(table(m[generation == 0, smoker]))

mean(m[generation == 4, age])

m[generation == 4, mean(age), income_type]
m[generation == 6, mean(age), .(smoker)]

prop.table(table(m[generation == 4, .(income_type, smoker)]), 1)

test = m[generation == 5, .(age = mean(age)), .(income_type, smoker)]
setorder(test,  income_type)
test

hist(m[generation == 2, age], breaks = 10)

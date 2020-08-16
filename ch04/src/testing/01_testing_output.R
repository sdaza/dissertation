###############################
# testing output
# author: sebastian daza
##############################


library(data.table)

# read data
f = fread("models/MobHealthRecycling/output/family.csv")
names(f)

table(f$kid_age)

# check random distribution
prop.table(table(f[parent_generation == 0, parent_type]))

# rigth distribution
prop.table(table(f[parent_generation %in% c(1:5), parent_type]))

# event distribution
prop.table(table(f[kid_age > 0, .(parent_type, kid_type)]))
prop.table(table(f[kid_age == 0, .(parent_type, kid_type)]))

hist(f[parent_generation == 2, parent_age])
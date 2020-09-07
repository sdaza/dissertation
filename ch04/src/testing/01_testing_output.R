###############################
# income mobility and health
# testing output
# author: sebastian daza
################################


library(data.table) 

# read data
m = fread("ch04/models/MobHealthRecycling/output/mortality.csv")

# no duplicates
anyDuplicated(m[, id])

names(m)
summary(m$id)
table(m$generation)

testing = m[generation <= 35, .(le = mean(p_age), kids = mean(nkids)), generation]
testing

# family 
f = fread("ch04/models/MobHealthRecycling/output/family.csv", fill = TRUE)

testing = f[, .(age = mean(parent_age)), parent_generation]

anyDuplicated(f[, .(kid_id, kid_generation)])

# county
c = fread("ch04/models/MobHealthRecycling/output/county_data.csv")
c

# migration 
m = fread("ch04/models/MobHealthRecycling/output/individual_migration.csv")
m

# estimation of max population
popMaxPopulation = function(popcount = 100, row = 5, col = 5) {
    return(popcount * row * col * 4)
}

popMaxPopulation(popcount = 400)

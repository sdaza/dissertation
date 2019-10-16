###################################
# individual income mobility paper
# nlsy97 analysis
# author: sebastian daza
# version: 0.01
###################################

dat = readRDS('ch03/output/data/nlsy97_analytic.rd')

dim(dat)

summary(ldat$s_rank)

x <- ldat[agei < 20, .(moves = max(moves, na.rm = TRUE)), id]
prop.table(table(x$moves > 0)) # 26%

hist(ldat[, agei])
hist(ldat[year == 2013, agei])
summary(ldat[year == 2013, agei]) # 27-31 years

# some plots
ggplot(ldat, aes(y = health, x = s_rank)) + geom_jitter()

ldat
# # define simple model to explore (ordinal model?)
m1 <- lm(health  ~ male + agei + s_rank, data = ldat[s == 1]) #
summary(m1)

# exploring ids
ids = unique(ldat$id)

ldat[id == sample(ids, 1), .(id, fips, age, s_rank, time, year)]
##############################
# generative model income mobility and mortality
# incoem distribution
# author: sebastian daza
##############################


# libraries
library(data.table)
library(haven)
library(ipumsr)
library(reldist)

# functions
table = function (...) base::table(..., useNA = 'ifany')
cor = function(...) stats::cor(..., use = "complete.obs")
perc.rank = function(x) trunc(rank(x))/length(x)

# read ipums data
ddi = ipumsr::read_ipums_ddi("data/ipums/usa_00001.xml")
ip = ipumsr::read_ipums_micro(ddi)
ip = data.table(ip)

setnames(ip, names(ip), tolower(names(ip)))

# individual income
ip[inctot == 9999999, inctot := NA]
ip[inctot <0, inctot := 0]
ip = ip[!is.na(inctot) & inctot > 0]
gini(ip$inctot, weights = ip$perwt)

dim(ip)
names(ip)

# table(ip$statefip)
# ip[, cty:= paste0(statefip, countyfip)]
# length(unique(ip$cty))
# table(ip[statefip == 4, cty])

ip[, incomeGroup3 := cut(inctot, breaks = quantile(inctot,
    probs = 0:3/3), labels = 1:3, right = TRUE, include.lowest = TRUE)]
ip[, incomeGroup4 := cut(inctot, breaks = quantile(inctot,
    probs = 0:4/4), labels = 1:4, right = TRUE, include.lowest = TRUE)]
ip[, incomeGroup5 := cut(inctot, breaks = quantile(inctot,
    probs = 0:5/5), labels = 1:5, right = TRUE, include.lowest = TRUE)]

ip[, .(min(inctot), max(inctot)), incomeGroup5]

table(ip$year)
s = ip[, .(incomeGroup3, incomeGroup4, incomeGroup5, inctot, perwt)]
setnames(s, names(s), c("incomeType3", "incomeType4", "incomeType5", "ind_income", "weight"))
s = s[sample(250000)]
hist(s[incomeType5 == 1, ind_income])
dim(s)

head(s)
openxlsx::write.xlsx(s, "data/incomeDistribution.xlsx", row.names = FALSE)
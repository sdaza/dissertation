###############################
# ABM income mobility and health
# transition matrices commuting zones
# author: sebastian daza
##################################


library(data.table)
library(xlsx)

table = function (...) base::table(..., useNA = 'ifany')
cor = function(...) stats::cor(..., use = "complete.obs")

correct_decimals = function(x) {
    as.numeric(gsub(",", "\\.", x))
}

# baseline transition matrix
values = c(0.337,	0.280,	0.184,	0.123,	0.075,
    0.242,	0.242,	0.217,	0.176,	0.123,
    0.178,	0.198,	0.221,	0.220,	0.183,
    0.134,	0.160,	0.209,	0.244,	0.254,
    0.109,	0.119, 0.170,	0.236,	0.365)

t = matrix(values, 5, 5, byrow =  TRUE)

diag(t) = 0

v = apply(t, 1, sum)
for (i in seq_along(v)) {
 t[i,] = t[i, ] /  v[i]
}

apply(t, 1, sum)
diag(t) = 1.0

# read data and transform transition matrices
covs = fread("data/cz_covs.csv")
im = fread("data/cz_income_mobility.csv")
im = im[, lapply(.SD, correct_decimals)]
im = im[complete.cases(im)]

# transition matrices
t = fread("data/cz_transition_matrix.csv")
n = names(t)
n = gsub("^P\\(Child |Par |\\)", "", n)
n = gsub(" \\|", "_", n)
setnames(t, names(t), n)
setnames(t, "Children in 1980-85 Cohorts", "children")

t[, children := gsub("\\.", "", children)]
t[, children := as.numeric(children)]
setnames(t, names(t), tolower(names(t)))
tt = melt(t, id.vars = c("cz", "children"),
    measure = patterns("^q1_q[1-5]","^q2_q[1-5]", "^q3_q[1-5]", "^q4_q[1-5]",
    "^q5_q[1-5]"),
    variable.name = "parent",
    value.name = c("q1", "q2", "q3", "q4", "q5"))
tt = tt[complete.cases(tt)]

tt[cz == 21302]

write.xlsx(tt, "data/cz_transition_matrices.xlsx", row.names = FALSE)

# merge with covs
setnames(covs, names(covs), tolower(names(covs)))
covs[, pop := as.numeric(gsub("\\.", "", pop))]
vars = c("black", "racial_seg", "income_seg", "seg_pov", "seg_affluence",
    "prop_comute_15", "hhincome", "gini", "crime", "income_growth")
covs[, (vars) := lapply(.SD, correct_decimals), .SDcols = vars]

dim(tt)
tt = merge(tt, covs, by = "cz", all.x = TRUE)
tt = merge(tt, im, by = "cz", all.x = TRUE)
dim(tt)

tt[, lincome := log(hhincome)]

tp = melt(tt, id.vars = c("cz", "pop", "rm", "am", "parent", "black", "income_seg", "seg_pov", "lincome"),
    measure = patterns("^q[1-5]"), value.name = "prop", variable.name = "child")
tp[, child := as.numeric(gsub("q", "", child))]
write.xlsx(tp, "data/cz_long_format_transition_matrices.xlsx", row.names = FALSE)

# testing
sum(tp[cz == 30100 & parent ==1, .(cz, parent, child, prop)]$prop)

# exploring data
hist(tp[parent == 5 & parent == child, prop])
hist(tp[parent == 1 & parent == child, prop])

tp[parent == ]

props = seq(0.1, 0.9, 1/10)
datasets = list()
for (i in seq_along(props)) {
    datasets[[i]] = tp[, .(prop = quantile(prop, props[i])), .(parent, child)][, percentile := props[i] * 100]
}
stp = rbindlist(datasets)
stp

stp[parent == 1]

setorder(s, parent)



cor(tp[parent == child, .(lincome, income_seg, black, prop, rm)])
tp[parent == child, .(mean(prop)), parent]
plot(tp[parent == child, .(log_prop_black = log(black),
    log_prop = log(prop))])

plot(tp[parent %in% c(1,5) & parent == child, .(log_prop_black = log(black),
    log_prop = log(prop))])

plot(tp[parent %in% c(1,5) & parent == child, .(log_income = lincome,
    log_prop = log(prop))])

cor(tp[parent == 1 & parent == child, .(prop, rm)])
cor(tp[parent == 2 & parent == child, .(prop, rm)])
cor(tp[parent == 3 & parent == child, .(prop, rm)])
cor(tp[parent == 4 & parent == child, .(prop, rm)])
cor(tp[parent == 5 & parent == child, .(prop, rm)])

cor(tp[parent == 1 & parent == child, .(prop, black)])
cor(tp[parent == 1 & parent == child, .(rm, black)])

plot(tp[parent == 1 & parent == child, .(prop, rm)])
plot(tp[parent == 2 & parent == child, .(prop, rm)])
plot(tp[parent == 5 & parent == child, .(prop, rm)])

cor(tt[parent == 5, .(black,  q5)])
cor(tt[parent == 1, .(black, q1)])

summary(tt[parent == 1, q1])
summary(tt[parent == 5, q5])

tp[cz == 16802]

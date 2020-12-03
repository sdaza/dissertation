###############################
# ABM income mobility and health
# transition matrices commuting zones
# author: sebastian daza
##################################


library(data.table)
library(xlsx)

# read data and transform transition matrices
covs = fread("data/cz_covs.csv")

# transition matrices
t = fread("data/transition_matrix_income.csv")
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
write.xlsx(tt, "data/transition_matrices_cz.xlsx", row.names = FALSE)

# merge with covs
setnames(covs, names(covs), tolower(names(covs)))
covs[, pop := as.numeric(gsub("\\.", "", pop))]
vars = c("black", "racial_seg", "income_seg", "seg_pov", "seg_affluence",
    "prop_comute_15", "hhincome", "gini", "crime", "income_growth")
covs[, (vars) := lapply(.SD, function(x) as.numeric(gsub(",", "\\.", x))), .SDcols = vars]

dim(tt)
tt = merge(tt, covs, by = "cz", all.x = TRUE)
dim(tt)

tt[, lincome := log(hhincome)]

tp = melt(tt, id.vars = c("cz", "pop", "parent", "black", "income_seg", "seg_pov", "lincome"),
    measure = patterns("^q[1-5]"), value.name = "prop", variable.name = "child")
tp[, child := as.numeric(gsub("q", "", child))]
write.xlsx(tp, "data/long_transition_matrices_cz.xlsx", row.names = FALSE)

# exploring data
hist(tp[parent == 5 & parent == child, prop])
hist(tp[parent == 1 & parent == child, prop])

s = tp[, quantile(prop, c(0.95)), .(parent, child)]
setorder(s, parent)
s

cor(tp[parent == child, .(lincome, income_seg, black, prop)])
tp[parent == child, .(mean(prop)), parent]
plot(tp[parent == child, .(log_prop_black = log(black),
    log_prop = log(prop))])

plot(tp[parent %in% c(1,5) & parent == child, .(log_prop_black = log(black),
    log_prop = log(prop))])

plot(tp[parent %in% c(1,5) & parent == child, .(log_income = lincome,
    log_prop = log(prop))])

cor(tp[parent == 1 & parent == child, .(lincome, prop)])
cor(tp[parent == 5 & parent == child, .(lincome, prop)])

plot(tp[parent == 1 & parent == child, .(lincome, prop)])
plot(tp[parent == 5 & parent == child, .(lincome, prop)])

cor(tt[parent == 5, .(black,  q5)])
cor(tt[parent == 1, .(black, q1)])

summary(tt[parent == 1, q1])
summary(tt[parent == 5, q5])

t = fread("data/transition_matrix_income.csv")
n = names(t)
n = gsub("^P\\(Child |Par |\\)", "", n)
n = gsub(" \\|", "_", n)
setnames(t, names(t), n)

setnames(t, "Children in 1980-85 Cohorts", "children")
t[, children := gsub("\\.", "", children)]
t[, children := as.numeric(children)]


summary(t$q1_q1)
summary(t$q2_q2)
summary(t$q3_q3)
summary(t$q4_q4)
summary(t$q5_q5)

t[q1_q1 < 0.20]

summary(t$children)

# {0.30, 0.175, 0.175, 0.175, 0.175},
# {0.175, 0.30, 0.175, 0.175, 0.175},
# {0.175, 0.175, 0.30, 0.175, 0.175},
# {0.175, 0.175, 0.175, 0.30, 0.175},
# {0.175, 0.175, 0.175, 0.175, 0.30}
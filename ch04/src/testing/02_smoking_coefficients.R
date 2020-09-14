# difference between highest and lowes income
log((22.3+18.8)/(7.1+21.4))

current_smokers = c(22.3, 11.3, 17.8, 14.0, 13.2, 7.1)
former_smokers = c(18.8, 22.0, 21.4, 22.1, 23.8, 21.4)

smokers = current_smokers + former_smokers
smokers
avg_middle_group = mean(smokers[2:5])
avg_middle_group

log(smokers[1]/avg_middle_group)
log(smokers[6]/avg_middle_group)
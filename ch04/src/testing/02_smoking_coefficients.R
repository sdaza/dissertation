

# difference between highest and lowes income
log((22.3+18.8)/(7.1+21.4))

1 Less than $35,000 22.3 (0.65) 16.9 (0.57) 5.4 (0.36) 18.8 (0.58) 58.9 (0.79)
2 $35,000 or more   11.3 (0.34) 8.2 (0.27) 3.1 (0.18)  22.0 (0.39) 66.7 (0.50)
3 $35,000–$49,999   17.8 (0.94) 13.4 (0.83) 4.4 (0.50) 21.4 (0.92) 60.7 (1.15)
4 $50,000–$74,999   14.0 (0.68) 11.0 (0.60) 2.9 (0.32) 22.1 (0.76) 63.9 (0.96)
5 $75,000–$99,999   13.2 (0.80) 9.6 (0.68) 3.6 (0.45)  23.8 (0.92) 62.9 (1.07)
6 $100,000 or more  7.1 (0.43) 4.5 (0.32) 2.6 (0.27)   21.4 (0.61) 71.5 (0.73)

current_smokers = c(22.3, 11.3, 17.8, 14.0, 13.2, 7.1)
former_smokers = c(18.8, 22.0, 21.4, 22.1, 23.8, 21.4)

smokers = current_smokers + former_smokers
smokers
avg_middle_group = mean(smokers[2:5])
avg_middle_group

log(smokers[1]/avg_middle_group)
log(smokers[6]/avg_middle_group)
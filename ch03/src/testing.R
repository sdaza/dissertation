
simulate_simple <- function(n) {
Z1 <- rbinom(n = n, size = 1, prob = 0.3)
Z2 <- rbinom(n = n, size = 1, prob = plogis(0 + 0.2*Z1))
A <- rbinom(n = n, size = 1, prob = plogis(-1 + 0.7*Z1 + 0.8*Z2))
Y <- rbinom(n = n, size = 1, prob = plogis(-0.5 + 1*Z1 + 0.7*Z2 + 1.3*A))
return(data.frame(Z1, Z2, A, Y))
}

# Simulate a data set
set.seed(456)
simple_df <- simulate_simple(n = 5000)

# Package data for Stan
stan_dat <- list(N = nrow(simple_df), P = 3,
X = cbind(1, simple_df$Z1, simple_df$Z2), A = simple_df$A,
Y = simple_df$Y)


 # Calculate frequentist ATE
ffit1 <- glm(Y ~ 1 + Z1 + Z2 + A, family = binomial(link = "logit"), data = simple_df)
fcoef <- coef(ffit1)

fATE <- mean(plogis(cbind(1, simple_df$Z1, simple_df$Z2, 1) %*% fcoef) -
       plogis(cbind(1, simple_df$Z1, simple_df$Z2, 0) %*% fcoef))

print(fATE)


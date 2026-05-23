#################################################################
## ST II
## Lab 2
## Martynas Baltramaitis
# 2024-03-07
#################################################################

# Values of y
y <- seq(1, 5)

# Probability of y
py <- c(0.1, 0.2, 0.1, 0.2, 0.4)

# P(1<Y<5)
which(y > 1 &  y < 5)
sum(py[which(y > 1 &  y < 5)])
py[which(y > 1 &  y < 5)]


# E(Y)
my<-sum(y*py)

#Var(Y)
vary <- sum(y^2 * py) - my^2

################################################################
set.seed(123)

size <- 10^5

ys <- sample(y, size, replace = TRUE, prob = py)
ys
?set.seed

table(ys)/size

## Calculate P(1<Y<5)

#Method 1: P(Y=2)+P(Y=3)+P(Y=4)
d2 <- which(ys == 2)
d3 <- which(ys == 3)
d4 <- which(ys == 4)


P1 <- (length(d2)+length(d3)+length(d4))/length(ys)

#E(ys) and Var(ys), using mean and var f-ns

ms <- mean(ys)
ms

vars <- var(ys)
vars

################################################################

# Create W = 2Y + 5
W <- 2*ys + 5

mean(W)
var(W)

###############################################################33
#Values of X
x <- seq(0., 4, 1)

#Prior
prior_x <- rep(1/5, 5)

#Likelihood
likelihood <- rep(0, 5)

for (i in 0:4) {
  likelihood[i+1] <- i/4
}
likelihood

# Or
likelihood <- x/4
likelihood

# Prior * likelihood
weight_x <- prior_x*likelihood

posterior_x <- weight_x / sum(weight_x)
posterior_x

results_Q2 <- cbind(x, prior_x, likelihood, weight_x, posterior_x)
results_Q2

colnames(results_Q2)[5] <- "Posterior X"

?base::plot

plot(x, posterior_x, #Coordinates
     col = "Blue",
     type = "b",
     ylim = c(0, 1), #Set y axis limit
     xlab = "x",
     ylab = "y",
     main = "Posterior distribution"
     )

#Plot prior
lines(x, prior_x, col = "red")

################################################################
## Poisson
################################################################

mu <- c(1, 1.5, 2, 2.5)

#Prior
prior_mu <- c(2/6, 1/6, 2/6, 1/6)

#likelihood
likelihood_mu <- rep(0, length(mu))

for (i in 1:4) {
  likelihood_mu[i] <- dpois(1, mu[i])
}

weight_mu <- prior_mu * likelihood_mu

#Posterior

posterior_mu <- weight_mu / sum(weight_mu)
posterior_mu

###############################################################
## Question 4
###############################################################
pi <- c(0.3, 0.5)
prior_pi <- c(0.6, 0.4)
likelihood_pi <- rep(0, 2)

for (i in 1:2) {
  likelihood_pi[i] <- dbinom(2, 3, pi[i])
}
weight_pi <- prior_pi * likelihood_pi

posterior_pi <- weight_pi / sum(weight_pi)
posterior_pi

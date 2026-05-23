################################
## Lab 5
## 2024-09-19
################################
#Question 1
################################

#Load the data
y <- c(64, 72, 84, 73, 98, 85, 85, 94, 72, 93)

# Prior mean
m <- 80

# Prior sd
s <- 10

# Prior:
r <- m^2 / s^2
v <- m / s^2

# Posterior:
sum_y <- sum(y)
n <- length(y)

r1 <- r + sum_y
v1 <- v + n

# 95% CI
qgamma(0.025, r1, v1)
qgamma(0.975, r1, v1)

pgamma(75, r1, v1)

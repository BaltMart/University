################################
# Lab 4
# 2024-04-12
################################

#Find P(0 < Z < 1.52)

pnorm(1.52, mean = 0, sd = 1) - pnorm(0, 0, 1)
pnorm(1.52)

#Find P(Z>2.11)

1 - pnorm(2.11, 0, 1)
pnorm(2.11, 0, 1, lower.tail = FALSE)

#Find 2.5 and 97.5 percentiles

qnorm(0.025, 0, 1)
qnorm(0.975, 0, 1)
###############################
# QUESTION 2
###############################

# punif 
punif(60, 0, 60) - punif(57, 0, 60)

###############################
# QUESTION 3
###############################

# beta
a<- 10
b<- 12
Ey <- a/(a+b)
Vary <- a*b/((a+b)^2 * (a+b+1))

#Sample of Y
set.seed(123)
Y <- rbeta(1000, a, b)

M <- mean(Y)
V <- var(Y)

#Plot historogram:
hist(Y, main = "Density", freq = FALSE)

# Plot Y ~ beta(a, b)

x <- seq(0, 1, 0.01)
curve(dbeta(x, a, b), add = TRUE, col = "blue")

#Plot an approximation:
curve(dnorm(x, Ey, sqrt(Vary)), add = TRUE, col = "red")

legend("topright", 
       col = c("black", "blue", "red"), 
       lty = c(1, 1, 1),
       legend = c("Beta histogram",
                  "Beta density",
                  "Norm. approximation"))
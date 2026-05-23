###############################
## Lab 6
## 2024-05-03
###############################
#Linear regression
###############################
#Question 1

setwd("C:/Users/martynas/Desktop/uni/Statistika")

income_data = read.table("income_data.txt", header = TRUE)

summary(income_data)

plot(happiness ~ income, data = income_data)

# Classical approach

reg_class = lm(happiness ~ income, data = income_data)

# if we don't want intercept:
# lm(happiness ~ income - 1, data = income_data)

summary(reg_class)

# Classical approach by hand

Y = as.matrix(income_data$happiness)
X = as.matrix(cbind(1, income_data$income))

beta = solve(t(X) %*% X) %*% t(X) %*% Y

# Calculate the residuals
res = Y - (beta[1] + beta[2] * X[, 2])

plot(res)

# Bayesian approach ------------------

B0 = c(0.5, 0.5)
# Sigma0 = matrix(c(0, 0, 0, 0),
#                 ncol = 2,
#                 byrow = TRUE)

Sigma_inv = matrix(c(0, 0, 0, 0),
                ncol = 2,
                byrow = TRUE)


# non-informative prior

V_star = solve(Sigma_inv + t(X) %*% X)

M_star = V_star %*% (Sigma_inv %*% B0 + t(X) %*% Y)






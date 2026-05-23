##################################
# Lab 4
# 2024-04-12


binbetaf <- function(a, b, n, y, alpha){
  #Inputs
  # a: parameter of beta prior
  # b: parameter of beta prior
  # n: number of trials
  # y: number of succsesses
  # alpha: credible interval (1 - alpha) e.g. 0.05
  # Parameters of posterior distribution  ~ beta(a1, b1):
  a1 <- a + y
  b1 <- b + n - y
  
  # Parameters of posterior mean and variance:
  m_pos <- a1 / (a1 + b1)
  var_pos <- a1 * b1 / ((a1+b1)^2 * (a1 + b1 +1))
  sd_pos <- sqrt(var_pos)
  
  # Weights:
  w_prior <- (a + b) / (a + b + n)
  w_data <- n / (a + b + n)
  
  # Credible intervals:
  qlow <- alpha / 2
  qup <- 1 - alpha / 2
  Low <- qbeta(qlow, a1, b1)
  Up <- qbeta(qup, a1, b1)
  
  # # Output:
  # print('Parameters of posterior distribution')
  # print(c(a1, b1))
  # 
  # print('Parameters of posterior mean and variance')
  # print(m_pos)
  # print(var_pos)
  # 
  # print('Prior weight')
  # print(w_prior)
  # 
  # print('Data weight')
  # print(w_data)
  # 
  # print('CI')
  # print(c(Low, Up))
  
  return(c(a_posterior = a1,
              b_posterior = b1))
}
  
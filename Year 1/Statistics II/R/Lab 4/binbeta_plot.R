binbeta_plot <- function(a, b, n, y, pp){
  #Inputs
  # a: parameter of beta prior
  # b: parameter of beta prior
  # n: number of trials
  # y: number of succsesses
  # pp: position of the legend
  
  #Calculate prior density
  pi <- seq(0, 1, 0.001)
  y_prior <- dbeta(pi, a, b)
  
  #Parameters of posterior:
  a1 <- a + y
  b1 <- b + n -y
  
  #Calculate posterior density
  y_pos <- dbeta(pi, a1, b1)
  
  ma <- max(y_prior, y_pos)
  
  #Plot :
  plot(pi, y_prior, 
       col = "red", 
       ylab = "",
       ylim = c(0, ma)) #Prior
  lines(pi, y_pos, col = "blue", ylab = "")
  legend(pp, 
         legend = c("Prior", "Posterior"),
         col = c("red", "blue"),
         lty = c(1, 1))
  
}
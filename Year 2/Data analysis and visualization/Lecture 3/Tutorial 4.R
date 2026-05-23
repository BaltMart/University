#Getting to know the data set during lecture

#Loading libraries
library(dplyr)
library(tidyverse)
library(tidyr)
library(haven)
library(ggplot2)
library(sandwich)


#Loading the data file
df_lec <- read_dta("mip_educ16_en.dta")

#Generating clean variables
df_lec1 <- df_lec |>
  mutate(ind4 = as.factor(bran_4),
         big_data = as.factor(digia11),
         ost = as.factor(ost))

#Plotting a histogram
hist(df_lec$bges, main = paste("Histogram of Number of Employees per firms"), 
     xlab = "Number of employees per firms")

#Calculating the correlation between the number of employees and inovation expenditures
cor.test(x = df_lec$bges, y = df_lec$iages)


#Depicting a bar chart how many firms used big data analytics in 2015
table(df_lec1$big_data)

ggplot(df_lec1, aes(x = big_data)) +
  geom_bar()


## Lecture 2
# Reproducing a picture
x = 0:100
lambda = c(3, 7, 15, 50)
poissonData = expand.grid(x = x, lambda = lambda)
poissonData$prob = dpois(poissonData$x, poissonData$lambda)

ggplot(data = poissonData, aes(x = x, y = prob, fill = factor(lambda))) +
  geom_bar(stat = "Identity", position = "Dodge",) +
  theme_minimal() +
  labs(title = "Comparison of Two Poisson Distributions", 
       x = "Number of Events (k)", 
       y = "Probability")
  
#f(x) = x + x^3 + 5
X <- c(-1.8, -1.1, -0.6, 0.1, 0.9, 1.3, 1.7)
meanx = mean(X)

dfx = (1 + 3*X^2)
meandfx = mean(dfx)

dfxAtMean = 1 + 3*meanx^2
dfxAtMean


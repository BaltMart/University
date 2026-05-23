#######################################
## Lab project
## Martynas Baltramaitis
#######################################
## Question 1
#######################################
# Task 1
# a)
Xn = c(rep(1, 10000)) #Generating a sequences
Xn[2] = 2 # Assigning a required value

for (n in 3:10000) # Updating sequence by the given formula
{
  Xn[n] = ((0.5 + 4 * (n - 1) - (Xn[n-2] / Xn[n-1])^(Xn[n-1] * 1.1)) / Xn[n-1])
}

# Printing out numbers 500 and 5000 of our sequence
print(paste0("Xn[500] = ", round(Xn[500], 2)))
print(paste0("Xn[5000] = ", round(Xn[5000], 2)))

# b) Plotting the behavior of the sequence
plot(Xn, type = 'l',
     xlab = 'Index', ylab = 'Values of the Xn',
     main = 'Plot of the sequence',
     col = 'green')

# Legend
legend("topleft",
       col = 'green',
       lty = 1,
       legend = 'Xn')

# c) Calculating sum of first 1000 elements, the median and standard deviation
# of our sequence
sum_1000 = sum(Xn, 1:1000)
median_Xn = median(Xn)
sd_Xn = sd(Xn)

# Task 2
# a)

y = Xn[seq(100, length(Xn), by = 100)]
z = Xn[seq(99, length(Xn), by = 100)]

# b)
Y = matrix(y, nrow = 10, byrow = TRUE)
Z = matrix(z, nrow = 10, byrow = TRUE)


D = t(Y) - Z
print(paste0("D[4,4] = ", D[4, 4]))

# Task 3
# a)
mu = y[seq(1, 25)]/10 # Setting mu values

prior_mu = c(rep(1/25, 25))

likelihood_mu = c(rep(0, length(mu)))

for (i in 1:25)
{
  likelihood_mu[i] = dpois(2, mu[i])
}

posterior_mu = (likelihood_mu * prior_mu) / sum(likelihood_mu * prior_mu)

table_mu = data.frame('Value of mu' = mu, 
                      Prior = prior_mu, 
                      Likelihood = likelihood_mu,
                      Posterior = posterior_mu)

# b)
# setting variables up to find the value of mu that will yield the highest posterior probability
highest_pos  = 0
highest_pos_mu_ind = 0

for (i in 1:25) # This will compare and save an index of the mu we are looking for
{
  if (posterior_mu[i] > highest_pos)
  {
    highest_pos = posterior_mu[i]
    highest_pos_mu_ind = i
  }
}

print(paste0("This value of mu yields the highest posterior probability: ", 
      mu[highest_pos_mu_ind]))

#######################################
## Question 2
#######################################
# Task 1 
# a)
#--------------------------------------
# Creating a function
#--------------------------------------
binbetaf = function(a, b, n, y){
  #Inputs:
  #a: parameters of beta prior distribution
  #b: parameters of beta prior distribution
  #n: number of trials
  #y: number of successes
  #Parameters of posterior distribution ~ Beta(a1, b1)
  a1 = a + y
  b1 = b + n - y
  
  #Parameters of posterior mean and variance
  m_pos = a1 / (a1 + b1)
  var_pos = a1 * b1 / ((a1+b1)^2 * (a1 + b1 +1))
  
  #Outputs:
  return(list(m_pos = m_pos, var_pos = var_pos))
}
#---------------------------------------
# Input values
a = 1 
b = 1
n = 15
y = 4

# Calculating mean and variance
binbetaf(a, b, n, y)

# b)
# Creating a function
#--------------------------------------
beta_plot = function(a, b, n, y, col_pri , col_pos){
  #Inputs:
  #a: parameters of beta prior distribution
  #b: parameters of beta prior distribution
  #n: number of trials
  #y: number of successes
  
  #Calculate prior density
  pi <- seq(0, 1, 1/15)
  y_prior <- dbeta(pi, a, b)
  
  #Parameters of posterior:
  a1 <- a + y
  b1 <- b + n - y
  
  #Calculate posterior density
  y_pos <- dbeta(pi, a1, b1)
  ma <- max(y_prior, y_pos)
  
  #Plot:
  plot(pi, y_prior,
       col = col_pos,
       ylim = (c(0, ma)),
       ylab = "",
       type = "p")
  lines(pi, y_pos, 
        col = "green", 
        ylab = "")
  
}
beta_plot(a, b, n, y, "orange", "black")

###############################
# Task 2
# a)
# I believe, that a mean of the people that are cat lovers are 0.5
# and the sd = 0.1
prior_mean = 0.5
prior_sd = 0.1
# Creating a function
#------------------------------------
beta_prior = function(prior_mean, prior_sd){
  var_prior = prior_sd^2 #Calculating variance
  
  a = ((1 - prior_mean)/var_prior - 1/prior_mean) * prior_mean^2
  b = a*(1/prior_mean - 1)
  
  return(list(a_prior = a, b_prior = b))
}
#-------------------------------------
prior = beta_prior(prior_mean, prior_sd)

#b)

pnorm(0.2, prior_mean, prior_sd, lower.tail = FALSE) # P(pi>0.2)
pnorm(0.6, prior_mean, prior_sd) # P(pi<0.6)

#c)

#Using function binbetaf and our prior parameters I will calculate posterior mean and variance

posterior_mean_var = binbetaf(prior$a_prior, prior$b_prior, n, y)

#d)

pnorm(0.2, posterior_mean_var$m_pos, sqrt(posterior_mean_var$var_pos), lower.tail = FALSE) #P(pi|data > 0.2)
pnorm(0.6, posterior_mean_var$m_pos, sqrt(posterior_mean_var$var_pos)) #P(pi|data < 0.6)

#e)

qnorm(c(0.16, 0.84), posterior_mean_var$m_pos, sqrt(posterior_mean_var$var_pos))  # 68% CI
qnorm(c(0.05, 0.95), posterior_mean_var$m_pos, sqrt(posterior_mean_var$var_pos))  # 90% CI
qnorm(c(0.025, 0.975), posterior_mean_var$m_pos, sqrt(posterior_mean_var$var_pos))  # 95% CI
qnorm(c(0.005, 0.995), posterior_mean_var$m_pos, sqrt(posterior_mean_var$var_pos))  # 99% CI

# Setting up for comparing with the normal approximation
norm_approx = function(posterior, CI){
  z = qnorm((1 + CI) / 2) #finding the Z for approximation
  left = posterior$m_pos - z * sqrt(posterior$var_pos)
  right = posterior$m_pos + z * sqrt(posterior$var_pos)
  return(c(left, right))
}
#Calculating with approximation

norm_approx(posterior_mean_var, 0.68) # 68% CI
norm_approx(posterior_mean_var, 0.90) # 90% CI
norm_approx(posterior_mean_var, 0.95) # 95% CI
norm_approx(posterior_mean_var, 0.99) # 99% CI

#f)

binom.test(50, 100, p = 0.5, alternative = "less", conf.level = 0.95)

#g and h)
library(ggplot2)

plot = ggplot()

# Survey responses
responses = c('Y', 'N', 'Y', 'Y', 'N', 'N', 'N', 'N', 'Y', 'N', 'N', 'N', 'N', 'N', 'N')

# Update prior with observations, but we know the n and y from previous tasks
posterior = binbetaf(prior$a_prior, prior$b_prior, n, y)

print(paste("Posterior mean: ", round(posterior$m_pos, 3)))
print(paste("Posterior variance: ", round(posterior$var_pos, 3)))

# Now we set up for analyzing one observation at the time
sequential_prior = list(a_prior = prior$a_prior, b_prior = prior$b_prior) # I do it way so that function below can update itself

for (i in 1:length(responses)) {
  response = responses[i] # Here we just take the response from our vector
  
  # We look if some new observation is Yes of No and update based on that and get new posterior mean and variance
  if(response == 'Y') {
     sequential_update = binbetaf(sequential_prior$a_prior, sequential_prior$b_prior, 1, 1)
    }
  else {
     sequential_update = binbetaf(sequential_prior$a_prior, sequential_prior$b_prior, 1, 0)
  }
  
  # Convert the new posterior means to beta parameters we can use later
   sequential_prior = beta_prior(sequential_update$m_pos, sqrt(sequential_update$var_pos))
   
   # Adding new beliefs to plot
   plot = plot + stat_function(fun = dbeta, args = 
                                 list(sequential_prior$a_prior, sequential_prior$b_prior),
                               aes(colour = factor(i)),
                               linewidth= i/length(responses))
}
print(paste("Sequential posterior mean: ", round(sequential_update$m_pos, 3)))
print(paste("Sequential posterior variance: ", round(sequential_update$var_pos, 3)))


# h) display the plot
print(plot)

#####################################
# Question 3
#####################################


library(openxlsx)
library(ggplot2)
library(dplyr)
library(tidyr)

install.packages("tidyverse")
library(tidyverse)

setwd("C:/Users/martynas/Desktop/uni/Statistika")

bank_data_2021 = read.xlsx("Data_LB.xlsx",
                           sheet = "2021Q3",
                           sep.names = " ",
                           rows = c(2:20),
                           cols = c(1:14),
                           colNames = TRUE)%>%
  pivot_longer(cols = 2:13,
               names_to = "Bank name",
               values_to = "Values") %>%
  mutate(Date = "2021-10-01")

bank_data_2022 = read.xlsx("Data_LB.xlsx",
                           sheet = "2022Q3",
                           sep.names = " ",
                           rows = c(2:20),
                           cols = c(1:15),
                           colNames = TRUE) %>%
  pivot_longer(cols = 2:14,
               names_to = "Bank name",
               values_to = "Values") %>%
  mutate(Date = "2022-10-01")

bank_data_2023 = read.xlsx("Data_LB.xlsx",
                           sheet = "2023Q3",
                           sep.names = " ",
                           rows = c(2:20),
                           cols = c(1:15),
                           colNames = TRUE) %>%
  pivot_longer(cols = 2:15,
               names_to = "Bank name",
               values_to = "Values") %>%
  mutate(Date = "2023-10-01")


bank_data_full = bind_rows(bank_data_2021, bank_data_2022, bank_data_2023)

#3.2 

total_assets = bank_data_full %>%
  filter(ID == "Total assets") %>%
  group_by(Date) %>%
  summarise(Total = sum(Values))

#3.3

bank_profits = bank_data_full %>%
  filter(ID == "Profit (loss) of the current year") %>%
  group_by(Date)


ggplot(bank_profits, aes(x = `Date`, y = `Values`, fill = `Bank name`)) +
  geom_col() +
  labs(title = "Total banking sector profit by Bank, mln.",
       x = "Year", y = "Profit/Loss (million EUR)",
       fill = "Bank:") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::comma_format(scale = 1/1000))

#3.5
data_subset = bank_data_full %>%
  filter(`Bank name` %in% c("SWE", "SEB", "REV", "MED", "MNB", "SIA")) %>%
  arrange(desc(Values))

bank_share = data_subset %>%
  filter(ID == "Total assets") %>%
  left_join(total_assets, by = "Date", relationship = "many-to-many") %>%
  mutate(`Market share` = (Values/Total*100)) ## bank share in percent

#3.6
ggplot(bank_share) +
  geom_col(aes(x = `Bank name`, y = `Values`, fill = `Date`), position = position_dodge()) +
  labs(title = "Bank assets by Bank each year, mln. Eur",
       x = "Date", y = " ",
       fill = "Date:") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::comma_format(scale = 1/1000))

#3.7

bank_profits_subset = data_subset %>%
  filter(`Bank name` %in% c("SWE", "SEB", "REV", "MED", "MNB", "SIA")) %>%
  filter(ID == "Profit (loss) of the current year") %>%
  arrange(desc(Values))


ggplot(bank_profits_subset) +
  geom_col(aes(x = `Values`, 
               y = `Date`, 
               fill = `Bank name`),
               position = position_dodge()) +
  labs(title = "Bank profit/loss by year, mln. EUR",
       y = " ") +
  theme_minimal() +
  facet_wrap(~`Bank name`, scales = "free_x") + # scales allows the scales for each bank to be different so that differences can be seen more clearly
  scale_x_continuous(labels = scales::comma_format(scale = 1/1000)) +
  theme(legend.position = "bottom")

#3.8

data_subset_ratios = data_subset %>%
  filter(ID %in% c("Cash balances with central banks", "Loans and advances (including leasing)",
                 "Total assets")) %>%
  pivot_wider(names_from = `ID`, values_from = Values) %>% # Transforming data to new format to calculate ratios
  mutate(`Loans` = `Loans and advances (including leasing)`/`Total assets`*100) %>%
  mutate(`Cash` = `Cash balances with central banks`/`Total assets`*100) %>%
  pivot_longer(cols = 3:7,
               names_to = "ID",
               values_to = "Values") %>%
  filter(ID %in% c("Loans", "Cash"))

# Ploting the ratios of select items
ggplot(data_subset_ratios) +
  geom_col(aes(x = `Date`,
               y = `Values`,
               fill = `ID`)) +
  geom_text(aes(x = `Date`,
                y = `Values`, label = round(Values, 1)), vjust = 0.5, hjust = 0.5, size = 2.5) + ## they overlap, need to fix
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~`Bank name`) 

#3.10

banks_by_group = bank_data_full %>%
  mutate(`Bank group` = case_when(`Bank name` %in% c("SWE", "SEB", "REV", "MED", "MNB", "SIA") ~ "Selected Bank",
                                  TRUE ~ "Other Bank")) %>% # adding a new column
  filter(ID %in% c("Total assets", "Total equity", "Profit (loss) of the current year")) %>%
  pivot_wider(names_from = `ID`, values_from = Values) %>% #transforming data to new format to calculate ratios
  mutate(`Profit to equity ratio` = `Profit (loss) of the current year`/ `Total equity`*100) %>%
  mutate(`Profit to asset ratio` = `Profit (loss) of the current year`/ `Total assets`*100)


# Creating a scatterplot
ggplot(banks_by_group, aes(x = `Profit to equity ratio`, 
                           y = `Profit to asset ratio`,
                           colour = `Bank group`,
                           fill = `Bank group`)) +
  geom_point() + 
  labs(tittle = " ", x = "Profit to equity ratio", y = "Profit to asset ratio") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Highlight y = 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Highlight x = 0
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~`Date`)

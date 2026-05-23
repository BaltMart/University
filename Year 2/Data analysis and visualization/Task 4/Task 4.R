library(tidyquant)
library(tidyverse)
library(timetk)
library(broom)
library(highcharter)
library(tibbletime)
library(glue)
library(scales)
library(stargazer)
library(zoo)
library(forecast)
library(tseries)

#Vector of the ETFs
markSymbols <- sort(c("BOTZ", "WTAI", "IGPT","ROBO","AIQ"))

#Extracting the data from yahoo finance for these ETFs
prices <- 
  getSymbols(markSymbols, src = 'yahoo', from = "2022-12-31", to = "2024-12-31",
             auto.assign = TRUE, warnings = FALSE) %>% 
  map(~Cl(get(.))) %>% #Extracting close price
  reduce(merge) %>% 
  `colnames<-`(markSymbols)

#Adding the date column to be able to plot data
prices_tbl <- prices %>%
  as_tibble(rownames = "date") %>%
  mutate(date = as.Date(date))

#Plotting the closing prices of the 
ggplot(data = prices_tbl, aes(x = date)) +
  geom_line(aes(y = AIQ, color = "AIQ")) +
  geom_line(aes(y = BOTZ, color = "BOTZ")) +
  geom_line(aes(y = ROBO, color = "ROBO")) +
  geom_line(aes(y = IGPT, color = "IGPT")) +
  geom_line(aes(y = WTAI, color = "WTAI")) +
  labs(title = "ETF Prices Over Time at Market Close", 
       x = "Date", 
       y = "Price",
       color = "ETF",
       caption = "Work my on. | Data Source: Yahoo Finance") +
  scale_color_manual(values = c("AIQ" = "blue", "BOTZ" = "red", "ROBO" = "yellow","IGPT" = "green", "WTAI" = "purple")) +
  theme_minimal()

#Creating a time series for close price of AIQ ETF
AIQ_close_ts <- ts(prices_tbl$AIQ, frequency = 250) # Here assuming there are 250 trading days in a year, to be able to decompose the data

#Decomposing
AIQ_close_decomp <- decompose(AIQ_close_ts)
plot(AIQ_close_decomp) #Data clearly not stationary(include why in answer sheet)

#Performing Augmented Dicky-Fuller test for unit root
adf_test <- adf.test(AIQ_close_ts, alternative = "stationary")
print(adf_test)#But this test says the data is stationary

#Transforming the data by lag (or in other world transforming it into growth rate)
AIQ_Close_growth_ts <- diff(log(AIQ_close_ts))
plot(AIQ_Close_growth_ts)

#Doing the ADF again
adf_test_gr <- adf.test(AIQ_Close_growth_ts, alternative = "stationary")
print(adf_test_gr)

acf(AIQ_Close_growth_ts)
pacf(AIQ_Close_growth_ts)

#Calculating returns
asset_returns_long <-  
  prices %>% 
  to.monthly(indexAt = "lastof", OHLC = FALSE) %>% 
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  gather(asset, returns, -date) %>% 
  group_by(asset) %>%  
  mutate(returns = (log(returns) - log(lag(returns)))) %>% 
  na.omit()

asset_returns_wide <- asset_returns_long %>%
  pivot_wider(names_from = asset, values_from = returns)

#Plotting the returns
ggplot(data = asset_returns_wide, aes(x = date)) +
  geom_line(aes(y = AIQ, color = "AIQ")) +
  geom_line(aes(y = BOTZ, color = "BOTZ")) +
  geom_line(aes(y = ROBO, color = "ROBO")) +
  geom_line(aes(y = IGPT, color = "IGPT")) +
  geom_line(aes(y = WTAI, color = "WTAI")) +
  labs(title = "ETF Returns calculated montly by log difference at the market close", 
       x = "Date", 
       y = "Return",
       color = "ETF",
       caption = "Work my on. | Data Source: Yahoo Finance") +
  scale_color_manual(values = c("AIQ" = "blue", "BOTZ" = "red", "ROBO" = "yellow","IGPT" = "green", "WTAI" = "purple")) +
  theme_minimal()
#Summary statistics for these returns
summaryStatAssets <- summary(asset_returns_wide)
summaryDf_1 <- as.data.frame(matrix(unlist(summaryStatAssets),nrow = 6))
summaryDf_1 <- summaryDf_1[, names(summaryDf_1) != "V1"]
names(summaryDf_1) <- markSymbols

sd_val_1 <- c(sd(asset_returns_wide$AIQ),
              sd(asset_returns_wide$BOTZ),
              sd(asset_returns_wide$IGPT),
              sd(asset_returns_wide$ROBO),
              sd(asset_returns_wide$WTAI))
sd_val_1_an <- paste("SD:", sep = " ", round(sd_val_1, 4))

summaryDf_1 <- rbind(summaryDf_1, sd_val_1_an)
print(summaryDf_1)

##Constructing a portfolio No1
#Weights of portfolio
w <- c(0.2, 0.2, 0.2, 0.2, 0.2)

w_1 <- w[1]
w_2 <- w[2]
w_3 <- w[3]
w_4 <- w[4]
w_5 <- w[5]

asset1 <- asset_returns_wide[,2]
asset2 <- asset_returns_wide[,3]
asset3 <- asset_returns_wide[,4]
asset4 <- asset_returns_wide[,5]
asset5 <- asset_returns_wide[,6]

portfolio_returns_byhand <-   
  (w_1 * asset1) + 
  (w_2 * asset2) + 
  (w_3 * asset3) +
  (w_4 * asset4) + 
  (w_5 * asset5)
names(portfolio_returns_byhand) <- "Returns_Portfolio_No1"

#Automatically calculating portfolio standard deviation
portfolio_sd <- StdDev(asset_returns_wide, weights = w)

#summary of the statistics
summary(portfolio_returns_byhand)

summaryStatsPort1 <- summary(portfolio_returns_byhand)
summaryDf_2 <- as.data.frame(matrix(unlist(summaryStatsPort1),nrow = 6))
names(summaryDf_2) <- "Portfolio No1 reb. month. stats"
portfolio_sd_an <- paste("SD:", sep = " ", round(portfolio_sd, 4))
summaryDf_2 <- rbind(summaryDf_2, portfolio_sd_an)
print(summaryDf_2)

#plotting the portfolio performance
asset_returns_wide_combined <- cbind(asset_returns_wide, portfolio_returns_byhand) #putting the returns next to dates

ggplot(data = asset_returns_wide_combined, aes(x = date)) +
  geom_line(aes(y = Returns_Portfolio_No1, color = "Returns_Portfolio_No1")) +
  labs(title = "Porfolio of equal weight ETFs monthly returns", 
       x = "Date", 
       y = "Return",
       color = "ETF",
       caption = "Work my on. | Data Source: Yahoo Finance") +
  scale_color_manual(values = c("Returns_Portfolio_No1" = "blue")) +
  theme_minimal()

## Constructing an alternative portfolio
#Weights of portfolio
wa <- c(0.4, 0.15, 0.15, 0.15, 0.15) #Using my previous logic of wanting to invest in AIQ the most out of all off other ETFs

w_1a <- wa[1]
w_2a <- wa[2]
w_3a <- wa[3]
w_4a <- wa[4]
w_5a <- wa[5]

portfolio_returns_byhand_alt <-   
  (w_1a * asset1) + 
  (w_2a * asset2) + 
  (w_3a * asset3) +
  (w_4a * asset4) + 
  (w_5a * asset5)
names(portfolio_returns_byhand_alt) <- "Returns_Portfolio_No2"

#Automatically calculating portfolio standard deviation
portfolio_sd_alt <- StdDev(asset_returns_wide, weights = wa)

#plotting the portfolio performance
asset_returns_wide_combined_alt <- cbind(asset_returns_wide, portfolio_returns_byhand_alt) #putting the returns next to dates

ggplot(data = asset_returns_wide_combined_alt, aes(x = date)) +
  geom_line(aes(y = Returns_Portfolio_No2, color = "Returns_Portfolio_No2")) +
  labs(title = "Porfolio of 40% weight of AIQ EFT and 15% of each other ETF monthly returns", 
       x = "Date", 
       y = "Return",
       color = "ETF",
       caption = "Work my on. | Data Source: Yahoo Finance") +
  scale_color_manual(values = c("Returns_Portfolio_No2" = "blue")) +
  theme_minimal()

#Summary statistics
summaryStatsPort2 <- summary(portfolio_returns_byhand_alt)
summaryDf_3 <- as.data.frame(matrix(unlist(summaryStatsPort2),nrow = 6))
names(summaryDf_3) <- "Portfolio No2 reb. month. stats"
portfolio_sd_alt_an <- paste("SD:", sep = " ", round(portfolio_sd_alt, 4))
summaryDf_3 <- rbind(summaryDf_3, portfolio_sd_alt_an)
print(summaryDf_3)

## Calculating of with yearly rebalancing
#creating portfolio xts, since did not do this before (ups)
prices_monthly <- to.monthly(prices, indexAt = "last", OHLC = FALSE)
asset_returns_xts <- na.omit(Return.calculate(prices_monthly, method = "log"))

#Portfolio No1 calculations and summary statistics
portfolio_returns_xts_rebalanced_yearly_no1 <- 
  Return.portfolio(asset_returns_xts, weights = w, rebalance_on = "years") %>%
  `colnames<-`("returns")

portfolio_sd_reb_yr_1 <- StdDev(portfolio_returns_xts_rebalanced_yearly_no1$returns)

summaryStatsPort3 <- summary(portfolio_returns_xts_rebalanced_yearly_no1)
summaryDf_4 <- as.data.frame(matrix(unlist(summaryStatsPort3),nrow = 6))
summaryDf_4 <- summaryDf_4[, names(summaryDf_4) != "V1"]
portfolio_sd_reb_yr_1_an <- paste("SD:", sep = " ", round(portfolio_sd_reb_yr_1, 4))
summaryDf_4 <- cbind(summaryDf_4, portfolio_sd_reb_yr_1_an) #had a bit of an issue here, so playing around with data to just make it behave as I want in a way displayed previously
summaryDf_4 <- rbind(summaryDf_4, portfolio_sd_reb_yr_1_an)
print(summaryDf_4) #upd, did not work, data correct, just the data frame not quite up to what I want

#Portfolio No2 calculations and summary statistics
portfolio_returns_xts_rebalanced_yearly_no2 <- 
  Return.portfolio(asset_returns_xts, weights = wa, rebalance_on = "years") %>%
  `colnames<-`("returns")

portfolio_sd_reb_yr_2 <- StdDev(portfolio_returns_xts_rebalanced_yearly_no2$returns)

summaryStatsPort4 <- summary(portfolio_returns_xts_rebalanced_yearly_no2)
summaryDf_5 <- as.data.frame(matrix(unlist(summaryStatsPort4),nrow = 6))
summaryDf_5 <- summaryDf_5[, names(summaryDf_5) != "V1"]
portfolio_sd_reb_yr_2_an <- paste("SD:", sep = " ", round(portfolio_sd_reb_yr_2, 4))
summaryDf_5 <- cbind(summaryDf_5, portfolio_sd_reb_yr_2_an)
summaryDf_5 <- rbind(summaryDf_5, portfolio_sd_reb_yr_2_an)
print(summaryDf_5)

## Monte Carlo
#Setting up for the simulation

mc_rep = 1000 # Number of Monte Carlo simulations
training_days = 30
stock_price = as.matrix(prices_tbl[,2:6])

returns = function(Y){ #function for calculating returns
  len = nrow(Y)
  yDif = Y[2:len, ] / Y[1:len-1, ] - 1
}

# Get the Stock Returns
stock_Returns = returns(stock_price)

#I will simulate AIQ
wSim_AIQ <- matrix(c(1, 0, 0, 0, 0), nrow = 1)

#Covariance matrix of returns
coVarMat = cov(stock_Returns)
miu = colMeans(stock_Returns)
Miu = matrix(rep(miu, training_days), nrow = ncol(stock_Returns))

# Initializing simulated 30 day portfolio returns
portfolio_Returns_30_m = matrix(0, training_days, mc_rep)

set.seed(25)
for (i in 1:mc_rep) {
  Z = matrix (rnorm(dim(stock_Returns)[2] * training_days), ncol = training_days)
  # Lower Triangular Matrix from Choleski Factorization
  L = t(chol(coVarMat))
  # Calculate stock returns for each day
  daily_Returns = Miu + L %*% Z
  # Calculate portfolio returns for 30 days
  portfolio_Returns_30 = cumprod(wSim_AIQ %*% daily_Returns + 1)
  portfolio_Returns_30_m[,i] = portfolio_Returns_30;
}

#Plotting the data
x_axis = rep(1:training_days, mc_rep)
y_axis = as.vector(portfolio_Returns_30_m-1)
plot_data = data.frame(x_axis, y_axis)
ggplot(data = plot_data, aes(x = x_axis, y = y_axis)) + geom_path(col = 'red', size = 0.0001) +
  xlab('Days') + ylab('AIQ Returns') +
  ggtitle('Simulated AIQ Returns in 30 days')+
  labs(caption = "Work my on. | Data Source: Yahoo Finance") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Portfolio Returns statistics on the 30th day.
Avg_Portfolio_Returns_1 = mean(portfolio_Returns_30_m[30,]-1)
SD_Portfolio_Returns_1 = sd(portfolio_Returns_30_m[30,]-1)
Median_Portfolio_Returns_1 = median(portfolio_Returns_30_m[30,]-1)
print(c(Avg_Portfolio_Returns_1,SD_Portfolio_Returns_1,Median_Portfolio_Returns_1))
# Construct a 95% Confidential Interval for average returns
Avg_CI_1 = quantile(portfolio_Returns_30_m[30,]-1, c(0.025, 0.975))
print(Avg_CI_1)

#Constructing the portfolio analysis
#I will the equal weight portfolio
#The set up is mostly the same, just different weights

wSim_eq <- matrix(c(0.2, 0.2, 0.2, 0.2, 0.2), nrow = 1)
portfolio_Returns_30_m_eq = matrix(0, training_days, mc_rep)

set.seed(25)
for (i in 1:mc_rep) {
  Z = matrix (rnorm(dim(stock_Returns)[2] * training_days), ncol = training_days)
  # Lower Triangular Matrix from Choleski Factorization
  L = t(chol(coVarMat))
  # Calculate stock returns for each day
  daily_Returns = Miu + L %*% Z
  # Calculate portfolio returns for 30 days
  portfolio_Returns_30 = cumprod(wSim_eq %*% daily_Returns + 1)
  portfolio_Returns_30_m_eq[,i] = portfolio_Returns_30;
}

#Plotting the results
x_axis_2 = rep(1:training_days, mc_rep)
y_axis_2 = as.vector(portfolio_Returns_30_m_eq-1)
plot_data = data.frame(x_axis_2, y_axis_2)
ggplot(data = plot_data, aes(x = x_axis_2, y = y_axis_2)) + geom_path(col = 'red', size = 0.0001) +
  xlab('Days') + ylab('Portfolio Returns') +
  ggtitle('Simulated Equal weight AI ETF Portfolio Returns in 30 days')+
  labs(caption = "Work my on. | Data Source: Yahoo Finance") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#Portfolio Returns statistics on the 30th day.
Avg_Portfolio_Returns_2 = mean(portfolio_Returns_30_m_eq[30,]-1)
SD_Portfolio_Returns_2 = sd(portfolio_Returns_30_m_eq[30,]-1)
Median_Portfolio_Returns_2 = median(portfolio_Returns_30_m_eq[30,]-1)
print(c(Avg_Portfolio_Returns_2,SD_Portfolio_Returns_2,Median_Portfolio_Returns_2))
#Construct a 95% Confidential Interval for average returns
Avg_CI_2 = quantile(portfolio_Returns_30_m_eq[30,]-1, c(0.025, 0.975))
print(Avg_CI_2)

#Setting up sensitivity analysis
# Parameters
seeds <- c(1, 25, 1000, 9874) #Seeds I will test
forecast_periods <- c(30, 60, 90) #Time in future to forecast
results <- list() #Result storage

for (period in forecast_periods) {
  for (s in seeds) {
    set.seed(s)
    
    Miu <- matrix(rep(miu, period), nrow = ncol(stock_Returns))
    
    # Run MC simulation
    portfolio_Returns_m <- matrix(0, period, mc_rep)
    for (i in 1:mc_rep) {
      Z <- matrix(rnorm(ncol(stock_Returns) * period), ncol = period)
      L <- t(chol(coVarMat))
      daily_returns <- Miu + L %*% Z
      portfolio_returns <- cumprod(wSim_eq %*% daily_returns + 1)
      portfolio_Returns_m[, i] <- portfolio_returns
    }
    
    # Collect summary statistics
    final_returns <- portfolio_Returns_m[period, ]-1
    stats <- round(c(mean = mean(final_returns),
               sd = sd(final_returns),
               quantile(final_returns, c(0.05, 0.95))),4)
    
    results[[paste0("Seed_", s, "_Period_", period)]] <- stats
  }
}

# Convert to DataFrame for comparison
result_df <- do.call(rbind, results)
print(result_df)



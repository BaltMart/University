library(tidyquant)
library(tidyverse)
library(timetk)
library(broom)
library(highcharter)
library(tibbletime)
library(glue)
library(scales)
library(stargazer)

# A vector of symbol for our ETFs.
symbols <- sort(c("SPY","VGT","EFA","DBC","AGG"))

# Pipe them to getSymbols, extract the closing prices, and merge to one xts object. 
# Take a look at result before moving on to calculate the returns.
# Notice that we are only grabbing prices from 2013 to present, but that is 
# only to keep the loading time shorter for the post. 
prices <- 
  getSymbols(symbols, src = 'yahoo', from = "2006-12-31", to = "2024-01-03",
             auto.assign = TRUE, warnings = FALSE) %>% 
  map(~Cl(get(.))) %>%
  reduce(merge) %>% 
  `colnames<-`(symbols)

prices_monthly <- to.monthly(prices, indexAt = "last", OHLC = FALSE)
asset_returns_xts <- na.omit(Return.calculate(prices_monthly, method = "log"))

# Tidyverse method, to long, tidy format
asset_returns_long <-  
  prices %>% 
  to.monthly(indexAt = "lastof", OHLC = FALSE) %>% 
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  gather(asset, returns, -date) %>% 
  group_by(asset) %>%  
  mutate(returns = (log(returns) - log(lag(returns)))) %>% 
  na.omit()

head(asset_returns_xts)
head(asset_returns_long)


## Weights of the portfolio
w <- c(0.25, 0.25, 0.20, 0.20, 0.10)


asset_weights_sanity_check <- tibble(w, symbols)
asset_weights_sanity_check
sum(asset_weights_sanity_check$w)

w_1 <- w[1]
w_2 <- w[2]
w_3 <- w[3]
w_4 <- w[4]
w_5 <- w[5]


asset1 <- asset_returns_xts[,1]
asset2 <- asset_returns_xts[,2]
asset3 <- asset_returns_xts[,3]
asset4 <- asset_returns_xts[,4]
asset5 <- asset_returns_xts[,5]

portfolio_returns_byhand <-   
  (w_1 * asset1) + 
  (w_2 * asset2) + 
  (w_3 * asset3) +
  (w_4 * asset4) + 
  (w_5 * asset5)

names(portfolio_returns_byhand) <- "returns"


sd_by_hand <- 
  # Important, don't forget to take the square root! 
  sqrt(
    # Our weighted variance terms.  
    (w_1^2 * var(asset1)) + (w_2^2 * var(asset2)) + (w_3^2 * var(asset3)) +
      (w_4^2 * var(asset4)) + (w_5^2 * var(asset5)) +
      # Our weighted covariance terms
      (2 * w_1 * w_2 * cov(asset1, asset2)) +  
      (2 * w_1 * w_3 * cov(asset1, asset3)) +
      (2 * w_1 * w_4 * cov(asset1, asset4)) +
      (2 * w_1 * w_5 * cov(asset1, asset5)) +
      (2 * w_2 * w_3 * cov(asset2, asset3)) +
      (2 * w_2 * w_4 * cov(asset2, asset4)) +
      (2 * w_2 * w_5 * cov(asset2, asset5)) +
      (2 * w_3 * w_4 * cov(asset3, asset4)) +
      (2 * w_3 * w_5 * cov(asset3, asset5)) +
      (2 * w_4 * w_5 * cov(asset4, asset5))
  )

# I want to print the percentage, so multiply by 100 and round.
sd_by_hand_percent <- round(sd_by_hand * 100, 2)

# Confirm portfolio volatility
portfolio_sd <- StdDev(asset_returns_xts, weights = w)

# I want to print out the percentage, so I'll multiply by 100 and round.
portfolio_sd_percent <- round(portfolio_sd * 100, 2)


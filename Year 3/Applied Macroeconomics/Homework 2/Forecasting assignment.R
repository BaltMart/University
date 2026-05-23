################################################################################
#### Applied Macro Assignment   ################################################
#### Forecasting                ################################################
#### Martynas Baltramaitis      ################################################
################################################################################
library(dplyr)
library(stats)
library(ggplot2)
library(vars)
library(forecast)
library(knitr)
################################################################################
## Functions
################################################################################
extract_errors <- function(fcast_obj, actual_vec) {
  setNames(actual_vec[1:3] - fcast_obj$mean[1:3], paste0("h", 1:3))
}

make_error_df <- function(errors, model_name, variable_name) {
  data.frame(
    Model    = model_name,
    Variable = variable_name,
    Horizon  = as.integer(sub("h", "", names(errors))),
    Error    = as.numeric(errors)
  )
}
################################################################################
## Question 1 
################################################################################
# Selecting file directory
dir_data <- choose.files(caption = "Select the file saved from the table (Forecasting in Macroeconomics assignment 1.csv)")

# Loading the data to the RStudio
data <- read.csv(dir_data)

# Plotting the timeseries
ggplot(data, aes(x = period)) +
  geom_line(aes(y = gdp, color = "GDP growth"), linewidth = 1) +
  geom_line(aes(y = inf, color = "Inflation"), linewidth = 1) +
  geom_line(aes(y = une, color = "Unemployment"), linewidth = 1) +
  scale_color_manual(values = c("GDP growth" = "red", "Inflation" = "green", "Unemployment" = "blue")) +
  labs(title = "Historic data of key economic values", x = "Year", y = "Percent", color = "Indicator") +
  theme_minimal()

# Converting data into time series
data_temp <- data %>%
  dplyr::select(-period)
data_ts <- ts(data_temp, start = 1998, end = 2021, frequency = 1)
rm(data_temp)
gdp_ts <- data_ts[, "gdp"]
inf_ts <- data_ts[, "inf"]
une_ts <- data_ts[, "une"]
################################################################################
## Question 2
################################################################################
# Defining the sample
inSampGDP_98to15 <- window(gdp_ts, start = 1998, end =2015)
holdSampGDP_16to21 <- window(gdp_ts, start = 2016, end =2021)

inSampInf_98to15 <- window(inf_ts, start = 1998, end =2015)
holdSampInf_16to21 <- window(inf_ts, start = 2016, end =2021)

inSampUne_98to15 <- window(une_ts, start = 1998, end =2015)
holdSampUne_16to21 <- window(une_ts, start = 2016, end =2021)

# Estimating different forecasting models
# GDP
AR_GDP <- arima(inSampGDP_98to15, order = c(1,0,0))
MA_GDP <- arima(inSampGDP_98to15, order = c(0,0,1))
ARMA_GDP <- arima(inSampGDP_98to15, order = c(1,0,1))
# Inflation
AR_Inf <- arima(inSampInf_98to15, order = c(1,0,0))
MA_Inf <- arima(inSampInf_98to15, order = c(0,0,1))
ARMA_Inf <- arima(inSampInf_98to15, order = c(1,0,1))
# Unemployment
AR_Un <- arima(inSampUne_98to15, order = c(1,0,0))
MA_Un <- arima(inSampUne_98to15, order = c(0,0,1))
ARMA_Un <- arima(inSampUne_98to15, order = c(1,0,1))

# VAR model of the three variables together
VAR_GDP_Inf_Une <- VAR(ts.union(inSampGDP_98to15, inSampInf_98to15, inSampUne_98to15), p = 1, type = "const")

####### Forecasting the models, saving the errors
###GDP
fcast_ar1_gdp <- forecast(AR_GDP, h = 3)
fcast_ma1_gdp <- forecast(MA_GDP, h = 3)
fcast_arma1_gdp <- forecast(ARMA_GDP, h = 3)

# Errors
errors_ar1_gdp   <- extract_errors(fcast_ar1_gdp, holdSampGDP_16to21)
errors_ma1_gdp   <- extract_errors(fcast_ma1_gdp, holdSampGDP_16to21)
errors_arma1_gdp <- extract_errors(fcast_arma1_gdp, holdSampGDP_16to21)

### Inflation
fcast_ar1_inf <- forecast(AR_Inf, h = 3)
fcast_ma1_inf <-  forecast(MA_Inf, h = 3)
fcast_arma1_inf <-  forecast(ARMA_Inf, h = 3)

# Errors
errors_ar1_inf   <- extract_errors(fcast_ar1_inf, holdSampInf_16to21)
errors_ma1_inf   <- extract_errors(fcast_ma1_inf, holdSampInf_16to21)
errors_arma1_inf <- extract_errors(fcast_arma1_inf, holdSampInf_16to21)

### Unemployment
fcast_ar1_un  <- forecast(AR_Un, h = 3)
fcast_ma1_un <-  forecast(MA_Un, h = 3)
fcast_arma1_un <-  forecast(ARMA_Un, h = 3)

# Errors
errors_ar1_un <- extract_errors(fcast_ar1_un, holdSampUne_16to21)
errors_ma1_un <- extract_errors(fcast_ma1_un, holdSampUne_16to21)
errors_arma1_un <- extract_errors(fcast_arma1_un, holdSampUne_16to21)

### VAR
fcast_var <- forecast(VAR_GDP_Inf_Une, h = 3)

errors_var_gdp <- extract_errors(fcast_var$forecast$inSampGDP_98to15, holdSampGDP_16to21)
errors_var_inf <- extract_errors(fcast_var$forecast$inSampInf_98to15, holdSampInf_16to21)
errors_var_une <- extract_errors(fcast_var$forecast$inSampUne_98to15, holdSampUne_16to21)

# Saving all errors to a data frame
errors_df_1 <- rbind(
  make_error_df(errors_ar1_gdp,   "AR(1)",   "GDP"),
  make_error_df(errors_ma1_gdp,   "MA(1)",   "GDP"),
  make_error_df(errors_arma1_gdp, "ARMA(1,1)", "GDP"),
  make_error_df(errors_ar1_inf,   "AR(1)",   "Inflation"),
  make_error_df(errors_ma1_inf,   "MA(1)",   "Inflation"),
  make_error_df(errors_arma1_inf, "ARMA(1,1)", "Inflation"),
  make_error_df(errors_ar1_un,    "AR(1)",   "Unemployment"),
  make_error_df(errors_ma1_un,    "MA(1)",   "Unemployment"),
  make_error_df(errors_arma1_un,  "ARMA(1,1)", "Unemployment"),
  make_error_df(errors_var_gdp, "VAR(1)", "GDP"),
  make_error_df(errors_var_inf, "VAR(1)", "Inflation"),
  make_error_df(errors_var_une,  "VAR(1)", "Unemployment")
)

# Making a latex table
kable(errors_df_1, format = "latex", booktabs = TRUE,
      caption = "Forecast Errors by Model, Variable and Forecast Horizon")

# Plotting forecasts with autoplot
autoplot(fcast_var) +
  facet_wrap(~ series, labeller = as_labeller(c(
    inSampGDP_98to15 = "GDP growth",
    inSampInf_98to15 = "Inflation",
    inSampUne_98to15 = "Unemployment"
  ))) +
  labs(title = "VAR(1) Forecasts", x = "Time", y = "Percent") +
  theme_minimal()

#GDP plots
autoplot(fcast_ar1_gdp)+
  labs(title = "AR(1) Forecast of GDP", x = "Time", y = "Percent") +
  theme_minimal()

autoplot(fcast_ma1_gdp)+
  labs(title = "MA(1) Forecast of GDP", x = "Time", y = "Percent") +
  theme_minimal()

autoplot(fcast_arma1_gdp)+
  labs(title = "ARMA(1,1) Forecast of GDP", x = "Time", y = "Percent") +
  theme_minimal()

#Inf plots
autoplot(fcast_ar1_inf)+
  labs(title = "AR(1) Forecast of Inflation", x = "Time", y = "Percent") +
  theme_minimal()

autoplot(fcast_ma1_inf)+
  labs(title = "MA(1) Forecast of Inflation", x = "Time", y = "Percent") +
  theme_minimal()

autoplot(fcast_arma1_inf)+
  labs(title = "ARMA(1,1) Forecast of Inflation", x = "Time", y = "Percent") +
  theme_minimal()

#Une plots
autoplot(fcast_ar1_un)+
  labs(title = "AR(1) Forecast of Unemployment", x = "Time", y = "Percent") +
  theme_minimal()

autoplot(fcast_ma1_un)+
  labs(title = "MA(1) Forecast of Unemployment", x = "Time", y = "Percent") +
  theme_minimal()

autoplot(fcast_arma1_un)+
  labs(title = "ARMA(1,1) Forecast of Unemployment", x = "Time", y = "Percent") +
  theme_minimal()

################################################################################
## Question 3
################################################################################
# Re-defining the in-sample for both scenarios in this task
# in1999 to 2016
inSampGDP_99to16 <- window(gdp_ts, start = 1999, end =2016)
holdSampGDP_17to21 <- window(gdp_ts, start = 2017, end =2021)

inSampInf_99to16 <- window(inf_ts, start = 1999, end =2016)
holdSampInf_17to21 <- window(inf_ts, start = 2017, end =2021)

inSampUne_99to16 <- window(une_ts, start = 1999, end =2016)
holdSampUne_17to21 <- window(une_ts, start = 2017, end =2021)

# in2000 to 2017
inSampGDP_00to17 <- window(gdp_ts, start = 2000, end =2017)
holdSampGDP_18to21 <- window(gdp_ts, start = 2018, end =2021)

inSampInf_00to17 <- window(inf_ts, start = 2000, end =2017)
holdSampInf_18to21 <- window(inf_ts, start = 2018, end =2021)

inSampUne_00to17 <- window(une_ts, start = 2000, end =2017)
holdSampUne_18to21 <- window(une_ts, start = 2018, end =2021)

## Fitting the models
# in1999 to 2016
# GDP
AR_GDP_2 <- arima(inSampGDP_99to16, order = c(1,0,0))
MA_GDP_2 <- arima(inSampGDP_99to16, order = c(0,0,1))
ARMA_GDP_2 <- arima(inSampGDP_99to16, order = c(1,0,1))
# Inflation
AR_Inf_2 <- arima(inSampInf_99to16, order = c(1,0,0))
MA_Inf_2 <- arima(inSampInf_99to16, order = c(0,0,1))
ARMA_Inf_2 <- arima(inSampInf_99to16, order = c(1,0,1))
# Unemployment
AR_Un_2 <- arima(inSampUne_99to16, order = c(1,0,0))
MA_Un_2 <- arima(inSampUne_99to16, order = c(0,0,1))
ARMA_Un_2 <- arima(inSampUne_99to16, order = c(1,0,1))

# VAR model of the three variables together
VAR_GDP_Inf_Une_2 <- VAR(ts.union(inSampGDP_99to16, inSampInf_99to16, inSampUne_99to16), p = 1, type = "const")

# in2000 to 2017
# GDP
AR_GDP_3 <- arima(inSampGDP_00to17, order = c(1,0,0))
MA_GDP_3 <- arima(inSampGDP_00to17, order = c(0,0,1))
ARMA_GDP_3 <- arima(inSampGDP_00to17, order = c(1,0,1))
# Inflation
AR_Inf_3 <- arima(inSampInf_00to17, order = c(1,0,0))
MA_Inf_3 <- arima(inSampInf_00to17, order = c(0,0,1))
ARMA_Inf_3 <- arima(inSampInf_00to17, order = c(1,0,1))
# Unemployment
AR_Un_3 <- arima(inSampUne_00to17, order = c(1,0,0))
MA_Un_3 <- arima(inSampUne_00to17, order = c(0,0,1))
ARMA_Un_3 <- arima(inSampUne_00to17, order = c(1,0,1))

# VAR model of the three variables together
VAR_GDP_Inf_Une_3 <- VAR(ts.union(inSampGDP_00to17, inSampInf_00to17, inSampUne_00to17), p = 1, type = "const")

## Forecasting the models and saving the errors
# in1999 to 2016
##GDP
fcast_ar1_gdp_2 <- forecast(AR_GDP_2, h = 3)
fcast_ma1_gdp_2 <- forecast(MA_GDP_2, h = 3)
fcast_arma1_gdp_2 <- forecast(ARMA_GDP_2, h = 3)

# Errors
errors_ar1_gdp_2 <- extract_errors(fcast_ar1_gdp_2, holdSampGDP_17to21)
errors_ma1_gdp_2 <- extract_errors(fcast_ma1_gdp_2, holdSampGDP_17to21)
errors_arma1_gdp_2 <- extract_errors(fcast_arma1_gdp_2, holdSampGDP_17to21)

### Inflation
fcast_ar1_inf_2 <- forecast(AR_Inf_2, h = 3)
fcast_ma1_inf_2 <- forecast(MA_Inf_2, h = 3)
fcast_arma1_inf_2 <- forecast(ARMA_Inf_2, h = 3)

# Errors
errors_ar1_inf_2 <- extract_errors(fcast_ar1_inf_2, holdSampInf_17to21)
errors_ma1_inf_2 <- extract_errors(fcast_ma1_inf_2, holdSampInf_17to21)
errors_arma1_inf_2 <- extract_errors(fcast_arma1_inf_2, holdSampInf_17to21)

### Unemployment
fcast_ar1_un_2  <- forecast(AR_Un_2, h = 3)
fcast_ma1_un_2 <-  forecast(MA_Un_2, h = 3)
fcast_arma1_un_2 <-  forecast(ARMA_Un_2, h = 3)

# Errors
errors_ar1_un_2 <- extract_errors(fcast_ar1_un_2, holdSampUne_17to21)
errors_ma1_un_2 <- extract_errors(fcast_ma1_un_2, holdSampUne_17to21)
errors_arma1_un_2 <- extract_errors(fcast_arma1_un_2, holdSampUne_17to21)

### VAR
fcast_var_2 <- forecast(VAR_GDP_Inf_Une_2, h = 3)

errors_var_gdp_2 <- extract_errors(fcast_var_2$forecast$inSampGDP_99to16, holdSampGDP_17to21)
errors_var_inf_2 <- extract_errors(fcast_var_2$forecast$inSampInf_99to16, holdSampInf_17to21)
errors_var_une_2 <- extract_errors(fcast_var_2$forecast$inSampUne_99to16, holdSampUne_17to21)

errors_df_2 <- rbind(
  make_error_df(errors_ar1_gdp_2,   "AR(1)",   "GDP"),
  make_error_df(errors_ma1_gdp_2,   "MA(1)",   "GDP"),
  make_error_df(errors_arma1_gdp_2, "ARMA(1,1)", "GDP"),
  make_error_df(errors_ar1_inf_2,   "AR(1)",   "Inflation"),
  make_error_df(errors_ma1_inf_2,   "MA(1)",   "Inflation"),
  make_error_df(errors_arma1_inf_2, "ARMA(1,1)", "Inflation"),
  make_error_df(errors_ar1_un_2,    "AR(1)",   "Unemployment"),
  make_error_df(errors_ma1_un_2,    "MA(1)",   "Unemployment"),
  make_error_df(errors_arma1_un_2,  "ARMA(1,1)", "Unemployment"),
  make_error_df(errors_var_gdp_2, "VAR(1)", "GDP"),
  make_error_df(errors_var_inf_2, "VAR(1)", "Inflation"),
  make_error_df(errors_var_une_2,  "VAR(1)", "Unemployment")
)

#in2000 to 2017
##GDP
fcast_ar1_gdp_3 <- forecast(AR_GDP_3, h = 3)
fcast_ma1_gdp_3 <- forecast(MA_GDP_3, h = 3)
fcast_arma1_gdp_3 <- forecast(ARMA_GDP_3, h = 3)

# Errors
errors_ar1_gdp_3 <- extract_errors(fcast_ar1_gdp_3, holdSampGDP_18to21)
errors_ma1_gdp_3 <- extract_errors(fcast_ma1_gdp_3, holdSampGDP_18to21)
errors_arma1_gdp_3 <- extract_errors(fcast_arma1_gdp_3, holdSampGDP_18to21)

### Inflation
fcast_ar1_inf_3 <- forecast(AR_Inf_3, h = 3)
fcast_ma1_inf_3 <- forecast(MA_Inf_3, h = 3)
fcast_arma1_inf_3 <- forecast(ARMA_Inf_3, h = 3)

# Errors
errors_ar1_inf_3 <- extract_errors(fcast_ar1_inf_3, holdSampInf_18to21)
errors_ma1_inf_3 <- extract_errors(fcast_ma1_inf_3, holdSampInf_18to21)
errors_arma1_inf_3 <- extract_errors(fcast_arma1_inf_3, holdSampInf_18to21)

### Unemployment
fcast_ar1_un_3  <- forecast(AR_Un_3, h = 3)
fcast_ma1_un_3 <-  forecast(MA_Un_3, h = 3)
fcast_arma1_un_3 <-  forecast(ARMA_Un_3, h = 3)

# Errors
errors_ar1_un_3 <- extract_errors(fcast_ar1_un_3, holdSampUne_18to21)
errors_ma1_un_3 <- extract_errors(fcast_ma1_un_3, holdSampUne_18to21)
errors_arma1_un_3 <- extract_errors(fcast_arma1_un_3, holdSampUne_18to21)

### VAR
fcast_var_3 <- forecast(VAR_GDP_Inf_Une_3, h = 3)

errors_var_gdp_3 <- extract_errors(fcast_var_3$forecast$inSampGDP_00to17, holdSampGDP_18to21)
errors_var_inf_3 <- extract_errors(fcast_var_3$forecast$inSampInf_00to17, holdSampInf_18to21)
errors_var_une_3 <- extract_errors(fcast_var_3$forecast$inSampUne_00to17, holdSampUne_18to21)

errors_df_3 <- rbind(
  make_error_df(errors_ar1_gdp_3,   "AR(1)",   "GDP"),
  make_error_df(errors_ma1_gdp_3,   "MA(1)",   "GDP"),
  make_error_df(errors_arma1_gdp_3, "ARMA(1,1)", "GDP"),
  make_error_df(errors_ar1_inf_3,   "AR(1)",   "Inflation"),
  make_error_df(errors_ma1_inf_3,   "MA(1)",   "Inflation"),
  make_error_df(errors_arma1_inf_3, "ARMA(1,1)", "Inflation"),
  make_error_df(errors_ar1_un_3,    "AR(1)",   "Unemployment"),
  make_error_df(errors_ma1_un_3,    "MA(1)",   "Unemployment"),
  make_error_df(errors_arma1_un_3,  "ARMA(1,1)", "Unemployment"),
  make_error_df(errors_var_gdp_3, "VAR(1)", "GDP"),
  make_error_df(errors_var_inf_3, "VAR(1)", "Inflation"),
  make_error_df(errors_var_une_3,  "VAR(1)", "Unemployment")
)
# Creating latex tables reporting errors for the models
kable(errors_df_2, format = "latex", booktabs = TRUE,
      caption = "Forecast Errors by Model, Variable and Forecast Horizon (in sample 1999 to 2016)")

kable(errors_df_3, format = "latex", booktabs = TRUE,
      caption = "Forecast Errors by Model, Variable and Forecast Horizon (in sample 2000 to 2017)")

# Plotting the forecast figures (using the same code, just changing the variables between generation)
autoplot(fcast_var_3) +
  facet_wrap(~ series, labeller = as_labeller(c(
    inSampGDP_00to17 = "GDP growth",
    inSampInf_00to17 = "Inflation",
    inSampUne_00to17 = "Unemployment"
  ))) +
  labs(title = "VAR(1) Forecasts", x = "Time", y = "Percent") +
  theme_minimal()

#GDP plots
autoplot(fcast_ar1_gdp_3)+
  labs(title = "AR(1) Forecast of GDP", x = "Time", y = "Percent") +
  theme_minimal()

autoplot(fcast_ma1_gdp_3)+
  labs(title = "MA(1) Forecast of GDP", x = "Time", y = "Percent") +
  theme_minimal()

autoplot(fcast_arma1_gdp_3)+
  labs(title = "ARMA(1,1) Forecast of GDP", x = "Time", y = "Percent") +
  theme_minimal()

#Inf plots
autoplot(fcast_ar1_inf_3)+
  labs(title = "AR(1) Forecast of Inflation", x = "Time", y = "Percent") +
  theme_minimal()

autoplot(fcast_ma1_inf_3)+
  labs(title = "MA(1) Forecast of Inflation", x = "Time", y = "Percent") +
  theme_minimal()

autoplot(fcast_arma1_inf_3)+
  labs(title = "ARMA(1,1) Forecast of Inflation", x = "Time", y = "Percent") +
  theme_minimal()

#Une plots
autoplot(fcast_ar1_un_3)+
  labs(title = "AR(1) Forecast of Unemployment", x = "Time", y = "Percent") +
  theme_minimal()

autoplot(fcast_ma1_un_3)+
  labs(title = "MA(1) Forecast of Unemployment", x = "Time", y = "Percent") +
  theme_minimal()

autoplot(fcast_arma1_un_3)+
  labs(title = "ARMA(1,1) Forecast of Unemployment", x = "Time", y = "Percent") +
  theme_minimal()
################################################################################
## Question 4
################################################################################
# Preparing to combine error df's
errors_df_1 <- mutate(errors_df_1, InSample = "1998-2015")
errors_df_2 <- mutate(errors_df_2, InSample = "1999-2016")
errors_df_3 <- mutate(errors_df_3, InSample = "2000-2017")

# Combining error data frames into one
error_df_all <- bind_rows(errors_df_1, errors_df_2, errors_df_3) %>%
  mutate(InSample = as.character(InSample),
         Variable = as.character(Variable),
         Horizon  = as.integer(Horizon))

# Preparing a data frame that contains true values to be merged to data frame of errors
actuals_all <- bind_rows(
  data.frame(InSample = "1998-2015", Variable = rep(c("GDP", "Inflation", "Unemployment"), each = 3),
             Horizon = rep(1:3, times = 3),
             Actual  = c(holdSampGDP_16to21[1:3], holdSampInf_16to21[1:3], holdSampUne_16to21[1:3])),
  data.frame(InSample = "1999-2016", Variable = rep(c("GDP", "Inflation", "Unemployment"), each = 3),
             Horizon = rep(1:3, times = 3),
             Actual  = c(holdSampGDP_17to21[1:3], holdSampInf_17to21[1:3], holdSampUne_17to21[1:3])),
  data.frame(InSample = "2000-2017", Variable = rep(c("GDP", "Inflation", "Unemployment"), each = 3),
             Horizon = rep(1:3, times = 3),
             Actual  = c(holdSampGDP_18to21[1:3], holdSampInf_18to21[1:3], holdSampUne_18to21[1:3]))
) %>%
  mutate(InSample = as.character(InSample),
         Variable = as.character(Variable),
         Horizon  = as.integer(Horizon))

# Merging the two data frames
error_df_all <- error_df_all %>%
  dplyr::left_join(actuals_all, by = c("InSample", "Variable", "Horizon"))

# Calculating the errors
err_measure_df <- error_df_all %>%
  group_by(InSample, Model, Variable) %>%
  summarise(
    MSE = mean(Error^2),
    MSPE = mean((Error/Actual)^2),
    RMSE = sqrt(mean(Error^2)),
    RMSPE = sqrt(mean((Error / Actual)^2)),
    MAE = mean(abs(Error)),
    RMAPE = mean(abs(Error / Actual)),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric),~round(.x, 3)))

kable(err_measure_df, format = "latex", booktabs = TRUE,
      caption = "Forecasting model accuracy measure table")
################################################################################
## Question 5
################################################################################
dm.test(errors_ma1_gdp, errors_var_gdp, alternative = "two.sided", h = 1, power = 2)
dm.test(errors_ma1_gdp_2, errors_var_gdp_2, alternative = "two.sided", h = 1, power = 2)
dm.test(errors_ma1_gdp_3, errors_var_gdp_3, alternative = "two.sided", h = 1, power = 2)

dm.test(errors_ma1_inf, errors_arma1_inf, alternative = "two.sided", h = 1, power = 2)
dm.test(errors_ma1_inf_2, errors_arma1_inf_2, alternative = "two.sided", h = 1, power = 2)
dm.test(errors_ma1_inf_3, errors_arma1_inf_3, alternative = "two.sided", h = 1, power = 2)

dm.test(errors_var_une, errors_ar1_un, alternative = "two.sided", h = 1, power = 2)
dm.test(errors_var_une_2, errors_ar1_un_2, alternative = "two.sided", h = 1, power = 2)
dm.test(errors_var_une_3, errors_ar1_un_3, alternative = "two.sided", h = 1, power = 2)


#### Assignment 1
## Task 1

library(tidyverse)
library(tidyr)

data = read.csv("C:\\Users\\martynas\\Desktop\\uni\\Year 2\\DATA\\Task 1\\final_dataset_full.csv", stringsAsFactors=FALSE)

#a
head(data)

# The data set is in panel data, long format, as variables are on the x axis
# It includes macroeconomic data points for such things as GDP, unemployment and population
# Also in the data set is a collection of climate data. For example, SPI or Standardized precipitation index, measured in standard deviations from the mean
# SMA - Soil Moisture Anomaly 
# lfi - Low-Flow Index
# fapar - vegetation productivity
# drg - drought indicator
# hw - heat waves

# Number of regions in data set
n_total_reg = n_distinct(data$NUTS_ID)

#b 
# Cleaning out the data set to remove regions without GDP data for all period
df_clean = data %>%
  group_by(NUTS_ID) %>%
  filter(any(!is.na(gdp)))

# Number of regions after adjusting
n_cl_reg = n_distinct(df_clean$NUTS_ID)
print(n_cl_reg)

# Min/max values of GDP for each observation year
gdp_extrema <- df_clean %>%
  group_by(YEAR) %>%
  summarise(
    min_GDP = ifelse(all(is.na(gdp)), NA, min(gdp, na.rm = TRUE)),
    max_GDP = ifelse(all(is.na(gdp)), NA, max(gdp, na.rm = TRUE))
  )
print(gdp_extrema)

#Lowest/highest GDP in 2020
gdp_data_2020 = df_clean %>%
  filter(YEAR == 2020) %>%
  select(NUTS_ID, YEAR, gdp)

sortByInc = gdp_data_2020 %>%
  arrange(gdp)
  
min_value = sortByInc$gdp[1]
min_country = sortByInc$NUTS_ID[1]

max_value = sortByInc$gdp[nrow(sortByInc)]
max_country = sortByInc$NUTS_ID[nrow(sortByInc)]

print(paste("Min GDP:", min_value, "Region:", min_country, 
            "; Max GDP:", max_value, "Region:", max_country))

#Highest GDP growth
gdp_growth = df_clean %>%
  select(NUTS_ID, YEAR, growth_rate) %>%
  arrange(desc(growth_rate))

highest_growth = head(gdp_growth, n = 1)
print(paste0("Highest growing region of the data period, the year and the growth rate: ", highest_growth))

## Part 2 of the task
# I decided to take three french regions for this part

filtered_df <- df_clean %>%
  filter(!is.na(spi12_yearly) & 
           !is.na(yearly_hw) & 
           !is.na(avg_hw_intensity) & 
           !is.na(avg_len_hw) & 
           !is.na(gross_value_added_A) & 
           !is.na(gross_value_added_C))

unique_fra = filtered_df %>%
  filter(grepl("^fr", NUTS_ID, ignore.case = TRUE)) %>%
  distinct(NUTS_ID)

FR_data = df_clean %>%
  filter(NUTS_ID == "FR101" | NUTS_ID == "FR102" | NUTS_ID == "FR103")

# Ranges of variables of interest
climate_ranges = FR_data %>%
  group_by(NUTS_ID) %>%
  summarise(
    SPI_12_yearly_range = range(spi12_yearly, na.rm = TRUE),
    yearly_hw_range = range(yearly_hw, na.rm = TRUE),
    avg_hw_intensity_range = range(avg_hw_intensity, na.rm = TRUE),
    avg_len_hw_range = range(avg_len_hw, na.rm = TRUE)
  )
print(climate_ranges)

#Average, median, mode, sd
install.packages("DescTools")
library(DescTools) #Used to calculate mode

# Function to calculate mode
custom_mode <- function(x) {
  x <- x[!is.na(x)]  # Remove NA values
  if(length(x) == 0) return(NA)  # Return NA if the vector is empty
  Mode(x)
}

economic_stats <- FR_data %>%
  group_by(NUTS_ID) %>%
  summarise(
    avg_gross_value_added_A = mean(gross_value_added_A, na.rm = TRUE),
    median_gross_value_added_A = median(gross_value_added_A, na.rm = TRUE),
    mode_gross_value_added_A = custom_mode(gross_value_added_A),
    sd_gross_value_added_A = sd(gross_value_added_A, na.rm = TRUE),
    avg_gross_value_added_C = mean(gross_value_added_C, na.rm = TRUE),
    median_gross_value_added_C = median(gross_value_added_C, na.rm = TRUE),
    mode_gross_value_added_C = custom_mode(gross_value_added_C),
    sd_gross_value_added_C = sd(gross_value_added_C, na.rm = TRUE)
  ) %>%
  distinct(NUTS_ID, .keep_all = TRUE)

print(economic_stats)

# Plot of time series of climate variable
library(ggplot2)

ggplot(FR_data, aes(x = YEAR, y = spi12_yearly, colour = NUTS_ID)) +
  geom_line() +
  labs(title = "Time Series of Standardized Precipitation Index",
       x = "Year",
       y = "Standardized Precipitation Index (SPI)",
       colour = "Regional IDs",
       caption = "Data created in ECB_SSM Hackathon, analysis own.") +
  theme_classic()

# Plot of time series of economic variable
ggplot(FR_data, aes(x = YEAR, y = gross_value_added_C, colour = NUTS_ID)) +
  geom_line() +
  geom_point() +
  labs(title = "Time Series of Gross Value Added C",
       x = "Year",
       y = "Gross Value Added",
       colour = "Regional IDs",
       caption= "Data created in ECB_SSM Hackathon, analysis own.") +
  theme_classic()

################################################################################
## Beyond wages, is there heterogeneity in hours worked?
## Exam paper codes
## Exam ID 312
## 2025-12-10
################################################################################
#### Loading Libraries
library(tidyverse)
library(ggplot2)
library(stargazer)
library(xtable)
library(dplyr)

# Functions
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#### Loading the data set
setwd("C:/Users/Desktop/uni/Year 3/AUTUMN/Microeconomics/Exam/hours heterogeneity")
DUS_2018 <- read.csv("DUS_2018.csv")

# Taking a look at the data
summary(DUS_2018)

# Renaming variables
DUS_2018 <- DUS_2018 %>%
  rename(GeoLoc = A11, # Geographical location of the local unit (NUTS-1)
         SizeBus = A12, # Size code of the enterprise to which the local unit belongs
         CollPay = A15, # Status of colective agrement
         Sex = B21,
         Age = B22_CLASS,
         Occupation = B23, # ISCO-08
         Education = B25, # Highest level of education and training completed
         lngthServ = B26, # Length of service in the enterprise (in years)
         ContWorkTime = B27, #Contractual working time FT or PT
         ShareOfFullTime = B271,
         EmploymentCont = B28, # Type of employment contract
         nHoursPaid = B32,
         nOvertime = B321,
         AverageHourlyWage = B43)

# Creating Hours worked per week from the data in the survey
DUS_2018 <- DUS_2018 %>%
  mutate(HoursWeekly = 40 * (ShareOfFullTime/100))

# Descriptive statistics of workers
df_des <- data.frame(
  Measure = c("Mean", "Variation", "Max", "Min", "Q25", "Q75", "Mode"),
  HoursWorkedWeekly = c(
    round(mean(DUS_2018$HoursWeekly, na.rm = TRUE),2),
    round(var(DUS_2018$HoursWeekly, na.rm = TRUE),2),
    round(max(DUS_2018$HoursWeekly, na.rm = TRUE),2),
    round(min(DUS_2018$HoursWeekly, na.rm = TRUE),2),
    round(quantile(DUS_2018$HoursWeekly, 0.25, na.rm = TRUE),2),
    round(quantile(DUS_2018$HoursWeekly, 0.75, na.rm = TRUE),2),
    round(get_mode(DUS_2018$HoursWeekly),2)
  ),
  HoursWorkedOctober = c(
    round(mean(DUS_2018$nHoursPaid, na.rm = TRUE),2),
    round(var(DUS_2018$nHoursPaid, na.rm = TRUE),2),
    round(max(DUS_2018$nHoursPaid, na.rm = TRUE),2),
    round(min(DUS_2018$nHoursPaid, na.rm = TRUE),2),
    round(quantile(DUS_2018$nHoursPaid, 0.25, na.rm = TRUE),2),
    round(quantile(DUS_2018$nHoursPaid, 0.75, na.rm = TRUE),2),
    round(get_mode(DUS_2018$nHoursPaid),2)
  ),
  Wage = c(
    round(mean(DUS_2018$AverageHourlyWage, na.rm = TRUE),2),
    round(var(DUS_2018$AverageHourlyWage, na.rm = TRUE),2),
    round(max(DUS_2018$AverageHourlyWage, na.rm = TRUE),2),
    round(min(DUS_2018$AverageHourlyWage, na.rm = TRUE),2),
    round(quantile(DUS_2018$AverageHourlyWage, 0.25, na.rm = TRUE),2),
    round(quantile(DUS_2018$AverageHourlyWage, 0.75, na.rm = TRUE),2),
    round(get_mode(DUS_2018$AverageHourlyWage),2)
  )
)

# Creating a Latex table
xt <- xtable(df_des,
             caption = "Descriptive Statistics of Hours Worked and Wages",
             label = "tab:descriptive")

print(xt,
      include.rownames = FALSE,  # This removes rownames from display
      booktabs = TRUE,
      caption.placement = "top")

n_men <- sum(DUS_2018$Sex == "M")
n_women <- sum(DUS_2018$Sex == "F")

n_edu <- DUS_2018 %>%
  group_by(Education) %>%
  summarise(
    n = n()
  )

n_age <- DUS_2018 %>%
  group_by(Age) %>%
  summarise(
    n = n()
  )

# Company characteristic statistics
n_bus <- DUS_2018 %>%
  group_by(SizeBus) %>%
  summarise(
    n = n(),
  )

n_coll <- DUS_2018 %>%
  group_by(CollPay) %>%
  summarise(
    n = n()
  )

n_occ <- n_distinct(DUS_2018$Occupation)


# Plot of Hours Worked My Created
ggplot(data = DUS_2018, aes(x = HoursWeekly)) +
  geom_histogram(
    binwidth = 5,
    fill = "lightblue",
    color = "black",
    alpha = 0.7) +
  labs(title = "Histogram of Hours Worked Weekly",
    x = "Hours",
    y = "Count",
    caption = "Source: Structure of Earning Survey in Lithuania 2018. Work my own.") +
  theme_minimal()

# Plot of Hours Worked in October
ggplot(data = DUS_2018, aes(x = nHoursPaid)) +
  geom_histogram(
    binwidth = 5,
    fill = "lightblue",
    color = "black",
    alpha = 0.7) +
  labs(title = "Histogram of Hours Worked in October",
       x = "Hours",
       y = "Count",
       caption = "Source: Structure of Earning Survey in Lithuania 2018. Work my own.") +
  theme_minimal()

# Creating a model to test for heterogeneity of workers in hours worked
r1 <- lm(nHoursPaid ~ Age + Sex + Education + Occupation + 
           NACE + SizeBus + CollPay + lngthServ, 
         data = DUS_2018)
summary(r2)

total_var_r1 <- var(DUS_2018$nHoursPaid, na.rm = TRUE)
residual_var_r1 <- var(residuals(r1), na.rm = TRUE)
explained_var_r1 <- total_var_r1 - residual_var_r1

decomp_r1 <- data.frame(
  Component = c("Total Variance", "Explained by Observables", 
                "Unexplained (Heterogeneity)", "% Unexplained"),
  Value = c(total_var_r1, explained_var_r1, residual_var_r1, 
            (residual_var_r1/total_var_r1)*100)
)

# Model with my constructed measure of weekly contractual hours
r2 <- lm(HoursWeekly ~ Age + Sex + Education + Occupation + 
           NACE + SizeBus + CollPay + lngthServ, 
         data = DUS_2018)
summary(r2)

total_var_r2 <- var(DUS_2018$HoursWeekly, na.rm = TRUE)
residual_var_r2 <- var(residuals(r2), na.rm = TRUE)
explained_var_r2 <- total_var_r2 - residual_var_r2

decomp_r2 <- data.frame(
  Component = c("Total Variance", "Explained by Observables", 
                "Unexplained (Heterogeneity)", "% Unexplained"),
  Value = c(total_var_r2, explained_var_r2, residual_var_r2, 
            (residual_var_r2/total_var_r2)*100)
)

# Model with wage as the dependent variable
r3 <- lm(AverageHourlyWage ~ Age + Sex + Education + Occupation + 
           NACE + SizeBus + CollPay + lngthServ, 
         data = DUS_2018)
summary(r3)

total_var_r3 <- var(DUS_2018$AverageHourlyWage, na.rm = TRUE)
residual_var_r3 <- var(residuals(r3), na.rm = TRUE)
explained_var_r3 <- total_var_r3 - residual_var_r3

decomp_r3 <- data.frame(
  Component = c("Total Variance", "Explained by Observables", 
                "Unexplained (Heterogeneity)", "% Unexplained"),
  Value = c(total_var_r3, explained_var_r3, residual_var_r3, 
            (residual_var_r3/total_var_r3)*100)
)

# Creating a Latex tables
xtable(decomp_r1,
       caption = "Decomposition of Variance of Monthly Hours Worked",
       label = "tab:decomposition_Monthly",
       digits = 2)

xtable(decomp_r2,
       caption = "Decomposition of Variance of Weekly Hours Worked",
       label = "tab:decomposition_Weekly",
       digits = 2)

xtable(decomp_r3,
       caption = "Decomposition of Variance of Wages",
       label = "tab:decompositon_wage",
       digits = 2)

# Within group dispersion
# first with monthly data
within_group_dispersion_october <- DUS_2018 %>%
  group_by(Occupation, Age, Sex, Education) %>%
  filter(n() >= 50) %>%  # Only groups with sufficient size
  summarise(
    n = n(),
    mean_hours = mean(nHoursPaid, na.rm = TRUE),
    sd_hours = sd(nHoursPaid, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(sd_hours))

# Key result
summary_stats_october <- summary(within_group_dispersion_october$sd_hours)

# Creating a latex output
df_summary_october <- data.frame(
  Statistic = names(summary_stats_october),
  Value = as.numeric(summary_stats_october)
)

xtable(df_summary_october,
       caption = "Within-Group Standard Deviation of Hours (Monthly Hours Worked)",
       label = "tab:within_group_sd_oc",
       digits = 2)

# My constructed weekly hours
within_group_dispersion_weekly <- DUS_2018 %>%
  group_by(Occupation, Age, Sex, Education) %>%
  filter(n() >= 50) %>%  # Only groups with sufficient size
  summarise(
    n = n(),
    mean_hours = mean(HoursWeekly, na.rm = TRUE),
    sd_hours = sd(HoursWeekly, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(sd_hours))

# Key result
summary_stats_weekly <- summary(within_group_dispersion_weekly$sd_hours)

df_summary_weekly <- data.frame(
  Statistic = names(summary_stats_weekly),
  Value = as.numeric(summary_stats_weekly)
)

xtable(df_summary_weekly,
       caption = "Within-Group Standard Deviation of Hours (Weekly Hours Worked)",
       label = "tab:within_group_sd_oc",
       digits = 2)

# Correlation between wage and hours worked
cor(DUS_2018$nHoursPaid, DUS_2018$AverageHourlyWage)
cor(DUS_2018$HoursWeekly, DUS_2018$AverageHourlyWage)

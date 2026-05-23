###############################################################################
#### Empirical Finance
### Data Assignment
## Martynas Baltramaitis, Adomas Jablonskas
# December 2025
###############################################################################


# Clear environment
rm(list = ls(all = TRUE)) 

#Set directory to the file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Load packages
library(data.table)
library(tidyverse)

###############################################################################
# 1. Data Preparation
###############################################################################
#####
# Q1
#####

q1_o <- fread(
  "historical_data_2014Q1.txt",
  sep = "|")

q1_o <- q1_o %>%
  rename(cs = V1, # Credit Score
         loan_id = V20, # Loan Identifier
         ltv = V12, # Original Loan-To-Value
         dti = V10, # Original Debt-To-Income Ratio
         prop_type = V18, # Property Type
         loan_purp = V21, # Loan purpose
         int = V13, # Original Interest Rate
         upb = V11, # Original Unpaid Principle Balance
         state = V17, # Property State
         status = V8  # Occupancy status
         ) %>%
  filter(loan_purp == 'P', # Purchase loans
         prop_type == 'SF') # Single-family home properties


q1_p <- fread(
  "historical_data_time_2014Q1.txt",
  sep = "|")

q1_p <- q1_p %>%
  select(V1, V4, V5) %>%
  rename(loan_id = V1, #Loan Identifier
         loan_age = V5, # Loan Age
         del = V4 # Current Loan Delinquency Status
         ) %>%
  filter(loan_age >= 1) # By composition, 0 does not make sense

# The following part is a replication of the practice session code

q1_p %>% count(del)

q1_p <- q1_p %>%
  mutate(del90 = case_when(
    loan_age <= 24 & del >= 3 ~ 1,
                        TRUE ~ 0))

q1_p %>% count(del90)

q1_p <- q1_p %>%
  group_by(loan_id) %>%
  summarize(del90 = sum(del90))

q1 <- inner_join(q1_o, q1_p, by = 'loan_id')

# q1_p is quite large, so we remove it to save memory space
rm(q1_p, q1_o)
gc()  


#####
# Q2
#####

# The data cleaning process for the remaining 4 quarters will be identical,
# so we refrain from abundant commenting

q2_o <- fread(
  "historical_data_2014Q2.txt",
  sep = "|")

q2_o <- q2_o %>%
  rename(cs = V1, 
         loan_id = V20, 
         ltv = V12, 
         dti = V10, 
         prop_type = V18, 
         loan_purp = V21, 
         int = V13, 
         upb = V11, 
         state = V17, 
         status = V8  
  ) %>%
  filter(loan_purp == 'P', 
         prop_type == 'SF') 

q2_p <- fread(
  "historical_data_time_2014Q2.txt",
  sep = "|")

q2_p <- q2_p %>%
  select(V1, V4, V5) %>%
  rename(loan_id = V1, 
         loan_age = V5, 
         del = V4 
  ) %>%
  filter(loan_age >= 1) 

q2_p %>% count(del)

q2_p <- q2_p %>%
  mutate(del90 = case_when(
    loan_age <= 24 & del >= 3 ~ 1,
    TRUE ~ 0))

q2_p %>% count(del90)

q2_p <- q2_p %>%
  group_by(loan_id) %>%
  summarize(del90 = sum(del90))

q2 <- inner_join(q2_o, q2_p, by = 'loan_id')

rm(q2_p, q2_o)
gc()  

#####
# Q3
#####

q3_o <- fread(
  "historical_data_2014Q3.txt",
  sep = "|")

q3_o <- q3_o %>%
  rename(cs = V1, 
         loan_id = V20, 
         ltv = V12, 
         dti = V10, 
         prop_type = V18, 
         loan_purp = V21, 
         int = V13, 
         upb = V11, 
         state = V17, 
         status = V8  
  ) %>%
  filter(loan_purp == 'P', 
         prop_type == 'SF') 

q3_p <- fread(
  "historical_data_time_2014Q3.txt",
  sep = "|")

q3_p <- q3_p %>%
  select(V1, V4, V5) %>%
  rename(loan_id = V1, 
         loan_age = V5, 
         del = V4 
  ) %>%
  filter(loan_age >= 1) 

q3_p %>% count(del)

q3_p <- q3_p %>%
  mutate(del90 = case_when(
    loan_age <= 24 & del >= 3 ~ 1,
    TRUE ~ 0))

q3_p %>% count(del90)

q3_p <- q3_p %>%
  group_by(loan_id) %>%
  summarize(del90 = sum(del90))

q3 <- inner_join(q3_o, q3_p, by = 'loan_id')

rm(q3_p, q3_o)
gc()  

#####
# Q4
#####

q4_o <- fread(
  "historical_data_2014Q4.txt",
  sep = "|")

q4_o <- q4_o %>%
  rename(cs = V1, 
         loan_id = V20, 
         ltv = V12, 
         dti = V10, 
         prop_type = V18, 
         loan_purp = V21, 
         int = V13, 
         upb = V11, 
         state = V17, 
         status = V8  
  ) %>%
  filter(loan_purp == 'P', 
         prop_type == 'SF') 

q4_p <- fread(
  "historical_data_time_2014Q4.txt",
  sep = "|")

q4_p <- q4_p %>%
  select(V1, V4, V5) %>%
  rename(loan_id = V1, 
         loan_age = V5, 
         del = V4 
  ) %>%
  filter(loan_age >= 1) 

q4_p %>% count(del)

q4_p <- q4_p %>%
  mutate(del90 = case_when(
    loan_age <= 24 & del >= 3 ~ 1,
    TRUE ~ 0))

q4_p %>% count(del90)

q4_p <- q4_p %>%
  group_by(loan_id) %>%
  summarize(del90 = sum(del90))

q4 <- inner_join(q4_o, q4_p, by = 'loan_id')

rm(q4_p, q4_o)
gc()  

# Combining the quarter tables into a single data frame

df <- bind_rows(q1, q2, q3, q4)

rm(q1, q2, q3, q4)
gc() 

# Creating the default indicator using the del90 variable
df <- df %>%
  mutate(default = ifelse(del90 > 0, 1, 0))
#####
# Checking for impossible values
#####

df %>% count(cs)
df %>% count(dti)
df %>% count(prop_type)
df %>% count(loan_purp)
df %>% count(del90)
df %>% count(default)

# Removing improbable values

df <- df %>%
  filter(cs != 9999,
         dti != 999)

###############################################################################
# 2. Exploratory Data Analysis
###############################################################################

#####
# 1. 
#####

# library to write tables in latex format
library(xtable)

summary_stats <- data.frame(
  Variable = c("LTV", "Credit score", "DTI"),
  Mean     = c(mean(df$ltv, na.rm = TRUE),
               mean(df$cs,  na.rm = TRUE),
               mean(df$dti, na.rm = TRUE)),
  SD       = c(sd(df$ltv, na.rm = TRUE),
               sd(df$cs,  na.rm = TRUE),
               sd(df$dti, na.rm = TRUE))
)

print(summary_stats)

table1 <- xtable(summary_stats)
print(table1)


#####
# 2. 
#####

df %>% count(ltv) # Values from 8 to 105, thus buckets 0-110 will be made
df %>% count(cs) # Values 517 - 835, thus buckets 500-850 will be made


df <- df %>%
  mutate(ltv_bucket = cut(ltv, breaks = seq(0, 110, 10), include.lowest = TRUE),
         cs_bucket = cut(cs, breaks = seq(500, 850, 50), include.lowest = TRUE))

# Default rates by buckets
ltv_def_rates <- df %>%
  group_by(ltv_bucket) %>%
  summarise(def_rate = mean(default), n = n()) %>%
  ungroup()

cs_def_rates <- df %>%
  group_by(cs_bucket) %>%
  summarise(def_rate = mean(default), n = n()) %>%
  ungroup() %>%
  filter(!is.na(cs_bucket))

print(ltv_def_rates)
table2 <- xtable(ltv_def_rates)
print(table2)

print(cs_def_rates)
table3 <- xtable(cs_def_rates)
print(table3)

# Plots
ggplot(ltv_def_rates, 
       aes(x = ltv_bucket, 
           y = def_rate)) +
  geom_col() + 
  labs(title = 'Default Rates by LTV Buckets') +
  xlab('LTV bucket') +
  ylab('Default rate') +
  theme_minimal()

ggplot(cs_def_rates, 
       aes(x = cs_bucket, 
           y = def_rate)) +
  geom_col() + 
  labs(title = 'Default Rates by Credit Score Buckets') +
  xlab('Default rate') +
  ylab('Credit Score bucket') +
  theme_minimal()

###############################################################################
# 3. Predictive Modeling and ROC/AUC Computation
###############################################################################

############################
# Modeling
############################
# LTV bins
df <- df %>%
  mutate(
    ltv_bin = cut(
      ltv,
      breaks = seq(20, 100, by = 5),   
      right  = TRUE,                   
      include.lowest = TRUE
    )
  )
# 80% of the data will be used to make the train sample and the remaining - test sample
set.seed(2014)
train_vec <- sample(nrow(df), size = 0.8 * nrow(df))
df_train <- df[train_vec, ]
df_test <- df[-train_vec]

# Simple logistic regression
reg1 <- glm(default ~ ltv + cs + dti + int + upb, 
            data = df_train,
            family = 'binomial')

summary(reg1)

# Polynomial degree 2 logistic regression
reg2 <- glm(default ~ poly(ltv, 2) + poly(cs, 2) + poly(dti, 2) + poly(int, 2) + poly(upb, 2),
            data = df_train,
            family = 'binomial')
summary(reg2)

# LTV bins regression

reg3 <- glm(default ~ ltv_bin + cs + dti + int + upb, 
                    data = df_train,
                    family = 'binomial')

summary(reg3)

#######
# Estimating the predicted default probability on the test set
#######

prob1 <- predict(reg1, df_test, type = 'response')
prob2 <- predict(reg2, df_test, type = 'response')
prob3 <- predict(reg3, df_test, type = 'response')

################################################################################
# 4. ROC Curve and AUC
################################################################################
## Computing ROC Curve and TPR and FPR
# Model 1 

roc_model_1_data <- data.frame(
  prob = prob1,
  real = df_test$default
)

roc_model_1_data <- roc_model_1_data %>%
  arrange(desc(prob))

thresholds_1 <- sort(unique(roc_model_1_data$prob))

P1 <- sum(roc_model_1_data$real == 1)      # number of defaults
N1 <- sum(roc_model_1_data$real == 0)      # number of non-defaults

roc_points_1 <- data.frame(
  threshold = thresholds_1,
  TPR = NA,
  FPR = NA
)

for (i in seq_along(thresholds_1)) {
  t <- thresholds_1[i]
  
  # predict default if prob >= t
  pred <- ifelse(roc_model_1_data$prob >= t, 1, 0)
  
  TP <- sum(pred & roc_model_1_data$real == 1)
  FP <- sum(pred &roc_model_1_data$real == 0)
  
  roc_points_1$TPR[i] <- TP / P1
  roc_points_1$FPR[i] <- FP / N1
}

plot(roc_points_1$FPR, roc_points_1$TPR, type = "l",
     xlab = "False Positive Rate",
     ylab = "True Positive Rate",
     main = "Manual ROC Curve for Model 1",
     lwd = 2,                 # thicker ROC line
     col = "#1f78b4",         # nice blue
     cex.lab = 1.2,           # larger axis labels
     cex.main = 1.4,          # larger title
     cex.axis = 1.1)          # larger axis font
# add 45-degree line
abline(a = 0, b = 1, lty = 2, lwd = 2, col = "gray40")

# add grid 
grid(col = "gray90")

# Model 2

roc_model_2_data <- data.frame(
  prob = prob2,
  real = df_test$default
)

roc_model_2_data <- roc_model_2_data %>%
  arrange(desc(prob))

thresholds_2 <- sort(unique(roc_model_2_data$prob))

P2 <- sum(roc_model_2_data$real == 1)      # number of defaults
N2 <- sum(roc_model_2_data$real == 0)      # number of non-defaults

roc_points_2 <- data.frame(
  threshold = thresholds_2,
  TPR = NA,
  FPR = NA
)

for (i in seq_along(thresholds_2)) {
  t <- thresholds_2[i]
  
  # predict default if prob >= t
  pred <- ifelse(roc_model_2_data$prob >= t, 1, 0)
  
  TP <- sum(pred & roc_model_2_data$real == 1)
  FP <- sum(pred &roc_model_2_data$real == 0)
  
  roc_points_2$TPR[i] <- TP / P2
  roc_points_2$FPR[i] <- FP / N2
}

plot(roc_points_2$FPR, roc_points_2$TPR, type = "l",
     xlab = "False Positive Rate",
     ylab = "True Positive Rate",
     main = "Manual ROC Curve for Model 2",
     lwd = 2,                 # thicker ROC line
     col = "#1f78b4",         # nice blue
     cex.lab = 1.2,           # larger axis labels
     cex.main = 1.4,          # larger title
     cex.axis = 1.1)          # larger axis font
# add 45-degree line
abline(a = 0, b = 1, lty = 2, lwd = 2, col = "gray40")

# add grid 
grid(col = "gray90")


# Model 3
roc_model_3_data <- data.frame(
  prob = prob3,
  real = df_test$default
)

roc_model_3_data <- roc_model_3_data %>%
  arrange(desc(prob)) %>%
  filter(!is.na(prob))

thresholds_3 <- sort(unique(roc_model_3_data$prob))

P3 <- sum(roc_model_3_data$real == 1)      # number of defaults
N3 <- sum(roc_model_3_data$real == 0)      # number of non-defaults

roc_points_3 <- data.frame(
  threshold = thresholds_3,
  TPR = NA,
  FPR = NA
)

for (i in seq_along(thresholds_3)) {
  t <- thresholds_3[i]
  
  # predict default if prob >= t
  pred <- ifelse(roc_model_3_data$prob >= t, 1, 0)
  
  TP <- sum(pred == 1 & roc_model_3_data$real == 1)
  FP <- sum(pred == 1 & roc_model_3_data$real == 0)
  
  roc_points_3$TPR[i] <- TP / P3
  roc_points_3$FPR[i] <- FP / N3
}

plot(roc_points_3$FPR, roc_points_3$TPR, type = "l",
     xlab = "False Positive Rate",
     ylab = "True Positive Rate",
     main = "Manual ROC Curve for Model 3",
     lwd = 2,                 # thicker ROC line
     col = "#1f78b4",         # nice blue
     cex.lab = 1.2,           # larger axis labels
     cex.main = 1.4,          # larger title
     cex.axis = 1.1)          # larger axis font
# add 45-degree line
abline(a = 0, b = 1, lty = 2, lwd = 2, col = "gray40")

# add grid 
grid(col = "gray90")


## Computing AUC for the three model ROC Curves
roc_points_1 <- roc_points_1[order(roc_points_1$FPR), ]
roc_points_1 <- roc_points_1 %>% mutate(diff_area = (FPR - lag(FPR))*(TPR + lag(TPR))/2)
auc_manual_1 <- sum(roc_points_1$diff_area, na.rm=TRUE) 

roc_points_2 <- roc_points_2[order(roc_points_2$FPR), ]
roc_points_2 <- roc_points_2 %>% mutate(diff_area = (FPR - lag(FPR))*(TPR + lag(TPR))/2)
auc_manual_2 <- sum(roc_points_2$diff_area, na.rm=TRUE) 

roc_points_3 <- roc_points_3[order(roc_points_3$FPR), ]
roc_points_3 <- roc_points_3 %>% mutate(diff_area = (FPR - lag(FPR))*(TPR + lag(TPR))/2)
auc_manual_3 <- sum(roc_points_3$diff_area, na.rm=TRUE) 

############################
# Group-Specific ROC and AUC
############################
# For Climate risk, we use Property state, dividing coastal states and non coastal states
# Idea being that coastal states would have a higher likelihood of flooding
coastal_states <- c(# Atlantic Coast
  'ME', 'NH', 'MA', 'RI', 'CT', 'NY', 'NJ', 'DE', 'MD', 
  'VA', 'NC', 'SC', 'GA', 'FL',
  
  # Gulf of Mexico
  'AL', 'MS', 'LA', 'TX',
  
  # Pacific Coast
  'CA', 'OR', 'WA', 'AK', 'HI')

inland_states <- c('MT','ID','WY','NV', 'UT','CO','AZ','NM','ND','SD', 'NE','KS','OK','MO',
  'IA','MN','WI','IL','IN','MI','OH','AR','TN','KY','WV','VT','PA'
)

# Splitting the dataset into two groups
df_coastal <- df %>%
  filter(state %in% coastal_states)
df_inland <- df %>%
  filter(state %in% inland_states) # Note, this split the dataset into comparable data sets by size

# Splitting the datasets into test and training
train_vec_coast <- sample(nrow(df_coastal), size = 0.8 * nrow(df_coastal))
train_vec_inland <- sample(nrow(df_inland), size = 0.8 * nrow(df_inland))

df_train_coast <- df_coastal[train_vec_coast, ]
df_test_coast <- df_coastal[-train_vec_coast]

df_train_inland <- df_inland[train_vec_inland, ]
df_test_inland <- df_inland[-train_vec_inland]

# Recreating the models
# c for coastal, i for inland

c_reg1 <- glm(default ~ ltv + cs + dti + int + upb, 
              data = df_train_coast,
              family = 'binomial')
c_reg2 <- glm(default ~ poly(ltv, 2) + poly(cs, 2) + poly(dti, 2) + poly(int, 2) + poly(upb, 2),
              data = df_train_coast,
              family = 'binomial')
c_reg3 <- glm(default ~ ltv_bin + cs + dti + int + upb, 
            data = df_train_coast,
            family = 'binomial')

i_reg1 <- glm(default ~ ltv + cs + dti + int + upb, 
              data = df_train_inland,
              family = 'binomial')
i_reg2 <- glm(default ~ poly(ltv, 2) + poly(cs, 2) + poly(dti, 2) + poly(int, 2) + poly(upb, 2),
              data = df_train_inland,
              family = 'binomial')

i_reg3 <- glm(default ~ ltv_bin + cs + dti + int + upb, 
              data = df_train_inland,
              family = 'binomial')

# Predicting the model values
prob1_c <- predict(c_reg1, df_test_coast, type = 'response')
prob2_c <- predict(c_reg2, df_test_coast, type = 'response')
prob3_c <- predict(c_reg3, df_test_coast, type = 'response')

prob1_i <- predict(i_reg1, df_test_inland, type = 'response')
prob2_i <- predict(i_reg2, df_test_inland, type = 'response')
prob3_i <- predict(i_reg3, df_test_inland, type = 'response')


# Calculating ROC and AUC values
library(pROC)
# Coastal
roc1_c <- roc(df_test_coast$default, prob1_c)
roc2_c <- roc(df_test_coast$default, prob2_c)
roc3_c <- roc(df_test_coast$default, prob3_c)
# Inland
roc1_i <- roc(df_test_inland$default, prob1_i)
roc2_i <- roc(df_test_inland$default, prob2_i)
roc3_i <- roc(df_test_inland$default, prob3_i)

# AUC values
auc_c <- c(auc(roc1_c), auc(roc2_c), auc(roc3_c))
auc_i <- c(auc(roc1_i), auc(roc2_i), auc(roc3_i))
names <- c("Simple logistic regression", "Polynomial degree 2 logistic regression", "LTV bins regression")
auc_manual <- c(auc_manual_1, auc_manual_2, auc_manual_3)
auc_climate <- data.frame(
  Model = names,
  AUC_coastal = auc_c,
  AUC_inland = auc_i,
  AUC_all = auc_manual
)
table_auc_comp <- xtable(auc_climate)
print(table_auc_comp)

# Plots comparing the coastal vs inland model performance
par(mfrow = c(1, 3))

# Model 1 comparison
plot(roc1_c, col = "red", main = "Model 1: Coastal vs Inland", lwd = 2,
     xlab = "False Positive Rate",
     ylab = "True Positive Rate",
     ylim = c(0, 1),
     legacy.axes = TRUE
     )
plot(roc1_i, col = "blue", add = TRUE, lwd = 2)
legend("bottomright", 
       legend = c(paste0("Coastal (AUC=", round(auc(roc1_c), 3), ")"),
                  paste0("Inland (AUC=", round(auc(roc1_i), 3), ")")), 
       col = c("red", "blue"), lwd = 2)
grid(col = "gray90")

# Model 2 comparison
plot(roc2_c, col = "red", main = "Model 2: Coastal vs Inland", lwd = 2,
     xlab = "False Positive Rate",
     ylab = "True Positive Rate",
     ylim = c(0, 1),
     legacy.axes = TRUE
     )
plot(roc2_i, col = "blue", add = TRUE, lwd = 2)
legend("bottomright", 
       legend = c(paste0("Coastal (AUC=", round(auc(roc2_c), 3), ")"),
                  paste0("Inland (AUC=", round(auc(roc2_i), 3), ")")), 
       col = c("red", "blue"), lwd = 2)
grid(col = "gray90")

# Model 3 comparison
plot(roc3_c, col = "red", main = "Model 3: Coastal vs Inland", lwd = 2,
     xlab = "False Positive Rate",
     ylab = "True Positive Rate",
     ylim = c(0, 1),
     legacy.axes = TRUE
     )
plot(roc3_i, col = "blue", add = TRUE, lwd = 2)
legend("bottomright", 
       legend = c(paste0("Coastal (AUC=", round(auc(roc3_c), 3), ")"),
                  paste0("Inland (AUC=", round(auc(roc3_i), 3), ")")), 
       col = c("red", "blue"), lwd = 2)
grid(col = "gray90")







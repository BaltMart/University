### Assignment 3
# The following is the code which I use to obtain all data, tables and results for the task

# Loading the usual libraries
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(car)
library(sandwich)
library(lmtest)

# Setting the working directory
setwd("C:\\Users\\martynas\\Desktop\\uni\\Year 2\\DATA\\Task 3")

## 1.
## I have selected Year 2013 Wave, which I henceforth will use for further solutions

# Loading the data
install.packages("haven") #First time using this library
library(haven)

df <- read_dta("mip_educ13_en.dta")
head(df)#Checking if data loaded correctly
df_mod <- df |>
  mutate(konk_fac = as.factor(konk),
         bade = as.factor(bade))

## 2.
## Descriptive statistics of all of the variables I will be using for my analysis
install.packages("stargazer")
library(stargazer)

# Creating a vector of all the variables
vars <- c('pd',
          'konk',
          'bges',
          'ost',
          'exno',
          'bhsp',
          'geomld',
          'geomud',
          'geomeu',
          'bade')

# Creating a vector for renaming
var_names <- c('Innovations',
               'Level of competition',
               'Number of Emploees',
               'East Germany',
               'Exporter',
               'University Degrees',
               'Local',
               'Nation-wide',
               'EU-wide',
               'Public procurement')

# Creating a vector for name mapping
var_mapping <- setNames(var_names, vars)

# Calculating the means, sd and creating the table
results <- df %>%
  summarise(across(all_of(vars), 
                   list(Mean = ~ mean(., na.rm = TRUE),
                        St.Dev. = ~ sd(., na.rm = TRUE)))) %>%
  pivot_longer(everything(), 
               names_to = c("variable", ".value"), 
               names_sep = "_") %>%
  mutate(variable = var_mapping[variable]) %>%
  rename(Variable = variable)

#Rounding to get more readable results in a table later
results <- results %>%
  mutate(across(where(is.numeric), ~ round(.,3)))

#Creating a stargazer table to load to latex
stargazer(results,
          type = "latex",
          summary = FALSE,
          title = "Descriptive Statistics",
          rownames = FALSE)


## 4.
## Analyzing regressions 
#First regression testing without controls (except for industry variables)
Reg1 <- glm(pd ~ konk_fac + as.factor(branche), df_mod, family = binomial(link = "logit"))
summary(Reg1)

# Adding the following controls: divide of east and west(ost), number of employees (bges), university graduates in intervals (bhsp)
Reg2 <- glm(pd ~ konk_fac + as.factor(branche) + ost + bges + bhsp, df_mod, family = binomial(link = "logit"))
summary(Reg2)

# Adding the following controls to the first regression: exporter (exno), regionality (three variables: local - geomld, nation-wide - geomud, EU - geomeu, and other as a dummy)
Reg3 <- glm(pd ~ konk_fac + as.factor(branche) + exno + geomld + geomud + geomeu, df_mod, family = binomial(link = "logit"))
summary(Reg3)

#Adding all of the controls together
Reg4 <- glm(pd ~ konk_fac + as.factor(branche) + ost + bges + bhsp +
              exno + geomld + geomud + geomeu, df_mod, family = binomial(link = "logit"))
summary(Reg4)


#Creating a regression table
stargazer(Reg1, Reg2, Reg3, Reg4,
          type = "latex",
          omit = "as.factor",
          dep.var.labels = "Product innovations activities in the last 3 years",
          covariate.labels = c("1 to 5 Competitors", "6 to 10 Competitors", "11 to 15 Competitors",
                               "15 to 50 Competitors", "More thank 50 Competitors", 
                               "East Germany", "Number of Employees", "University Degrees", "Exporter",
                               "Local", "Nation-wide", "EU wide"))

## 5.
## Inference
#Creating a list of data with robust SEs
robust_list_se <- list(
  Reg1_rob <- coeftest(Reg1, vcov = vcovCL(Reg1, type = "HC3"))[,2],
  Reg2_rob <- coeftest(Reg2, vcov = vcovCL(Reg2, type = "HC3"))[,2],
  Reg3_rob <- coeftest(Reg3, vcov = vcovCL(Reg3, type = "HC3"))[,2]
)
#Creating a list of data with robust p values
robust_list_pval <- list(
  Reg1_rob <- coeftest(Reg1, vcov = vcovCL(Reg1, type = "HC3"))[,4],
  Reg2_rob <- coeftest(Reg2, vcov = vcovCL(Reg2, type = "HC3"))[,4],
  Reg3_rob <- coeftest(Reg3, vcov = vcovCL(Reg3, type = "HC3"))[,4]
)

#Creating a table for comparisons 
stargazer(Reg1, Reg2, Reg3, Reg1, Reg2, Reg3,
          type = "latex",
          se = list(NULL, NULL, NULL, robust_list_se[[1]], robust_list_se[[2]], robust_list_se[[3]]),
          p = list(NULL, NULL, NULL, robust_list_pval[[1]], robust_list_pval[[2]], robust_list_pval[[3]]),
          omit = c("as.factor", "exno", "bges", "bhsp", "exno", "geomld", "geomud", "geomeu", "ost"),
          covariate.labels = c("1 to 5 Competitors", "6 to 10 Competitors", "11 to 15 Competitors",
                               "15 to 50 Competitors", "More thank 50 Competitors"),
          dep.var.labels = "Product innovations activities in the last 3 years",
          column.labels = c("Reg1", "Reg2", "Reg3", "Reg1 Robust", "Reg2 Robust", "Reg3 Robust"))
## 6.
## Interaction
#Running a regression with interaction variable bade
moderator <- glm(pd ~ konk_fac*bade + as.factor(branche), data = df_mod, family = binomial(link = "logit"))
summary(moderator)

## 7.
## Visualization
install.packages("margins") #needed to calculate marginal effect of the moderator variable
library(margins)

mod_Margins <- margins(moderator, variables = "konk_fac", at = list(bade = c(0, 1))) #finding margins of moderator

#Extracting only the data that interest us from the summary of the margins function
summary_df <- summary(mod_Margins)[, c("factor", "bade", "AME", "SE", "z", "p", "lower", "upper")]
#Preparing data to visualize it
margins_df <- as.data.frame(summary(mod_Margins))

#Creating a table of marginal effects to use in Latex
stargazer(summary_df,
          type = "latex",
          summary = FALSE,
          title = "Margins summary"
)

#Creating a plot marginal effects
ggplot(margins_df, aes(x = factor, y = AME, color = factor(bade))) +
  geom_point(size = 3, position = position_dodge(width = 1)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(width = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "Marginal Effects by Competitor Levels and Public procurement",
       x = "Competitor Levels",
       y = "Average Marginal Effects (AME)",
       color = "Public procurement") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_discrete(labels = c("0", "1"))

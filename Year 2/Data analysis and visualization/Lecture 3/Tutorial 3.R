library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)

irs <- read.csv("C:\\Users\\martynas\\Desktop\\uni\\Year 2\\DATA\\Lecture 3\\IRS2019.csv", sep = ";")
View(irs)

#finding descriptives

mindivs = min(irs$Divis)

irs <- irs %>%
  mutate(Dolincome = TotalIncome*1000, DolSalaries = Salaries*1000, DolDivis = Divis*1000, DolRE = Retax*1000)

irs <- irs %>%
  mutate(HHincome = Dolincome/NumofReturns, HHSal = DolSalaries/NumofReturns, HHdivis = DolDivis/NumofReturns, HHRE = DolRE/NumofReturns)

irs <- irs %>%
  mutate(Fracdivis = NumofRetwithDivis/NumofReturns)

boxplot(irs$Fracdivis)

irs$Fracdivis %>% summary

#Richest/Poorest places
irs %>% filter(HHSal > 601000)
irs %>% filter(HHSal < 3550)

plot(x = irs$HHincome, y = irs$Fracdivis)
#to look at skedasticity, plot the residuals to check
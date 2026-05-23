library(tidyverse)
library(tidyr)
library(dplyr)

mlb = read.csv("C:\\Users\\martynas\\Desktop\\uni\\Year 2\\The data thing\\Lecture 2\\MLB.csv", sep = ";")

#converting to cm and kg
MLB = MLB %>%
  mutate(Height1 = Height*2.54, Weight1 = Weight*0.45359237)

#find the minimum height, weight, age
minage = min(mlb[,6])
minheight = min(mlb[,4])
minweight = min(mlb[,5])

#find the maximum height, weight, age
maxage = max(mlb[,6])
maxheight = max(mlb[,4])
maxweight = max(mlb[,5])

#find the means
meanage = mean(MLB$Age)
meanheight = mean(MLB$Height)
meanweight = mean(MLB$Weight)

#find standard deviations

sdage = sd(MLB[,6])
sdheight = sd(MLB[,4])
sdweight = sd(MLB[,5])

#boxplot
boxplot(mlb$Height1)
mlb$Height1 %>% summary

# Plot
## ggplot
library(ggplot2)
ggplot(aes(x = Height1, y = Weight1, colour = Team), data = mlb) + geom_point()

#Weight vs age
plot(x = MLB$Age, y = MLB$Weight1)

#Regression
out = lm(Weight1 ~ Age, data = MLB)
summary(out)

plot(x = MLB$Age, y = MLB$Weight1)
abline(out, col = 2)
summary(out)

ggplot(aes(x = Age, y = Weight1), data = MLB) + geom_point() + geom_smooth(method = "lm")
ggplot(aes(x = Age, y = Weight1), data = MLB) + geom_point() + geom_smooth(method = "lm") + facet_wrap("Position")

out2 = lm(Weight1 ~ Age + Height1, data = MLB)
summary(out2)
plot(residuals(out2))

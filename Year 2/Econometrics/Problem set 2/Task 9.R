library(ggplot2)
library(dplyr)
library(readr)

# Reading data
df = read.csv("C:\\Users\\martynas\\Desktop\\uni\\Year 2\\Econometrics\\Problem set 2\\Earnings_and_Height.csv", header = TRUE)

# View the dataset
head(df)


#A
#i

avg_earnings_short = df %>%
  filter(height <= 67) %>%
  summarize(avg_earnings = mean(earnings))

avg_earnings_short

#ii
avg_earnings_tall = df %>%
  filter(height > 67) %>%
  summarize(avg_earnings = mean(earnings, na.rm = TRUE))

avg_earnings_tall

#iii
diff_earnings = avg_earnings_tall$avg_earnings - avg_earnings_short$avg_earnings
diff_earnings

#iv

# New variable `is_tall`
df$is_tall = ifelse(df$height > 67, 1, 0)

# Run a regression of earnings on is_tall
model = lm(earnings ~ is_tall, data = df)

# Summary of the model
summary(model)

# Confidence interval for the difference in earnings
confint(model, level = 0.95)

#B
#i
# Scatterplot of earnings vs height
ggplot(df, aes(x = height, y = earnings)) +
  geom_point() +
  labs(title = "Scatterplot of Earnings vs Height", x = "Height (inches)", y = "Earnings")

#4b
# Bivariate regression of earnings on height
model_height = lm(earnings ~ height, data = df)

# Summary of the regression
summary(model_height)

# Predict earnings for workers of different heights
predicted_earnings = predict(model_height, newdata = data.frame(height = c(65, 67, 70)))
predicted_earnings


# Get R-squared value
summary(model_height)$r.squared

# Convert height from inches to centimeters (1 inch = 2.54 cm)
df$height_cm = df$height * 2.54

# Run the height-earnings regression using height in centimeters
model_cm = lm(earnings ~ height_cm, data = df)

# Summary of the new model
summary(model_cm)

# Slope and intercept of the original model (in inches)
slope_inches = coef(model_height)["height"]
intercept_inches = coef(model_height)["(Intercept)"]
r_squared_inches = summary(model_height)$r.squared

# Slope and intercept of the new model (in cm)
slope_cm = coef(model_cm)["height_cm"]
intercept_cm = coef(model_cm)["(Intercept)"]
r_squared_cm = summary(model_cm)$r.squared

# Compare the two models
comparison = data.frame(
  Model = c("Inches", "Centimeters"),
  Slope = c(slope_inches, slope_cm),
  Intercept = c(intercept_inches, intercept_cm),
  R_squared = c(r_squared_inches, r_squared_cm)
)

comparison

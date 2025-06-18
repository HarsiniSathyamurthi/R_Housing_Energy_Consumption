# ----------------------------------------------------------
# Dataset: Appliance Energy Prediction
# ----------------------------------------------------------

# Clearing steps
rm(list = ls())  # Clear environment
dev.off()        # Clear plots
cat("\014")      # Clear console, ASCII character Form Feed (\014 or CTRL+L) 


# Setup working directory
setwd("E:/Project")


# Install and Load required libraries
if (!require("tidyverse")) install.packages("tidyverse")
install.packages("dplyr")
install.packages("httr")
install.packages("readr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("ggcorrplot")
install.packages("car")
install.packages("lmtest")

library(tidyverse)     # Data wrangling and visualization
library(dplyr)         # Data Manipulation
library(httr)          # Accessing APIs
library(readr)         # Read data files
library(naniar)        # Handling missing data
library(ggplot2)       # Visualisation
library(corrplot)      # Correlation plots
library(ggcorrplot)    # Better Correlation plots
library(car)           # Regression tools
library(lmtest)        # Regression tests

# ----------------------------------------------------------
# Importing the Appliance Energy Prediction dataset
# ----------------------------------------------------------

# Importing the Appliances Energy dataset from the local copy
#energy_data <- read.csv("energydata_complete.csv")

# Importing data directly from the link
# Sending request and get content of the mentioned dataset
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00374/energydata_complete.csv"
response <- GET(url)
content <- content(response, "text")
energy_data <- read_csv(content) # Read the CSV content into the energy data frame

# ----------------------------------------------------------
# Inspecting and Modifying the dataset
# ----------------------------------------------------------

# General exploration of the dataset
str(energy_data)          # Gives the structure of the dataset
dim(energy_data)          # Dimensions (rows, cols)
head(energy_data)         # First few rows
names(energy_data)        # Displays the column names


# Assigning new column names for easy interpretation and readability
new_column_names <- c("Timestamp", "Appliances_Wh", "Lights_Wh", "Temp_Kitchen", "Humidity_Kitchen",
                      "Temp_Living", "Humidity_Living", "Temp_Laundry", "Humidity_Laundry",
                      "Temp_Office", "Humidity_Office", "Temp_Bathroom", "Humidity_Bathroom",
                      "Temp_Outside_North", "Humidity_Outside_North", "Temp_Ironing", "Humidity_Ironing",
                      "Temp_Teenager_Room", "Humidity_Teenager_Room", "Temp_Parents_Room", "Humidity_Parents_Room",
                      "Temp_Outside_Weather", "Pressure_mmHg", "Humidity_Outside_Weather",
                      "Wind_Speed_mps", "Visibility_km", "Dew_Point_C", "Random_V1", "Random_V2")
names(energy_data) <- new_column_names

# Checking if the new column names are updated correctly
summary(energy_data)
head(energy_data)
View(energy_data)

# Converting timestamp to datetime and extracting useful time features
energy_data$date <- as.POSIXct(energy_data$Timestamp)
energy_data$hour <- as.numeric(format(energy_data$Timestamp, "%H"))
energy_data$day <- weekdays(energy_data$Timestamp)

glimpse(energy_data)      # View structure of the data
dim(energy_data)          # Dimensions (rows, cols)

# ----------------------------------------------------------
# Outlier removal for Appliances_Wh (Target Variable)
# ----------------------------------------------------------

# Remove top and bottom 1% outliers from Appliances_Wh
# This helps stabilize the regression model by reducing skew
energy_data_clean <- energy_data %>%
  filter(
    Appliances_Wh >= quantile(Appliances_Wh, 0.01),
    Appliances_Wh <= quantile(Appliances_Wh, 0.99)
  )

summary(energy_data_clean$Appliances_Wh)
head(energy_data)
energy_data <- energy_data_clean


# ----------------------------------------------------------
# Missing values handling (nil in this dataset)
# ----------------------------------------------------------

sum(is.na(energy_data))   # Theoretical check
                          # If =  0, there is no missing values in the dataset
vis_miss(energy_data)     # Visualize missing data patterns

# ----------------------------------------------------------
# DESCRIPTIVE STATISTICS AND VISUALISATIONS
# Visualisations and Graphical Analysis
# ----------------------------------------------------------

# Descriptive Statistics
summary(energy_data)

# Histogram of Appliances energy use
ggplot(energy_data, aes(x = Appliances_Wh)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30, aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(energy_data$Appliances_Wh), sd = sd(energy_data$Appliances_Wh)), 
                color = "red", linetype = "dashed") +
  labs(title = "Distribution of Appliances Energy Use", x = "Appliances (Wh)", y = "Density")


# Scatter plot: Outdoor Temperature vs Appliances
ggplot(energy_data, aes(x = Temp_Outside_Weather, y = Appliances_Wh)) +
  geom_point(color = "lightgreen") +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  labs(title = "Outdoor Temperature vs Appliances Energy Use", 
       x = "Temperature Living ", y = "Appliances (Wh)") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "darkred") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"))

# Boxplot: Appliances by hour
ggplot(energy_data, aes(x = factor(hour), y = Appliances_Wh)) +
  geom_boxplot(fill = "violet", outlier.shape = 1, outlier.color = "gray") +
  labs(title = "Appliances Energy Use by Hour", 
       x = "Hour of Day", y = "Appliances (Wh)") + 
  theme_minimal()

# Density plot with a normal curve
ggplot(energy_data, aes(x = Appliances_Wh)) +
  geom_density(fill = "yellow", alpha = 0.5) +
  stat_function( #helps in adding a cuvre. it is a mathematical function
    fun = dnorm, #fun -> function name; #dnorm = normal distribution
    args = list(mean = mean(energy_data$Appliances_Wh), sd = sd(energy_data$Appliances_Wh)), #used to show the two different values
    color = "red", linetype = "dashed", size = 1
  ) +
  labs(title = "Density Plot of Appliances_Wh with Normal Curve",
       x = "Appliances (Wh)", y = "Density") +
  theme_light()


# ----------------------------------------------------------
# Confidence Interval
# ----------------------------------------------------------

# 95% Confidence Interval for Appliances_Wh
ci <- t.test(energy_data$Appliances_Wh)
print(ci)

# ----------------------------------------------------------
# Correlation Analysis
# ----------------------------------------------------------
numeric_vars <- energy_data %>% select_if(is.numeric)  # Select only numeric columns

cor_matrix <- cor(numeric_vars, method = "spearman")

dev.off()  # Clears the active graphics device
graphics.off()  # Completely resets the graphics system

ggcorrplot(cor_matrix, method = "square", type = "upper",
           lab = FALSE,
           colors = c("red", "white", "blue"),
           title = "Appliances Energy Use Correlation Matrix")
dev.off()

# ----------------------------------------------------------
# Cross-Tabulation
# ----------------------------------------------------------

# Create a cross-tabulation between Hour and Appliances_Wh
cross_tab <- table(energy_data$hour, energy_data$Appliances_Wh) 
print(cross_tab)

margin.table(cross_tab, 1)  # Row frequencies (per Hour)

# ----------------------------------------------------------
# Hypothesis Testing
# ----------------------------------------------------------

# One-Sample t-test: Is the mean Appliances energy use > 200 Wh?
mean(energy_data$Appliances_Wh)
t.test(energy_data$Appliances_Wh, mu = 200, alternative = "greater", conf.level = 0.95)
# H0: Mean Appliances energy use = 200 Wh
# H1: Mean Appliances energy use > 200 Wh
# mu = 200: (null hypothesis) assumed mean
# P-value < 0.05? → If yes, reject H0

# Two-Sided t-test: Is mean Wind Speed different from 2 m/s?
mean(energy_data$Wind_Speed_mps)
t.test(energy_data$Wind_Speed_mps, mu = 2, alternative = "two.sided", conf.level = 0.95)
# H0: Mean Wind Speed = 2 m/s
# H1: Mean Wind Speed ≠ 2 m/s
# Two-tailed test → checks if the mean is either lower or higher than 2

# Independent t-test: Compare Temperature in Living Room vs. Kitchen
t.test(energy_data$Temp_Living, energy_data$Temp_Kitchen, 
       alternative = "two.sided", conf.level = 0.95, var.equal = TRUE)
# H0: Mean Temperature (Living Room) = Mean Temperature (Kitchen)
# H1: Mean Temperature (Living Room) ≠ Mean Temperature (Kitchen)


# ----------------------------------------------------------
# ANOVA
# ----------------------------------------------------------

# Perform ANOVA to test differences in Appliances energy use across hours
res.aov <- aov(Appliances_Wh ~ factor(hour), data = energy_data)

summary(res.aov)

# ----------------------------------------------------------
# Multiple Linear Regression Models
# ----------------------------------------------------------

# Initial model with all numeric predictors (excluding timestamps and random variables)
mlr_model <- lm(Appliances_Wh ~ . - Timestamp - date - day - Lights_Wh - Random_V1 - Random_V2, 
                data = energy_data)
summary(mlr_model)  # Note significant predictors (p < 0.05)

# Diagnostic plots for initial model
par(mfrow = c(2, 2))  # Set up 2x2 plotting area
plot(mlr_model)       # Residuals vs Fitted, Normal Q-Q, Scale-Location, Residuals vs Leverage
par(mfrow = c(1, 1))  # Reset plotting layout

# ----------------------------------------------------------
# Refined Model - Keeping Only Significant Predictors
# ----------------------------------------------------------
mlr_refined <- lm(Appliances_Wh ~ Temp_Living + Humidity_Living + Temp_Kitchen + 
                    hour + Pressure_mmHg + Dew_Point_C, 
                  data = energy_data)
summary(mlr_refined)  # Check R² improvement

# Diagnostic plots for refined model
par(mfrow = c(2, 2))
plot(mlr_refined)
par(mfrow = c(1, 1))

# ----------------------------------------------------------
# Model with Lagged Feature
# ----------------------------------------------------------
# Create lagged variable and remove NA rows
energy_data <- energy_data %>%
  mutate(lag_Appliances = lag(Appliances_Wh, 1)) %>%
  filter(!is.na(lag_Appliances))  # Remove NA rows

mlr_lagged <- lm(Appliances_Wh ~ Temp_Living + Humidity_Living + lag_Appliances + hour, 
                 data = energy_data)
summary(mlr_lagged)  # R² should improve significantly

# ----------------------------------------------------------
# Final Combined Model
# ----------------------------------------------------------
final_model <- lm(Appliances_Wh ~ Temp_Living * Humidity_Living + 
                    Temp_Kitchen * Humidity_Kitchen + 
                    lag_Appliances + hour, 
                    data = energy_data)
summary(final_model)
anova(final_model)
plot(final_model)
par(mfrow = c(2, 2))  # Set up 2x2 plotting area
plot(final_model)

# ----------------------------------------------------------
# Comprehensive Model Tests
# ----------------------------------------------------------

# Residual Analysis using Histogram
ggplot(energy_data, aes(x = final_model$residuals)) +
  geom_histogram(aes(y = ..density..), fill = "steelblue", color = "black", bins = 30) +
  stat_function(fun = dnorm, args = list(mean = mean(final_model$residuals), sd = sd(final_model$residuals)), 
                color = "red", size = 1) +
  labs(title = "Distribution of Model Residuals", subtitle = "With Normal Distribution Curve",
       x = "Residuals", y = "Density") +
  theme_minimal()

# Plot influential observations
plot(final_model, 4)  # Cook's distance plot
energy_data[c(1432, 3414, 17490), ]  # View influential observations

# Homoscedasticity Tests
bptest(final_model)  # p-value < 0.05 indicates heteroscedasticity

# Normality Test
# Shapiro-Wilk test for normality - Resulting in error 
# Reason: 5000 is the maximum range for this function
shapiro_test <- shapiro.test(final_model$residuals)

# Multicollinearity Check
# Variance Inflation Factors (VIF)
car::vif(final_model)

# ----------------------------------------------------------
# Final Model Performance
# ----------------------------------------------------------
summary(final_model)$r.squared

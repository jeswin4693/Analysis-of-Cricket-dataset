# Import required libraries
library(tidyverse)
library(caret)
library(stats)
library(corrplot)

# Read the data
cricket_data <- read.csv("BattingHundredsCareer.csv")

# Data preprocessing
# Remove '+' from X4s and X6s columns and convert to numeric
cricket_data$X4s <- as.numeric(gsub("\\+", "", cricket_data$X4s))
cricket_data$X6s <- as.numeric(gsub("\\+", "", cricket_data$X6s))

# Convert Span to number of years
cricket_data$Years <- sapply(strsplit(cricket_data$Span, "-"), function(x) {
  as.numeric(x[2]) - as.numeric(x[1]) + 1
})

# Create multiple linear regression models

# Model 1: Predicting Runs based on Innings, X4s, X6s
model1 <- lm(Runs ~ Inns + X4s + X6s, data = cricket_data)

# Model 2: Predicting Average based on Strike Rate, X4s, X6s
model2 <- lm(Ave ~ SR + X4s + X6s, data = cricket_data)

# Print model summaries
summary(model1)
summary(model2)

# Create visualizations
# Scatter plot for actual vs predicted runs
predicted_runs <- predict(model1)
plot(cricket_data$Runs, predicted_runs,
     main = "Actual vs Predicted Runs",
     xlab = "Actual Runs",
     ylab = "Predicted Runs")
abline(0, 1, col = "red")

# Diagnostic plots for model1
par(mfrow = c(2,2))
plot(model1)

# Feature importance
importance1 <- abs(coef(model1))[-1]  # Remove intercept
barplot(importance1, 
        main = "Feature Importance for Runs Prediction",
        names.arg = names(importance1),
        las = 2)

# Correlation matrix for numeric variables
numeric_vars <- cricket_data %>% 
  select_if(is.numeric)

# Create correlation matrix
correlation_matrix <- cor(numeric_vars, use = "complete.obs")

# Create correlation plot
corrplot(correlation_matrix, 
         method = "color",
         type = "upper",
         order = "hclust",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         main = "\nCorrelation Matrix")

# Print correlation matrix with rounded values
print(round(correlation_matrix, 2))

# Cross-validation
set.seed(123)
train_control <- trainControl(method = "cv", number = 5)
cv_model <- train(Runs ~ Inns + X4s + X6s,
                  data = cricket_data,
                  method = "lm",
                  trControl = train_control)
print(cv_model)

# Predictions with confidence intervals
new_data <- data.frame(
  Inns = c(20, 25, 30),
  X4s = c(100, 120, 140),
  X6s = c(20, 25, 30)
)

predictions <- predict(model1, newdata = new_data, interval = "confidence")
print(predictions)

# Calculate R-squared and RMSE
r2 <- summary(model1)$r.squared
rmse <- sqrt(mean((cricket_data$Runs - predicted_runs)^2))
cat("R-squared:", r2, "\n")
cat("RMSE:", rmse, "\n")
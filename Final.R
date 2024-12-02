# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readxl)  # For reading Excel files
library(caret)   # For model performance metrics

# Load the cleaned data
data <- read_excel("Cleaned_Abnb_Data.xlsx")

# Check structure of the data
str(data)

# Updated Full Model with Log Price
model_full_log <- lm(log_price ~ room_type + number_of_reviews + reviews_per_month + availability_rate, data = data)
summary(model_full_log)

# Residual Diagnostics
# Residuals vs. Fitted Values
ggplot(data, aes(x = predict(model_full_log), y = residuals(model_full_log))) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values (Log Model)",
       x = "Fitted Values",
       y = "Residuals")

# Normal Q-Q Plot for Residuals
qqnorm(residuals(model_full_log))
qqline(residuals(model_full_log), col = "red")

# Model Performance Metrics
data$predicted_log_price <- predict(model_full_log)
data$predicted_price <- exp(data$predicted_log_price) # Back-transform to original price scale
actual_vs_predicted <- data.frame(
  Actual = data$price,
  Predicted = data$predicted_price
)

# Calculate Performance Metrics
rmse <- sqrt(mean((actual_vs_predicted$Actual - actual_vs_predicted$Predicted)^2))
mae <- mean(abs(actual_vs_predicted$Actual - actual_vs_predicted$Predicted))
r2 <- cor(actual_vs_predicted$Actual, actual_vs_predicted$Predicted)^2

cat("Model Performance Metrics:\n")
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("R-squared:", r2, "\n")

# Visualization: Actual vs Predicted Prices
ggplot(actual_vs_predicted, aes(x = Predicted, y = Actual)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  scale_x_log10() + scale_y_log10() + # Use log scales for better visualization
  labs(title = "Actual vs Predicted Prices (Improved Model)",
       x = "Predicted Price",
       y = "Actual Price")

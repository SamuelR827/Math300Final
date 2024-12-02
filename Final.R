# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readxl)

# Load the cleaned data
data <- read_excel("Filtered_Cleaned_Abnb_Data.xlsx")

# Descriptive Statistics
summary(data)

# Boxplots for Price by Room Type
ggplot(data, aes(x = room_type, y = price)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Price by Room Type",
       x = "Room Type",
       y = "Price")

# Histogram of Prices
ggplot(data, aes(x = price)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histogram of Prices",
       x = "Price",
       y = "Count")

# ---- Function for Confidence Intervals and Hypothesis Testing ----
display_conf_intervals <- function(model, model_name) {
  cat("Confidence Intervals for", model_name, "Model:\n")
  print(confint(model))
  cat("\n")
}

perform_hypothesis_testing <- function(model, predictors) {
  for (predictor in predictors) {
    p_value <- summary(model)$coefficients[predictor, 4]
    cat("P-value for", predictor, ": ", p_value, "\n")
    if (p_value < 0.05) {
      cat(predictor, "has a significant effect on Price (reject H0).\n")
    } else {
      cat(predictor, "does not have a significant effect on Price (fail to reject H0).\n")
    }
    cat("\n")
  }
}

# ---- Regression Models, Visualizations, Residual Analysis, and Hypothesis Testing ----

# 1. Price vs Availability (availability_365)
model_availability <- lm(price ~ availability_365, data = data)
summary(model_availability)

# Visualization
ggplot(data, aes(x = availability_365, y = price)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +
  labs(title = "Price vs. Availability (availability_365)",
       x = "Availability (days per year)",
       y = "Price")

# Residual Analysis
ggplot(data, aes(x = predict(model_availability), y = resid(model_availability))) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted (Availability)",
       x = "Fitted Values",
       y = "Residuals")

# Confidence Intervals and Hypothesis Testing
display_conf_intervals(model_availability, "Availability")
perform_hypothesis_testing(model_availability, c("availability_365"))

# 2. Price vs Number of Reviews
model_reviews <- lm(price ~ number_of_reviews, data = data)
summary(model_reviews)

# Visualization
ggplot(data, aes(x = number_of_reviews, y = price)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +
  labs(title = "Price vs. Number of Reviews",
       x = "Number of Reviews",
       y = "Price")

# Residual Analysis
ggplot(data, aes(x = predict(model_reviews), y = resid(model_reviews))) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted (Number of Reviews)",
       x = "Fitted Values",
       y = "Residuals")

# Confidence Intervals and Hypothesis Testing
display_conf_intervals(model_reviews, "Number of Reviews")
perform_hypothesis_testing(model_reviews, c("number_of_reviews"))

# 3. Price vs Reviews Per Month
model_reviews_per_month <- lm(price ~ reviews_per_month, data = data)
summary(model_reviews_per_month)

# Visualization
ggplot(data, aes(x = reviews_per_month, y = price)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +
  labs(title = "Price vs. Reviews Per Month",
       x = "Reviews Per Month",
       y = "Price")

# Residual Analysis
ggplot(data, aes(x = predict(model_reviews_per_month), y = resid(model_reviews_per_month))) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted (Reviews Per Month)",
       x = "Fitted Values",
       y = "Residuals")

# Confidence Intervals and Hypothesis Testing
display_conf_intervals(model_reviews_per_month, "Reviews Per Month")
perform_hypothesis_testing(model_reviews_per_month, c("reviews_per_month"))

# 4. Combined Reviews Model: Number of Reviews and Reviews Per Month
model_reviews_combined <- lm(price ~ number_of_reviews + reviews_per_month, data = data)
summary(model_reviews_combined)

# Visualization
ggplot(data, aes(x = predict(model_reviews_combined), y = price)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Prices (Combined Reviews)",
       x = "Predicted Price",
       y = "Actual Price")

# Residual Analysis
ggplot(data, aes(x = predict(model_reviews_combined), y = resid(model_reviews_combined))) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted (Combined Reviews)",
       x = "Fitted Values",
       y = "Residuals")

# Confidence Intervals and Hypothesis Testing
display_conf_intervals(model_reviews_combined, "Combined Reviews")
perform_hypothesis_testing(model_reviews_combined, c("number_of_reviews", "reviews_per_month"))

# 5. Full Model: All Variables Together
model_full <- lm(price ~ room_type + number_of_reviews + reviews_per_month + availability_365, data = data)
summary(model_full)

# Visualization
data$predicted_price <- predict(model_full)
ggplot(data, aes(x = predicted_price, y = price)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Prices (Full Model)",
       x = "Predicted Price",
       y = "Actual Price")

# Residual Analysis
ggplot(data, aes(x = predict(model_full), y = resid(model_full))) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted (Full Model)",
       x = "Fitted Values",
       y = "Residuals")

# Confidence Intervals and Hypothesis Testing
display_conf_intervals(model_full, "Full")
perform_hypothesis_testing(model_full, c("room_typePrivate room", "room_typeShared room", 
                                         "number_of_reviews", "reviews_per_month", "availability_365"))

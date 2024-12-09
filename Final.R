# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readxl)

# Load the cleaned data
data <- read.csv("filtered_data2.csv")

# Descriptive Statistics
summary(data)

# Histogram of Prices
p2 <- ggplot(data, aes(x = price)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histogram of Prices",
       x = "Price",
       y = "Count")
p2
ggsave("histogram_of_prices.png", plot = p2, width = 8, height = 6)

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

# Boxplots for Price by Room Type
# ---- Regression Model for Price by Room Type ----
model_room_type <- lm(price ~ room_type, data = data)
summary(model_room_type)

p1 <- ggplot(data, aes(x = room_type, y = price)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Price by Room Type",
       x = "Room Type",
       y = "Price")
p1
ggsave("boxplot_price_by_room_type.png", plot = p1, width = 8, height = 6)

# 1. Price vs Availability (availability_365)
model_availability <- lm(price ~ availability_365, data = data)
summary(model_availability)

p3 <- ggplot(data, aes(x = availability_365, y = price)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +
  labs(title = "Price vs. Availability (availability_365)",
       x = "Availability (days per year)",
       y = "Price")
p3
ggsave("price_vs_availability.png", plot = p3, width = 8, height = 6)

# Residual Analysis for Availability
p4 <- ggplot(data, aes(x = predict(model_availability), y = resid(model_availability))) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted (Availability)",
       x = "Fitted Values",
       y = "Residuals")
p4
ggsave("residuals_availability.png", plot = p4, width = 8, height = 6)

# Confidence Intervals and Hypothesis Testing
display_conf_intervals(model_availability, "Availability")
perform_hypothesis_testing(model_availability, c("availability_365"))

# 2. Price vs Number of Reviews
model_reviews <- lm(price ~ number_of_reviews, data = data)
summary(model_reviews)

p5 <- ggplot(data, aes(x = number_of_reviews, y = price)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +
  labs(title = "Price vs. Number of Reviews",
       x = "Number of Reviews",
       y = "Price")
p5
ggsave("price_vs_number_of_reviews.png", plot = p5, width = 8, height = 6)

# Residual Analysis for Number of Reviews
p6 <- ggplot(data, aes(x = predict(model_reviews), y = resid(model_reviews))) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted (Number of Reviews)",
       x = "Fitted Values",
       y = "Residuals")
p6
ggsave("residuals_number_of_reviews.png", plot = p6, width = 8, height = 6)

# 3. Price vs Reviews Per Month
model_reviews_per_month <- lm(price ~ reviews_per_month, data = data)
summary(model_reviews_per_month)

p7 <- ggplot(data, aes(x = reviews_per_month, y = price)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +
  labs(title = "Price vs. Reviews Per Month",
       x = "Reviews Per Month",
       y = "Price")
p7
ggsave("price_vs_reviews_per_month.png", plot = p7, width = 8, height = 6)

# Residual Analysis for Reviews Per Month
p8 <- ggplot(data, aes(x = predict(model_reviews_per_month), y = resid(model_reviews_per_month))) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted (Reviews Per Month)",
       x = "Fitted Values",
       y = "Residuals")
p8
ggsave("residuals_reviews_per_month.png", plot = p8, width = 8, height = 6)



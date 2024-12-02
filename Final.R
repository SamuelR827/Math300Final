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

# ---- Regression Models, Residual Analysis, Hypothesis Testing, and Confidence Intervals ----

# Function to display confidence intervals for regression coefficients
display_conf_intervals <- function(model) {
  conf_int <- confint(model)
  print(conf_int)
}

# 1. Price vs Availability (availability_365)
model_availability <- lm(price ~ availability_365, data = data)
summary(model_availability)

# Residual Analysis
ggplot(data, aes(x = predict(model_availability), y = resid(model_availability))) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted (Availability)",
       x = "Fitted Values",
       y = "Residuals")

# Confidence Intervals
cat("Confidence Intervals for Availability Model:\n")
display_conf_intervals(model_availability)

# Hypothesis Testing
cat("P-value for Availability: ", summary(model_availability)$coefficients["availability_365", 4], "\n")
if (summary(model_availability)$coefficients["availability_365", 4] < 0.05) {
  cat("Availability has a significant effect on Price (reject H0).\n")
} else {
  cat("Availability does not have a significant effect on Price (fail to reject H0).\n")
}

# 2. Price vs Number of Reviews
model_reviews <- lm(price ~ number_of_reviews, data = data)
summary(model_reviews)

# Residual Analysis
ggplot(data, aes(x = predict(model_reviews), y = resid(model_reviews))) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted (Reviews)",
       x = "Fitted Values",
       y = "Residuals")

# Confidence Intervals
cat("Confidence Intervals for Reviews Model:\n")
display_conf_intervals(model_reviews)

# Hypothesis Testing
cat("P-value for Number of Reviews: ", summary(model_reviews)$coefficients["number_of_reviews", 4], "\n")
if (summary(model_reviews)$coefficients["number_of_reviews", 4] < 0.05) {
  cat("Number of Reviews has a significant effect on Price (reject H0).\n")
} else {
  cat("Number of Reviews does not have a significant effect on Price (fail to reject H0).\n")
}

# 3. Price vs Reviews Per Month
model_reviews_per_month <- lm(price ~ reviews_per_month, data = data)
summary(model_reviews_per_month)

# Residual Analysis
ggplot(data, aes(x = predict(model_reviews_per_month), y = resid(model_reviews_per_month))) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted (Reviews Per Month)",
       x = "Fitted Values",
       y = "Residuals")

# Confidence Intervals
cat("Confidence Intervals for Reviews Per Month Model:\n")
display_conf_intervals(model_reviews_per_month)

# Hypothesis Testing
cat("P-value for Reviews Per Month: ", summary(model_reviews_per_month)$coefficients["reviews_per_month", 4], "\n")
if (summary(model_reviews_per_month)$coefficients["reviews_per_month", 4] < 0.05) {
  cat("Reviews Per Month has a significant effect on Price (reject H0).\n")
} else {
  cat("Reviews Per Month does not have a significant effect on Price (fail to reject H0).\n")
}

# 4. Combined Reviews Model: Number of Reviews and Reviews Per Month
model_reviews_combined <- lm(price ~ number_of_reviews + reviews_per_month, data = data)
summary(model_reviews_combined)

# Residual Analysis
ggplot(data, aes(x = predict(model_reviews_combined), y = resid(model_reviews_combined))) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted (Combined Reviews)",
       x = "Fitted Values",
       y = "Residuals")

# Confidence Intervals
cat("Confidence Intervals for Combined Reviews Model:\n")
display_conf_intervals(model_reviews_combined)

# Hypothesis Testing
cat("P-value for Number of Reviews: ", summary(model_reviews_combined)$coefficients["number_of_reviews", 4], "\n")
cat("P-value for Reviews Per Month: ", summary(model_reviews_combined)$coefficients["reviews_per_month", 4], "\n")

# 5. Full Model: All Variables Together
model_full <- lm(price ~ room_type + number_of_reviews + reviews_per_month + availability_365, data = data)
summary(model_full)

# Residual Analysis
ggplot(data, aes(x = predict(model_full), y = resid(model_full))) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted (Full Model)",
       x = "Fitted Values",
       y = "Residuals")

# Confidence Intervals
cat("Confidence Intervals for Full Model:\n")
display_conf_intervals(model_full)

# Hypothesis Testing
cat("P-value for Room Type (Private Room): ", summary(model_full)$coefficients["room_typePrivate room", 4], "\n")
cat("P-value for Room Type (Shared Room): ", summary(model_full)$coefficients["room_typeShared room", 4], "\n")
cat("P-value for Number of Reviews: ", summary(model_full)$coefficients["number_of_reviews", 4], "\n")
cat("P-value for Reviews Per Month: ", summary(model_full)$coefficients["reviews_per_month", 4], "\n")
cat("P-value for Availability: ", summary(model_full)$coefficients["availability_365", 4], "\n")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readxl)  # For reading Excel files

# Load the cleaned data
data <- read_excel("Filtered_Cleaned_Abnb_Data.xlsx")

# Simple Regression: Single Variable Analysis
# Regression: Price vs. Availability (availability_365)
model_availability <- lm(price ~ availability_365, data = data)
summary(model_availability)

# Regression: Price vs. Number of Reviews
model_reviews <- lm(price ~ number_of_reviews, data = data)
summary(model_reviews)

# Regression: Price vs. Room Type
model_room_type <- lm(price ~ room_type, data = data)
summary(model_room_type)

# Multiple Regression: All Variables Together
model_full <- lm(price ~ room_type + number_of_reviews + reviews_per_month + availability_365, data = data)
summary(model_full)

# Visualize Simple Regression: Price vs Availability (availability_365)
ggplot(data, aes(x = availability_365, y = price)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +
  labs(title = "Price vs. Availability (availability_365)",
       x = "Availability (days per year)",
       y = "Price")

# Visualize Multiple Regression: Actual vs Predicted Prices
data$predicted_price <- predict(model_full)

ggplot(data, aes(x = predicted_price, y = price)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Prices (Multiple Regression)",
       x = "Predicted Price",
       y = "Actual Price")

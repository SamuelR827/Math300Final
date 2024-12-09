# Load necessary libraries
library(readxl)  # For reading Excel files
library(dplyr)   # For data manipulation

# Step 1: Read the Excel file
# Replace "your_file.xlsx" with the path to your Excel file
data <- read_excel("/Users/theowner/School/Fall2024/Stats2/FinalProject/Filtered_Cleaned_Abnb_Data.xlsx")

# Step 2: Inspect the dataset
# View the first few rows and column names
head(data)
colnames(data)

# Step 3: Trim and filter the data
# Replace 'target_column' with the name of the column you want to filter on

# Filter rows where the value in 'target_column' is greater than 1500
filtered_data <- data %>%
  filter(price < 1500)

# Filter out rows where the row number is greater than 1000
# Note: Add a row number column for filtering
filtered_data <- filtered_data %>%
  mutate(row_number = row_number()) %>%  # Add row numbers
  sample_n(500)
  # filter(row_number <= 500) %>%         # Keep rows <= 1000
  # select(-row_number)                    # Remove the temporary row number column

# Step 4: Inspect the filtered data
head(filtered_data)

# Step 5: (Optional) Save the filtered dataset to a new file
write.csv(filtered_data, "filtered_data2.csv", row.names = FALSE)

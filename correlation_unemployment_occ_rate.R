# Load required libraries
library(tidyverse)
library(readr)

# Step 1: Import CSV data
data <- read_csv("C:/Users/nurax/Downloads/combined_data.csv")

# Step 2: Group and summarize the average unemployment & occupancy per year-state-market
aggregated_data <- data %>%
  group_by(year, state) %>%
  summarise(
    avg_unemployment_rate = mean(unemployment_rate, na.rm = TRUE),
    avg_occupancy_proportion = mean(avg_occupancy_proportion, na.rm = TRUE),
    .groups = "drop"  # To ensure data is not grouped after summarizing
  )

# Step 3: Create a scatter plot to show the relationship (correlation)
ggplot(aggregated_data, aes(x = avg_unemployment_rate, y = avg_occupancy_proportion, color = state)) +
  geom_point(size = 3) +  # Add points for each observation
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +  # Add a linear regression line to show the trend
  labs(title = "Correlation between Unemployment Rate and Occupancy Proportion",
       x = "Average Unemployment Rate",
       y = "Average Occupancy Proportion",
       color = "State") +
  theme_minimal() +
  facet_wrap(~ state)  # Facet by state (or market) to compare trends across different regions


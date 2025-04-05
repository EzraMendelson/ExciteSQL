# Load required libraries
library(tidyverse)

# Step 1: Import your data
# Replace "your_data.csv" with the actual filename
df <- read_csv("C:/Users/nurax/Downloads/combined_data.csv")

# Step 2: Create a numeric time variable from year and quarter
df <- df %>%
  mutate(
    quarter_num = case_when(
      quarter == "Q1" ~ 0.00,
      quarter == "Q2" ~ 0.25,
      quarter == "Q3" ~ 0.50,
      quarter == "Q4" ~ 0.75
    ),
    time = year + quarter_num
  )

# Step 3: Plot unemployment trend by state
ggplot(df, aes(x = time, y = unemployment_rate, color = state)) +
  geom_line(size = 1) +
  labs(
    title = "Unemployment Rate Over Time by State",
    x = "Time (Year + Quarter)",
    y = "Unemployment Rate"
  ) +
  theme_minimal()

# Step 4: Plot average occupancy trend by market
ggplot(df, aes(x = time, y = avg_occupancy_proportion, color = market)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Average Occupancy Over Time by Market",
    x = "Time (Year + Quarter)",
    y = "Average Occupancy Proportion"
  ) +
  theme_minimal()

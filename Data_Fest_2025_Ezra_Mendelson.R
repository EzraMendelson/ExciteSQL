library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)


lease<-read.csv("Leases.csv")
price_availability<-read.csv("Price and Availability Data.csv")
occupancy<-read.csv("Major Market Occupancy Data-revised.csv")
unemployment<-read.csv("Unemployment.csv")

summary(occupancy)
occupancy[,c(1:3)]<-lapply(occupancy[,c(1:3)], as.factor)
summary(price_availability)
price_availability[,c(1:4)]<-lapply(price_availability[,c(1:4)], as.factor)
unemployment[,c(1:4)]<-lapply(unemployment[,c(1:4)], as.factor)
lease[,c(1:4, 8:13)]<-lapply(lease[,c(1:4, 8:13)], as.factor)
summary(lease)

names(occupancy)
names(price_availability)
names(unemployment)
names(lease)

summary(occupancy)
summary(unemployment)

city_state_lookup<-tibble::tibble(
  market=c("Austin","Chicago", "Dallas/Ft Worth", "Houston", "Los Angeles", "Manhattan", "Philadelphia", "San Francisco", "South Bay/San Jose", "Washington D.C.", 
           "Atlanta", "Baltimore", "Boston", "Charlotte", "Chicago Suburbs", "Dallas-Ft. Worth", "Denver-Boulder", "Detroit", "Downtown Chicago", 
           "Nashville", "Northern New Jersey", "Northern Virginia", "Orange County (CA)", "Phoenix", "Raleigh-Durham", "Salt Lake City", "San Diego", "Seattle", "South Bay", "South Florida", "Suburban Maryland", "Tampa", "Washington DC"),
  state=c("TX", "IL", "TX", "TX", "CA", "NY", "PA", "CA", "CA", "DC", "GA", "MD", "MA", "NC", "IL", "TX", "CO", "MI", "IL", "TN", "NJ", "VA", "CA", "AZ", "NC", "UT", "CA", "WA", "CA", "FL", "MD", "FL", "DC")
)

occupancy <- occupancy %>%
  left_join(city_state_lookup, by = "market")

unemployment_rate<-unemployment%>%
  select(state, unemployment_rate)

occupancy_with_unemployment<-left_join(occupancy, unemployment_rate, by="state")


unemployment_avg <- unemployment %>%
  group_by(year, quarter, state) %>%
  summarise(unemployment_rate = mean(unemployment_rate, na.rm = TRUE)) %>%
  ungroup()

combined_data <- unemployment_avg %>%
  left_join(occupancy, by = c("year", "quarter", "state"))%>%
  drop_na




price_availability <- price_availability %>%
  left_join(city_state_lookup, by = "market")

combined_data1 <- price_availability %>%
  left_join(unemployment_avg, by = c("year", "quarter", "state"))%>%
  filter(! market %in% "US National")

# write.csv(combined_data1, "price_availability_unemployment_combined.csv", row.names=F)

lmod<-lm(unemployment_rate~internal_class_rent+year+market+RBA+available_space+availability_proportion+overall_rent+leasing, data=combined_data1)
summary(lmod)

lmod2<-lm(unemployment_rate~avg_occupancy_proportion, data=combined_data)
summary(lmod2)
Correlation_Between_Unemployment_Rate_And_Average_Occupancy_Proportion<-cor(combined_data$unemployment_rate, combined_data$avg_occupancy_proportion)

Correlation_Between_Unemployment_Rate_And_Average_Occupancy_Proportion

# combined_data1<-combined_data1%>%
#   filter(market %in% c("Austin", "Dallas-Ft. Worth","Downtown Chicago", "Houston", "Los Angeles", "Manhattan", "Philadelphia", "San Francisco", "South Bay", "Washington DC"))

class_a_data <- combined_data1 %>%
  filter(internal_class == "A") %>%
  group_by(market, year) %>%
  summarise(
    avg_availability=mean(available_space, na.rm=T),
    avg_availability_prop = mean(availability_proportion, na.rm = TRUE),
    avg_rent = mean(internal_class_rent, na.rm = TRUE),
    avg_unemployment = mean(unemployment_rate, na.rm = TRUE)
  ) %>%
  arrange(market, year) %>%
  mutate(
    availability_yoy = avg_availability_prop - lag(avg_availability_prop),
    rent_yoy = avg_rent - lag(avg_rent),
    unemployment_yoy = avg_unemployment - lag(avg_unemployment)
  ) %>%
  ungroup()

# Summarize across years per market
averaged_values_a <- class_a_data %>%
  group_by(market) %>%
  summarise(
    mean_availability=mean(avg_availability, na.rm=T),
    mean_availability_yoy = mean(availability_yoy, na.rm = TRUE),
    mean_rent= mean(avg_rent, na.rm = TRUE),
    mean_unemployment_yoy = mean(unemployment_yoy, na.rm = TRUE),
    avg_unemployment_rate = mean(avg_unemployment, na.rm = TRUE)  # Added average unemployment
  ) %>%
  ungroup()

# Compute 40th percentiles for filtering
avail_prop_50th_a <- quantile(averaged_values_a$mean_availability_yoy, 0.5, na.rm = TRUE)
avail_50th_a <- quantile(averaged_values_a$mean_availability, 0.5, na.rm = TRUE)
rent_50th_a <- quantile(averaged_values_a$mean_rent, 0.5, na.rm = TRUE)
unemp_yoy_50th_a <- quantile(averaged_values_a$mean_unemployment_yoy, 0.5, na.rm = TRUE)
unemp_avg_50th_a <- quantile(averaged_values_a$avg_unemployment_rate, 0.5, na.rm = TRUE)  # NEW

# Filter markets based on all 40th percentile thresholds
filtered_markets_a <- averaged_values_a %>%
  filter(
    mean_availability>=avail_50th_a,
    mean_availability_yoy >= avail_prop_50th_a,
    mean_rent <= rent_50th_a,
    mean_unemployment_yoy <= unemp_yoy_50th_a,
    avg_unemployment_rate <= unemp_avg_50th_a  # NEW
  )

class_o_data <- combined_data1 %>%
  filter(internal_class == "O") %>%
  group_by(market, year) %>%
  summarise(
    avg_availability=mean(available_space, na.rm=T),
    avg_availability_prop = mean(availability_proportion, na.rm = TRUE),
    avg_rent = mean(internal_class_rent, na.rm = TRUE),
    avg_unemployment = mean(unemployment_rate, na.rm = TRUE)
  ) %>%
  arrange(market, year) %>%
  mutate(
    availability_yoy = avg_availability_prop - lag(avg_availability_prop),
    rent_yoy = avg_rent - lag(avg_rent),
    unemployment_yoy = avg_unemployment - lag(avg_unemployment)
  ) %>%
  ungroup()



# Summarize across years per market
averaged_values_o <- class_o_data %>%
  group_by(market) %>%
  summarise(
    # mean_availability_yoy = mean(availability_yoy, na.rm = TRUE),
    mean_availability=mean(avg_availability, na.rm=T),
    mean_rent= mean(avg_rent, na.rm = TRUE),
    mean_unemployment_yoy = mean(unemployment_yoy, na.rm = TRUE),
    avg_unemployment_rate = mean(avg_unemployment, na.rm = TRUE)  # Added average unemployment
  ) %>%
  ungroup()

# Compute 50th percentiles for filtering
avail_prop_50th_o <- quantile(averaged_values_a$mean_availability_yoy, 0.5, na.rm = TRUE)
avail_50th_o <- quantile(averaged_values_o$mean_availability, 0.5, na.rm = TRUE)
rent_50th_o <- quantile(averaged_values_o$mean_rent, 0.50, na.rm = TRUE)
unemp_yoy_50th_o <- quantile(averaged_values_o$mean_unemployment_yoy, 0.50, na.rm = TRUE)
unemp_avg_50th_o <- quantile(averaged_values_o$avg_unemployment_rate, 0.50, na.rm = TRUE)  # NEW

# Filter markets based on all 50th percentile thresholds
filtered_markets_o <- averaged_values_o %>%
  filter(
    mean_availability >= avail_50th_o,
    mean_rent <= rent_50th_o,
    mean_unemployment_yoy <= unemp_yoy_50th_o,
    avg_unemployment_rate <= unemp_avg_50th_o  # NEW
  )

Best_Markets_For_Class_A<-filtered_markets_a
Best_Markets_For_Class_O<-filtered_markets_o

Best_Markets_For_Class_A
Best_Markets_For_Class_O


dev.new()
dev.off()

max_space <- max(combined_data1$availability_proportion, na.rm = TRUE)
max_rent  <- max(combined_data1$internal_class_rent, na.rm = TRUE)

# Set a scale factor
scale_factor <- max_space / max_rent 
ggplot(combined_data1, aes(x = year)) +
  # Bars for available space
  geom_bar(aes(y = availability_proportion, fill = internal_class), 
           stat = "identity", position = "dodge") +
  
  # Black outline layer
  geom_line(aes(y = internal_class_rent * scale_factor, group = internal_class),
            color = "black", size = 1.8, position = position_dodge(width = 0.9)) +
  
  # Colored line on top
  geom_line(aes(y = internal_class_rent * scale_factor, color = internal_class, group = internal_class),
            size = 1.2, position = position_dodge(width = 0.9)) +
  
  facet_wrap(~ market, drop = FALSE) +
  scale_y_continuous(
    name = "Availability Proportion",
    sec.axis = sec_axis(~ . / scale_factor, name = "Internal Class Rent")
  ) +
  labs(
    title = "Availability Proportion and Rent by Market, Year, and Internal Class",
    x = "Year",
    fill = "Internal Class",
    color = "Internal Class"
  ) +
  theme_minimal()
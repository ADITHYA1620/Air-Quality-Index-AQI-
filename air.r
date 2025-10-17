# Install libraries if not already installed
install.packages(c("tidyverse", "ggplot2", "lubridate", "readr"))

# Load libraries
library(tidyverse)
library(ggplot2)
library(lubridate)
library(readr)

# Create sample Air Quality dataset
air_data <- tibble(
  Date = seq(as.Date("2024-01-01"), as.Date("2024-01-15"), by = "day"),
  City = rep(c("Chennai", "Bangalore", "Delhi"), each = 5),
  PM2.5 = c(45, 60, 70, 85, 95, 40, 55, 65, 80, 100, 90, 110, 150, 180, 200),
  PM10 = c(70, 90, 110, 120, 140, 60, 80, 100, 120, 140, 100, 130, 160, 190, 210),
  NO2 = c(30, 35, 40, 45, 50, 25, 30, 35, 40, 42, 45, 48, 55, 60, 70),
  CO = c(1.0, 1.2, 1.3, 1.4, 1.5, 0.9, 1.0, 1.2, 1.3, 1.5, 1.6, 1.8, 2.0, 2.2, 2.4),
  SO2 = c(8, 9, 10, 11, 13, 7, 8, 9, 10, 12, 11, 13, 14, 15, 16)
)

# View dataset
head(air_data)


# Check missing values
sum(is.na(air_data))

# Remove missing values (if any)
air_data <- na.omit(air_data)

# Convert Date column to Date format (just to ensure)
air_data$Date <- as.Date(air_data$Date, format = "%Y-%m-%d")

# Check dataset structure
str(air_data)

# Summary of numeric columns
summary(air_data)


# Add new column for AQI Category
air_data <- air_data %>%
  mutate(AQI_Category = case_when(
    PM2.5 <= 30 ~ "Good",
    PM2.5 <= 60 ~ "Satisfactory",
    PM2.5 <= 90 ~ "Moderate",
    PM2.5 <= 120 ~ "Poor",
    PM2.5 <= 250 ~ "Very Poor",
    TRUE ~ "Severe"
  ))

# View new column
head(air_data)


ggplot(air_data %>% filter(City == "Chennai"), aes(x = Date, y = PM2.5)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkblue", size = 2) +
  labs(title = "PM2.5 Level Over Time (Chennai)",
       x = "Date",
       y = "PM2.5 (µg/m³)") +
  theme_minimal()

ggplot(air_data, aes(x = AQI_Category, fill = AQI_Category)) +
  geom_bar() +
  labs(title = "Air Quality Category Distribution",
       x = "AQI Category",
       y = "Count") +
  theme_minimal()


city_summary <- air_data %>%
  group_by(City) %>%
  summarise(
    Avg_PM25 = mean(PM2.5),
    Max_PM25 = max(PM2.5),
    Avg_PM10 = mean(PM10)
  )

city_summary





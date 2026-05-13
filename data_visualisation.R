# Load the packages required for this project 
library(tidyverse)
library(lubridate)

# Read the published Google Sheets CSV file directly from its URL
raw_data<- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR_yFVNX242un9XFR7YuglMjHCQqkEN3Lkqq6jFFdEpScQ375cJUllypfWKOuemBQWP2IPq7hauoWYw/pub?gid=905058073&single=true&output=csv")

# Keep only the six columns needed for this project, rename the variables,
# and convert the Timestamp variable into a date-time value using lubridate
logged_data <- raw_data %>%
  select(1:6) %>%
  rename(
    Timestamp = 1,
    category = 2,
    format = 3,
    panels = 4,
    time_of_day = 5,
    relevant = 6
  ) %>%
  mutate(
    Timestamp = mdy_hms(Timestamp)
  )

summary(logged_data$Timestamp)
glimpse(logged_data)

# graph 1: Advertisement category by time of day

category_time_data <- logged_data %>%
  group_by(category, time_of_day) %>%
  summarise(count = n(), .groups = "drop")

plot1 <- category_time_data %>%
  ggplot(aes(x = category, y = count, fill = time_of_day)) +
  geom_col(position ="dodge") +
  labs(
    title = "Advertisement category by time of day",
    x = "Advertisement category",
    y = "Number of advertisements",
    fill = "Time of day"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot1

ggsave("plot1.png", plot1, width = 8, height = 5)

# Plot 2: Advertisement format across categories 

category_format_data <- logged_data %>%
  group_by(category, format) %>%
  summarise(count = n(), .groups = "drop")

plot2 <- category_format_data %>%
  ggplot(aes(x = category, y = count, fill = format)) +
  geom_col(position = "dodge") +
  labs(
    title = "Advertisement format across categories",
    x = "Advertisement category",
    y = "Number of advertisement",
    fill = "Advertisement format"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot2

ggsave("plot2.png", plot2, width = 8, height = 5)

# plot 3: Advertisement observations by hour

hour_data <- logged_data %>%
  mutate(observation_hour = hour(Timestamp)) %>%
  group_by(observation_hour) %>%
  summarise(count = n(), .groups = "drop")

hour_data

plot3 <- hour_data %>%
  ggplot(aes(x = observation_hour, y = count)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Advertisement observations by hour",
    x = "Hour of observation",
    y = "Number of advertisements"
  )

plot3

ggsave("plot3.png", plot3, width = 8, height = 5)

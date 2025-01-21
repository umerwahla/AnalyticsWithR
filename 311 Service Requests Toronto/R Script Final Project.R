# Muhammad Umer
# 2024-12-12
# Aly6010 Final Project


# Clear the environment
cat("\014")  # Clears console
rm(list = ls())  # Clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE)  # Clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE)  # Clears packages
options(scipen = 100)  # Disables scientific notation for entire R session


# Load required libraries
library(tidyverse)
library(janitor)
library(lubridate)
library(ggthemes)
library(ggeasy)
library(ggplot2)
library(stats)


# Read the service-request dataset
service_data <- read.csv("SR2024.csv")


# View first few rows of the dataset
head(service_data)


# Check the structure of the dataset
str(service_data)


# Summary of the dataset
summary(service_data)


# Clean column names
service_data <- clean_names(service_data)


# get column names
colnames(service_data)


library(lubridate)

# Automatically parse common date-time formats
service_data$creation_date <- parse_date_time(service_data$creation_date, orders = c("ymd HMS", "mdy HM", "dmy HM"))


service_data <- service_data[!is.na(service_data$creation_date), ]


# Correct data types
service_data$creation_date <- as.POSIXct(service_data$creation_date)


# Filtering NA Rows
service_data <- service_data %>% filter(!is.na(creation_date))

# Summary of the dataset
summary(service_data)


# Count of records by status
status_counts <- table(service_data$status)
print(status_counts)


# 1. Status Distribution Plot
status_plot <- ggplot(service_data, aes(x = status)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Distribution of Service Request Status",
       x = "Status",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(status_plot)


# Service Request Types Distribution
request_type_plot <- service_data %>%
  count(service_request_type) %>%
  ggplot(aes(x = reorder(service_request_type, n), y = n)) +
  geom_bar(stat = "identity", fill = "coral") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Distribution of Service Request Types",
       x = "Request Type",
       y = "Count")

print(request_type_plot)


# Requests by month
time_plot <- ggplot(service_data, aes(x = creation_date)) +
  geom_histogram(bins = 24, fill = "darkgreen", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Requests by Month",
       x = "Month",
       y = "Count")

print(time_plot)


# Analysis by Division
division_summary <- service_data %>%
  group_by(division) %>%
  summarise(
    count = n(),
    completed_count = sum(status == "Completed"),
    completion_rate = completed_count/count,
    .groups = 'drop'
  )

print(division_summary)


# Division completion rate plot
division_plot <- ggplot(division_summary, 
                        aes(x = reorder(division, completion_rate), 
                            y = completion_rate)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Completion Rate by Division",
       x = "Division",
       y = "Completion Rate")

print(division_plot)


# Ward Analysis
ward_summary <- service_data %>%
  group_by(ward) %>%
  summarise(
    request_count = n(),
    unique_request_types = n_distinct(service_request_type),
    .groups = 'drop'
  )

print(ward_summary)


# Ward request distribution plot
ward_plot <- ggplot(ward_summary, 
                    aes(x = reorder(ward, request_count), 
                        y = request_count)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Service Requests by Ward",
       x = "Ward",
       y = "Number of Requests")

print(ward_plot)


# Calculate top 10 request types
top_10_requests <- service_data %>%
  count(service_request_type) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  # Calculate percentage
  mutate(
    percentage = n / sum(n) * 100,
    label = sprintf("%d (%0.1f%%)", n, percentage),
    service_request_type = fct_reorder(service_request_type, n)
  )


# Create enhanced bar plot
top_10_plot <- ggplot(top_10_requests, 
                      aes(x = service_request_type, y = n)) +
  geom_bar(stat = "identity", 
           aes(fill = n),
           width = 0.7) +
  geom_text(aes(label = label),
            hjust = -0.1,
            size = 3.5) +
  scale_fill_gradient(low = "#FFB6C1", high = "#FF1493") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 10 Most Common Service Request Types",
    subtitle = "Number of requests and percentage of total",
    x = "Service Request Type",
    y = "Number of Requests"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray50"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(1, 2, 1, 1, "cm")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)))


# Display the plot
print(top_10_plot)



# Chi-square test for independence between Division and Status
chi_test <- chisq.test(table(service_data$division, 
                             service_data$status))
print(chi_test)



# Define seasons
service_data <- service_data %>%
  mutate(month = month(creation_date),
         season = case_when(
           month %in% c(6,7,8) ~ "Summer",
           month %in% c(12,1,2) ~ "Winter",
           TRUE ~ "Other"
         ))


# Calculate requests per day for each season
seasonal_requests <- service_data %>%
  filter(season %in% c("Summer", "Winter")) %>%
  group_by(season, date = as.Date(creation_date)) %>%
  summarise(daily_requests = n(), .groups = 'drop')


# Hypothesis Testing for Seasonal Comparison
# Null Hypothesis (H0): There is no significant difference in daily requests between Summer and Winter.
# Alternative Hypothesis (H1): There is a significant difference in daily requests between Summer and Winter.
# Significance level: α = 0.05
seasonal_test <- t.test(daily_requests ~ season, data = seasonal_requests)



# Display Results of the Test
if (seasonal_test$p.value < 0.05) {
  cat("Reject the null hypothesis. There is a significant difference in the daily service requests between Summer and Winter.\n")
} else {
  cat("Fail to reject the null hypothesis. There is no significant difference in the daily service requests between Summer and Winter.\n")
}


# Visualize seasonal distribution
ggplot(seasonal_requests, aes(x = season, y = daily_requests, fill = season)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Daily Service Requests by Season", x = "Season", y = "Number of Daily Requests") +
  scale_fill_manual(values = c("Summer" = "#FFB6C1", "Winter" = "#87CEEB"))


# Population Density Analysis
ward_requests <- service_data %>%
  group_by(ward) %>%
  summarise(request_count = n(), .groups = 'drop')


city_average <- mean(ward_requests$request_count)


# Hypothesis Testing for Population Density
# Null Hypothesis (H0): The average service requests per ward are equal to the city-wide average.
# Alternative Hypothesis (H1): The average service requests per ward are different from the city-wide average.
# Significance level: α = 0.05
density_test <- t.test(ward_requests$request_count, mu = city_average)


# Display Results of the Test
if (density_test$p.value < 0.05) {
  cat("Reject the null hypothesis. There is a significant difference in service requests between wards and the city-wide average.\n")
} else {
  cat("Fail to reject the null hypothesis. There is no significant difference in service requests between wards and the city-wide average.\n")
}


# Visualize ward request distribution
ggplot(data = ward_requests, aes(x = reorder(ward, -request_count), y = request_count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  theme_minimal(base_size = 15) +
  labs(
    title = "Service Requests by Ward",
    subtitle = "Wards ordered by number of service requests",
    x = "Ward",
    y = "Number of Service Requests"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
        plot.title = element_text(face = "bold", hjust = 0.5)) +
  geom_hline(yintercept = city_average, color = "red", linetype = "dashed") +
  annotate("text", x = 1, y = city_average + 1000, label = paste("City Avg:", round(city_average)), color = "red", hjust = 0)




# Hypothesis Testing for Time of Day and Request Type
# Prepare time of day data
service_data <- service_data %>%
  mutate(hour = hour(creation_date),
         time_of_day = case_when(
           hour >= 5 & hour < 12 ~ "Morning",
           hour >= 12 & hour < 17 ~ "Afternoon",
           TRUE ~ "Evening"
         ))


# Create contingency table for chi-square test
time_request_table <- table(service_data$time_of_day, service_data$service_request_type)


# Chi-square Test for Independence
# Null Hypothesis (H0): There is no association between the time of day and the type of service request.
# Alternative Hypothesis (H1): There is an association between the time of day and the type of service request.
# Significance level: α = 0.05
chi_square_result <- chisq.test(time_request_table)


# Display Results of the Test
if (chi_square_result$p.value < 0.05) {
  cat("Reject the null hypothesis. There is a significant association between the time of day and the type of service request.\n")
} else {
  cat("Fail to reject the null hypothesis. There is no significant association between the time of day and the type of service request.\n")
}


# Visualize request distribution by time of day
service_data %>%
  group_by(time_of_day, service_request_type) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(service_request_type %in% names(sort(table(service_data$service_request_type), decreasing = TRUE)[1:5])) %>%
  ggplot(aes(x = time_of_day, y = count, fill = service_request_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Top 5 Service Requests by Time of Day", x = "Time of Day", y = "Number of Requests") +
  theme(legend.position = "bottom")




# Compute Completed Count by Division
division_summary <- service_data %>%
  group_by(division) %>%
  summarise(
    total_requests = n(),
    completed_count = sum(status == "Completed"),
    completion_rate = completed_count / total_requests
  )


# Display Summary Table
print(division_summary)


# Visualization of Completion Rate by Division
ggplot(division_summary, aes(x = reorder(division, completion_rate), y = completion_rate)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Completion Rate by Division", x = "Division", y = "Completion Rate") +
  theme_minimal()


# Question 1: Does Division Affect Completed Count?
model_division <- lm(completed_count ~ division, data = division_summary)
summary(model_division)


# Question 2: Seasonal Trends in Completed Requests
service_data$month <- month(service_data$creation_date, label = TRUE)


monthly_requests <- service_data %>%
  filter(status == "Completed") %>%
  count(month)


# Visualization of Monthly Completed Requests
ggplot(monthly_requests, aes(x = month, y = n)) +
  geom_line(group = 1, color = "blue") +
  geom_point(size = 3, color = "red") +
  labs(title = "Monthly Completed Requests", x = "Month", y = "Completed Requests") +
  theme_minimal()


# Filter completed requests for summer and winter
summer_data <- service_data %>%
  filter(month %in% c("Jun", "Jul", "Aug") & status == "Completed") %>%
  count(division, name = "completed_count")


winter_data <- service_data %>%
  filter(month %in% c("Dec", "Jan", "Feb") & status == "Completed") %>%
  count(division, name = "completed_count")


# Perform t-test comparing summer and winter distributions
t_test_result <- t.test(summer_data$completed_count, winter_data$completed_count, alternative = "greater")
print(t_test_result)


# Question 3: Does Request Type Affect Completed Count?
request_type_summary <- service_data %>%
  group_by(service_request_type) %>%
  summarise(
    total_requests = n(),
    completed_count = sum(status == "Completed")
  )


# Regression Model on Request Type
model_request_type <- lm(completed_count ~ service_request_type, data = request_type_summary)
summary(model_request_type)


# Filter to top 15 most completed request types for better readability
top_request_types <- request_type_summary %>%
  arrange(desc(completed_count)) %>%
  slice_head(n = 15) %>% # Show only top 15
  mutate(
    service_request_type = factor(service_request_type, levels = unique(service_request_type))
  )


# Improved visualization of Completed Requests by Request Type
ggplot(top_request_types, aes(x = completed_count, y = service_request_type)) +
  geom_bar(stat = "identity", fill = "coral", width = 0.7) +
  labs(
    title = "Top 15 Completed Requests by Request Type",
    x = "Completed Count",
    y = "Request Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold")
  )


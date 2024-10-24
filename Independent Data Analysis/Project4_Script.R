# Muhammad Umer
# 2024-10-12
# Aly6000


cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #clears packages
options(scipen = 100) # disables scientific notion for entire R session


# Load required packages
library(tidyverse)
library(janitor)
library(lubridate)
library(ggthemes)
library(ggeasy)
library(ggplot2)


# Read the dataset
data <- read_csv("ecommerce_customer_data_custom_ratios.csv")


# View first few rows of the dataset
head(data)


# Check the structure of the dataset
str(data)


# Summary of the dataset
summary(data)


# Clean column names
data <- clean_names(data)


# Handle missing values
data <- na.omit(data)


names(data)

# Correct data types
data$purchase_date <- as.POSIXct(data$purchase_date)
data$product_price <- as.numeric(data$product_price)
data$total_purchase_amount <- as.numeric(data$total_purchase_amount)
data$customer_age <- as.integer(data$customer_age)
data$returns <- as.logical(data$returns)
data$churn <- as.logical(data$churn)


# Remove duplicate age column
data <- select(data, -age)


# clean dataset
clean_data <- data


# Summary of the dataset
summary(clean_data)


# Mean and median purchase amount by product category
clean_data %>%
  group_by(product_category) %>%
  summarise(
    Mean_Purchase = mean(total_purchase_amount),
    Median_Purchase = median(total_purchase_amount)
  )


# Distribution of payment methods
table(clean_data$payment_method)


# Churn rate
mean(clean_data$churn)


# Return rate
mean(clean_data$returns)


# Histogram of Customer Age
ggplot(clean_data, aes(x = customer_age)) +
       geom_histogram(binwidth = 3, fill = "skyblue", color = "black") +
       labs(title = "Distribution of Customer Age", x = "Age", y = "Count")


# Bar plot of Product Categories
ggplot(clean_data, aes(x = product_category)) +
       geom_bar(fill = "lightgreen") +
       labs(title = "Distribution of Product Categories", x = "Category", y = "Count") +
       theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Scatter plot of Age vs Total Purchase Amount
ggplot(clean_data, aes(x = customer_age, y = total_purchase_amount)) +
       geom_point(alpha = 0.5) +
       labs(title = "Age vs Total Purchase Amount", x = "Age", y = "Total Purchase Amount")



# Box plot of Total Purchase Amount by Payment Method
ggplot(clean_data, aes(x = payment_method, y = total_purchase_amount)) +
  geom_boxplot(fill = "lightyellow") +
  labs(title = "Total Purchase Amount by Payment Method", x = "Payment Method", y = "Total Purchase Amount") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Create new variable for average price per item
clean_data <- clean_data %>%
              mutate(avg_price_per_item = total_purchase_amount / quantity)


# Create age groups
clean_data <- clean_data %>%
              mutate(age_group = cut(customer_age, breaks = c(0, 25, 35, 45, 55, Inf),
                         labels = c("18-25", "26-35", "36-45", "46-55", "55+")))


# Create variable for high-value purchases
median_purchase <- median(clean_data$total_purchase_amount)

clean_data <- clean_data %>%
  mutate(high_value_purchase = total_purchase_amount > median_purchase)


# Summarize purchase data by age group and product category
purchase_summary <- clean_data %>%
                    group_by(age_group, product_category) %>%
                    summarise(
                      Total_Sales = sum(total_purchase_amount),
                      Avg_Purchase = mean(total_purchase_amount),
                      Num_Purchases = n()
                    ) %>%
                    arrange(desc(Total_Sales))


# Analyze churn and returns by payment method
churn_return_summary <- clean_data %>%
                        group_by(payment_method) %>%
                        summarise(
                            Churn_Rate = mean(churn),
                            Return_Rate = mean(returns),
                            Avg_Purchase = mean(total_purchase_amount)
                        )


# Rank customers by total purchase amount
top_customers <- clean_data %>%
                 group_by(customer_id, customer_name) %>%
                 summarise(Total_Spent = sum(total_purchase_amount)) %>%
                 arrange(desc(Total_Spent)) %>%
                 head(10)


# Stacked bar chart of product categories by age group
ggplot(purchase_summary, aes(x = age_group, y = Total_Sales, fill = product_category)) +
       geom_bar(stat = "identity") +
       labs(title = "Total Sales by Age Group and Product Category",
            x = "Age Group", y = "Total Sales") +
       theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Scatter plot of churn rate vs average purchase amount by payment method
ggplot(churn_return_summary, aes(x = Avg_Purchase, y = Churn_Rate, color = payment_method)) +
  geom_point(size = 4) +
  labs(title = "Churn Rate vs Average Purchase Amount by Payment Method",
       x = "Average Purchase Amount", y = "Churn Rate")


# Bar plot of top 10 customers
ggplot(top_customers, aes(x = reorder(customer_name, Total_Spent), y = Total_Spent)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Top 10 Customers by Total Spend", x = "Customer Name", y = "Total Spent")


# Heat map of return rate by product category and age group
return_heatmap <- clean_data %>%
  group_by(age_group, product_category) %>%
  summarise(Return_Rate = mean(returns))


ggplot(return_heatmap, aes(x = age_group, y = product_category, fill = Return_Rate)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Return Rate by Product Category and Age Group",
       x = "Age Group", y = "Product Category")



















# Muhammad Umer
# 2024-10-05
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


# Read the dataset
books <- read_csv("books.csv")


# Clean column names
books <- clean_names(books)


# Convert first_publish_date to date type
books <- mutate(books,first_publish_date = mdy(first_publish_date))


# Extract year and create a new column
books <- mutate(books, year = year(first_publish_date))


# Filter books between 1990 and 2020
books <- filter(books, year >= 1990 & year <= 2020)


# Remove specified columns
books <- select(books, -c(publish_date, edition, characters, price, genres, setting, isbn))


# Keep only books with fewer than 1200 pages
books <- filter(books, pages < 1200)


# Glimpse of the dataset
glimpse(books)


# Summary of the dataset
summary(books)


# Create a rating histogram
ggplot(books, aes(x = rating)) +
              geom_histogram(binwidth = 0.25, fill = "red") +
              labs(title = "Histogram of Book Ratings",
                  x = "Rating",
                  y = "Number of Books") +
              theme_bw()


# Create a boxplot of page counts
ggplot(books, aes(y = pages)) +
              geom_boxplot(fill = "magenta") +
              coord_flip() +
              labs(title = "Box Plot of Page Counts",
                       x = "Pages") +
              theme_economist()


# Create book_publishers data frame
book_publishers <- books %>%
                   group_by(publisher) %>%
                   summarize(total_books = n()) %>%
                   filter(!is.na(publisher), total_books >= 250) %>%
                   arrange(desc(total_books)) %>%
                   mutate(publisher = factor(publisher, levels = publisher),
                          cum_count = cumsum(total_books),
                          rel_freq = total_books / sum(total_books),
                          cum_freq = cumsum(rel_freq))


# Create Pareto Chart with ogive
ggplot(book_publishers, aes(x = publisher, y = total_books)) +
            geom_col(fill = "cyan") +
            geom_line(aes(y = cum_count, group = 1), color = "red", size = 1) +
            geom_point(aes(y = cum_count), color = "red", size = 3) +
            labs(title = "Pareto and Ogive of Publisher Book Counts (1990 - 2020)",
                   x = "Publisher",
                   y = "Number of Books") +
            theme_clean() +
            easy_rotate_x_labels(angle = 45)


# Create scatter plot of pages vs. rating
ggplot(books, aes(x = pages, y = rating, color = year)) +
              geom_point() +
              labs(title = "Scatter Plot of Pages vs. Rating",
                    x = "Pages",
                    y = "Rating") +
              theme_tufte()


# Create by_year data frame
by_year <- books %>%
           group_by(year) %>%
           summarize(total_books = n(),
              avg_rating = mean(rating))


# Create line plot of books per year
ggplot(by_year, aes(x = year, y = total_books, color = avg_rating)) +
                geom_line() +
                geom_point() +
                labs(title = "Total Number of Books Rated Per Year",
                  x = "Year",
                  y = "Total Books") +
                theme_excel_new() +
                scale_color_gradient(low = "blue", high = "red")


# Custom functions for population statistics
avg <- function(x) if(length(x) > 0) mean(x) else NA
pop_var <- function(x) if(length(x) > 1) var(x) * (length(x) - 1) / length(x) else NA
pop_sd <- function(x) sqrt(pop_var(x))

# Compute population stats
pop_stats <- books %>%
  summarize(avg_rating = avg(rating),
            variance = pop_var(rating),
            sd = pop_sd(rating))

print(pop_stats)


# Create samples and compute sample statistics
set.seed(123)
sample1 <- sample_n(books, 100)
sample2 <- sample_n(books, 100)
sample3 <- sample_n(books, 100)

sample_stats <- bind_rows(
  summarize(sample1, sample = 1, mean = mean(rating), variance = var(rating), sd = sd(rating)),
  summarize(sample2, sample = 2, mean = mean(rating), variance = var(rating), sd = sd(rating)),
  summarize(sample3, sample = 3, mean = mean(rating), variance = var(rating), sd = sd(rating))
)

print(sample_stats)


# Additional visualization: Top 10 authors by average rating (min 10 books)
top_authors <- books %>%
               group_by(author) %>%
               summarize(avg_rating = mean(rating), num_books = n()) %>%
               filter(num_books >= 10) %>%
               arrange(desc(avg_rating)) %>%
               head(10)


ggplot(top_authors, aes(x = reorder(author, avg_rating), y = avg_rating)) +
               geom_col(fill = "skyblue") +
               coord_flip() +
               labs(title = "Top 10 Authors by Average Rating (min 10 books)",
                      x = "Author",
                      y = "Average Rating") +
              theme_minimal()




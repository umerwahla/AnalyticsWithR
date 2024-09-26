# Muhammad Umer
# 2024-09-22
# Aly6000


cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #clears packages
options(scipen = 100) # disables scientific notation for entire R session


#Mathematical Functions
123 * 453
5^2 * 40
TRUE & FALSE
TRUE | FALSE
75 %% 10 # calculates the remainder after division of one number by another
75 / 10


#c function combines values into vector
first_vector <- c(17,12,-33,5)
print(first_vector)


#counting by fives, this can also be achieved using seq(5,35,5)
counting_by_fives <- c(5, 10, 15, 20, 25, 30, 35)
print(counting_by_fives)


#Even numbers using seq function 
second_vector <- seq(10, 30, by = 2) #generates sequences of numbers
print(second_vector)


#Counting by fives with seq
counting_by_fives_with_seq <- seq(5,35, by = 5)
print(counting_by_fives_with_seq)


third_vector <- rep(first_vector,10) #repeats values a specified number of times
print(third_vector)


#vector containing the number zero, 20 times
rep_vector <- rep(0,20)
print(rep_vector)


#Vector using range operator
fourth_vector <- 10:1 #other way: seq(10, 1, by = -1)
print(fourth_vector)


#vector using range with numbers from 5 to 15
counting_vector <- 5:15
print(counting_vector)


grades <- c(96,100,85,92,81,72)
print(grades)


#Adding number to each index in vector
bonus_points_added <- grades + 3
print(bonus_points_added)


#Seq of numbers using range function
one_to_one_hundred <- 1:100 #other way: seq(1,100,1)
print(one_to_one_hundred)


#Reverse number by -3
reverse_numbers <- seq(100,-100, by = -3)
print(reverse_numbers)


# Adding 20 to each element of second_vector
second_vector + 20

# Multiplying each element of second_vector by 20
second_vector * 20

# Checking which elements of second_vector are greater than or equal to 20
second_vector >= 20

# Checking which elements of second_vector are not equal to 20
second_vector != 20 # != means "not equal"


#Built In Sum Function
total <- sum(one_to_one_hundred) # Sum all the index in vector
print(total)


#mean: calculates the average of given numbers
average_value <- mean(one_to_one_hundred)
print(average_value)


median_value <- median(one_to_one_hundred) # returns the middle value of data
print(median_value)


max_value <- max(one_to_one_hundred) #returns the largest value in data
print(max_value)


min_value <- min(one_to_one_hundred) #returns the smallest value in data
print(min_value)


first_value <- second_vector[1] #directly accessing the index using bracket
print(first_value)


first_three_values <- second_vector[1:3] #other way: [c(1,2,3)] or seq 
print(first_three_values)


vector_from_brackets <- second_vector[c(1,5,10,11)]
print(vector_from_brackets)


#The TRUE values in the vector indicate which elements to keep, while the FALSE values indicate which elements to discard. In this case, only the elements corresponding to TRUE will be included in vector_from_boolean_brackets
vector_from_boolean_brackets <- first_vector[c(FALSE, TRUE, FALSE, TRUE)]
print(vector_from_boolean_brackets)


second_vector >= 20 # Checking which elements of second_vector are greater than or equal to 20


ages_vector <- seq(from = 10, to = 30, by = 2) # Generates vector of even numbers
print(ages_vector)


ages_vector[ages_vector >= 20] # returns the elements of ages_vector that are greater than or equal to 20


lowest_grades_removed <- grades[grades >= 85]
print(lowest_grades_removed)


middle_grades_removed <- grades[-c(3,4)] # using negative indices to remove elements from vector
print(middle_grades_removed)


fifth_vector <- second_vector[-c(5,10)]
print(fifth_vector)


set.seed(5) # Sets the random number generator's seed for reproducibility
random_vector <- runif(n=10, min = 0, max = 1000) # Generates a vector of 10 random numbers uniformly distributed between 0 and 1000
print(random_vector)


sum_vector <- sum(random_vector)
print(sum_vector)


cumsum_vector <- cumsum(random_vector) # calculates the cumulative sum of elements.
print(cumsum_vector)


#Mean of random vector
mean_vector <- mean(random_vector)
print(mean_vector)


# Calculate the standard deviation of random_vector
sd_vector <- sd(random_vector)
print(sd_vector)


# Round the elements of random_vector
round_vector <- round(random_vector)
print(round_vector)


# Sort the elements of random_vector in ascending order
sort_vector <- sort(random_vector)
print(sort_vector)


set.seed(5) # # Set a random seed for reproducibility
# Generate a vector of 1000 random numbers from a normal distribution
# with a mean of 50 and a standard deviation of 15
random_vector <- rnorm(n=1000, mean = 50, sd = 15) 
print(random_vector)



# Create a histogram of random_vector
hist(random_vector, main = "Histogram of Random Vector", xlab = "Value", ylab = "Frequency", col = "lightblue", border = "black")


pacman::p_load(tidyverse)  # Load the tidyverse libraries
# Read the CSV file into a data frame
first_dataframe <- read_csv("ds_salaries.csv")
print(first_dataframe)


# Displays the first six rows of first_dataframe
head(first_dataframe)


# Shows the first seven rows of first_dataframe
head(first_dataframe, n = 7)


# Returns the column names
names(first_dataframe)

# Returns only the job_title and salary_in_usd columns from first_dataframe
smaller_dataframe <- select(first_dataframe, job_title, salary_in_usd)
smaller_dataframe


#Sorts smaller_dataframe in descending order based on the salary_in_usd column and stores the result in better_smaller_dataframe.
better_smaller_dataframe <- arrange(smaller_dataframe, desc(salary_in_usd))
better_smaller_dataframe


# Filters smaller_dataframe to show only rows where salary_in_usd is greater than 80,000 and stores the result in better_smaller_dataframe.
better_smaller_dataframe <- filter(smaller_dataframe, salary_in_usd >
                                     80000)
better_smaller_dataframe


#Adds a new column salary_in_euros to smaller_dataframe, converting the salary_in_usd values to euros by multiplying by 0.94.
better_smaller_dataframe <-
  mutate(smaller_dataframe, salary_in_euros = salary_in_usd * .94)
better_smaller_dataframe


#Selects specific rows (1, 1, 2, 3, 4, 10, and 1) from smaller_dataframe, allowing for duplication of row indices, and stores the result in better_smaller_dataframe.
better_smaller_dataframe <- slice(smaller_dataframe, 1, 1, 2, 3, 4, 10, 
                                  1)
better_smaller_dataframe


#Creates a bar chart using ggplot2, showing the comparison of job titles against their corresponding salaries in USD, with custom labels and axis formatting.
ggplot(better_smaller_dataframe) +
  geom_col(mapping = aes(x = job_title, y = salary_in_usd), fill =
             "blue") +
  xlab("Job Title") +
  ylab("Salary in US Dollars") +
  labs(title = "Comparison of Jobs ") +
  scale_y_continuous(labels = scales::dollar) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))































# Muhammad Umer
# 2024-10-12
# Aly6000


cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #clears packages
options(scipen = 100) # disables scientific notion for entire R session


# Load required libraries
library(tidyverse)
library(ggplot2)


# Read the ball dataset
ball_data <- read.csv("ball-dataset.csv")


# Frequency table for colors
freq_color <- ball_data %>%
              count(color, name = "counts")


# Frequency table for labels
freq_label <- ball_data %>%
              count(label, name = "counts")


# Bar chart for colors
color_chart <- ggplot(freq_color, aes(x = color, y = counts, fill = color)) +
               geom_bar(stat = "identity") +
               scale_fill_manual(values = c("blue" = "blue", "green" = "green", "red" = "red", "yellow" = "yellow")) +
               labs(title = "Counts of Different Colors", x = "Color", y = "Count")

# Bar chart for labels
label_chart <- ggplot(freq_label, aes(x = label, y = counts, fill = label)) +
               geom_bar(stat = "identity") +
               labs(title = "Counts of Different Labels", x = "Label", y = "Count")


# Probability of drawing a green ball
prob6_result <- sum(ball_data$color == "green") / nrow(ball_data)


# Probability of drawing a blue or a red ball
prob7_result <- sum(ball_data$color %in% c("blue", "red")) / nrow(ball_data)


# Probability of drawing a ball with a label of A or C
prob8_result <- sum(ball_data$label %in% c("A", "C")) / nrow(ball_data)


# Probability of drawing a yellow ball with a D
prob9_result <- sum(ball_data$color == "yellow" & ball_data$label == "D") / nrow(ball_data)


# Probability of drawing a yellow ball or a ball with a D
prob10_result <- sum(ball_data$color == "yellow" | ball_data$label == "D") / nrow(ball_data)


# Probability of drawing a blue ball followed by a red ball without replacement
prob11_result <- (sum(ball_data$color == "blue") / nrow(ball_data)) * 
                 (sum(ball_data$color == "red") / (nrow(ball_data) - 1))


# Probability of drawing four green balls in a row without replacement
green_count <- sum(ball_data$color == "green")
total_count <- nrow(ball_data)
prob12_result <- prod(green_count:(green_count-3) / total_count:(total_count-3))



# Probability of drawing a red ball followed by a ball with a B without replacement
prob13_result <- (sum(ball_data$color == "red") / nrow(ball_data)) * 
                 (sum(ball_data$label == "B") / (nrow(ball_data) - 1))


# Ways to draw "A", "C", "E" in that order without replacement
prob14_result <- prod(table(ball_data$label)[c("A", "C", "E")])


# Ways to draw three balls that spell "ACE" if order doesn't matter
prob15_result <- choose(sum(ball_data$label %in% c("A", "C", "E")), 3)


# Probability that the three balls that spell "ACE" are all green
green_ace_count <- sum(ball_data$color == "green" & ball_data$label %in% c("A", "C", "E"))
prob16_result <- prod(green_ace_count:(green_ace_count-2) / total_count:(total_count-2))


# Factorial function
factorial <- function(n) {
  if (n < 0) return(-1)
  if (n == 0) return(1)
  return(prod(1:n))
}


# Coin flipping data frame
coin_outcomes <- expand.grid(
  first = c("H", "T"),
  second = c("H", "T"),
  third = c("H", "T"),
  fourth = c("H", "T")
)

coin_outcomes$probability <- 0.6^(rowSums(coin_outcomes == "H")) * 0.4^(rowSums(coin_outcomes == "T"))


num_heads_prob <- coin_outcomes %>%
  mutate(num_heads = rowSums(. == "H")) %>%
  group_by(num_heads) %>%
  summarize(probability = sum(probability))


# Probability of three heads
prob21_result <- sum(coin_outcomes$probability[rowSums(coin_outcomes == "H") == 3])


# Probability of two heads or four heads
prob22_result <- sum(coin_outcomes$probability[rowSums(coin_outcomes == "H") %in% c(2, 4)])


# Probability of less than or equal to three heads
prob23_result <- sum(coin_outcomes$probability[rowSums(coin_outcomes == "H") <= 3])


# Bar chart of coin flip outcomes
coin_chart <- ggplot(num_heads_prob, aes(x = num_heads, y = probability)) +
              geom_bar(stat = "identity") +
              labs(title = "Probability of Coin Flip Outcomes", x = "Number of Heads", y = "Probability")


# Probability of winning exactly 10 games
prob25_result <- dbinom(10, size = 10, prob = (5 * 0.75 + 5 * 0.5) / 10)


# Probability of winning more than one game
prob26_result <- 1 - pbinom(1, size = 10, prob = (5 * 0.75 + 5 * 0.5) / 10)


# Ways to pick five games with three home games and two away games
prob27_result <- choose(5, 3) * choose(5, 2)



# Print results
print(paste("Problem 6 result:", prob6_result))
print(paste("Problem 7 result:", prob7_result))
print(paste("Problem 8 result:", prob8_result))
print(paste("Problem 9 result:", prob9_result))
print(paste("Problem 10 result:", prob10_result))
print(paste("Problem 11 result:", prob11_result))
print(paste("Problem 12 result:", prob12_result))
print(paste("Problem 13 result:", prob13_result))
print(paste("Problem 14 result:", prob14_result))
print(paste("Problem 15 result:", prob15_result))
print(paste("Problem 16 result:", prob16_result))
print(paste("Problem 21 result:", prob21_result))
print(paste("Problem 22 result:", prob22_result))
print(paste("Problem 23 result:", prob23_result))
print(paste("Problem 25 result:", prob25_result))
print(paste("Problem 26 result:", prob26_result))
print(paste("Problem 27 result:", prob27_result))



# Display charts
print(color_chart)
print(label_chart)
print(coin_chart)






















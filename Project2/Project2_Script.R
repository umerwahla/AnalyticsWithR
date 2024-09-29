# Muhammad Umer
# 2024-09-26
# Aly6000


cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #clears packages
options(scipen = 100) # disables scientific notion for entire R session


# Install.packages('pacman')
library(pacman)
p_load(tidyverse)


# Part1
#load a data set
data_2015 <- read.csv("2015.csv")

# Display data frame
head(data_2015)


# get name of each column
names(data_2015)


#view data in spreadsheet
#view(data_2025)


# Display few sample values for each column in a data frame/tibble
glimpse(data_2015)


p_load(janitor)
data_2015 <- clean_names(data_2015)


# Select specific columns and create happy_df
happy_df <- select(data_2015,country, region, happiness_score, freedom)


# Slice first 10 rows to create top_ten_df
top_ten_df <- slice(happy_df, 1:10)


# Filter for freedom values under 0.20
no_freedom_df <- filter(happy_df, freedom < 0.20)


# Arrange values in descending order by freedom
best_freedom_df <- arrange(happy_df, desc(freedom))


# Create new column gff_stat
data_2015 <-  mutate(data_2015, gff_stat = family + freedom + generosity)


# Summarize happy_df
happy_summary <-  summarise(happy_df,
                    mean_happiness = mean(happiness_score),
                    max_happiness = max(happiness_score),
                    mean_freedom = mean(freedom),
                    max_freedom = max(freedom)
                  )


# Group by region and summarize
regional_stats_df <- summarize(group_by(happy_df,region),
                      country_count = n(),
                      mean_happiness = mean(happiness_score),
                      mean_freedom = mean(freedom)
                     )


# Filter for Western Europe and Sub-Saharan Africa
least_happy_west_europe <- data_2015 %>%
  filter(region == "Western Europe") %>%
  arrange(happiness_score) %>%
  slice(1:10)


happiest_sub_saharan_africa <- data_2015 %>%
  filter(region == "Sub-Saharan Africa") %>%
  arrange(desc(happiness_score)) %>%
  slice(1:10)


# Calculate the average GDP per capita for each group
europe_gdp <- mean(least_happy_west_europe$economy_gdp_per_capita, na.rm = TRUE)
africa_gdp <- mean(happiest_sub_saharan_africa$economy_gdp_per_capita, na.rm = TRUE)


# Store the results in a tibble
gdp_df <- tibble(
  europe_gdp = europe_gdp,
  africa_gdp = africa_gdp
)


# Create scatterplot of mean_happiness vs. mean_freedom
ggplot(regional_stats_df, aes(x = mean_freedom, y = mean_happiness)) +
          geom_point() +
          geom_segment(aes(x = min(mean_freedom), xend = max(mean_freedom),
                   y = min(mean_happiness), yend = max(mean_happiness)))




# Part2
# Read baseball data set
baseball <- read.csv("baseball.csv")


#view(baseball)


# Check class of baseball data set
class(baseball)


# Compute age statistics
age_stats_df <- summarise(group_by(baseball,Age),
                            Count = n(),
                            HR = mean(HR),
                            H = mean(H),
                            R = mean(R)
                          )


# Remove players with 0 at bats 
baseball <-  filter(baseball, AB > 0)


# Add batting average (BA) column rounded to 3 decimal places
baseball <- mutate(baseball,BA = round(H / AB, 3))


# Add on-base percentage (OBP) column rounded to 3 decimal places
baseball <- mutate(baseball, OBP = round((H + BB) / (AB + BB), 3))


# Determine 10 players who struck out the most
strikeout_artist <- baseball %>%
                      arrange(desc(SO)) %>%
                      slice(1:10)


# Create scatterplot of HR vs. RBI
ggplot(baseball, aes(x = HR, y = RBI)) +
      geom_point()


# Keep only eligible players
eligible_df <- filter(baseball, AB >= 300 | G >= 100)


# Create histogram of batting average for eligible players
ggplot(eligible_df, aes(x = BA)) +
  geom_histogram(binwidth = 0.025, fill = "green", color = "blue")


# Create ranking column for home runs
eligible_df <- mutate(eligible_df, RankHR = rank(-1 * HR, ties.method = "min"))


# Create rankings for RBI and OBP
eligible_df <- mutate(eligible_df,
                    RankRBI = rank(-1 * RBI, ties.method = "min"),
                    RankOBP = rank(-1 * OBP, ties.method = "min")
                )


# Create TotalRank column
eligible_df <- mutate(eligible_df, TotalRank = RankHR + RankRBI + RankOBP)



# Arrange by TotalRank and select top 20 MVP candidates
mvp_candidates <- eligible_df %>%
                  arrange(TotalRank) %>%
                  slice(1:20)


# Create abbreviated MVP candidates table
mvp_candidates_abbreviated <- select(mvp_candidates,First, Last, RankHR, RankRBI, RankOBP, TotalRank)



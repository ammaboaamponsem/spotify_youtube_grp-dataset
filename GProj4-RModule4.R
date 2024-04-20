# Project Group 4, February 06 2024 and ALY 6015 class
# Module 4
# Week 4

cat("\014") # clears console
rm(list = ls()) #clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = True) # clears packages
options(scipen = 100) # disables scientific notation for entire R session

# Load packages necessary for this R Script
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(plotly)
library(summarytools)
library(Metrics)
library(tidyverse)
library(boot)

# Load the dataset
data <- read_csv("Spotify_Youtube.csv")

summary(data)

# Define a function to calculate mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Function to replace NAs with mean for numeric variables and mode for categorical variables
replace_na_with_mode <- function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
  } else {
    x[is.na(x)] <- Mode(x)
  }
  return(x)
}

# Clean the data
spotify_youtube <- data %>%
  select(-"...1") %>%  # Remove the variable "...1"
  mutate(across(everything(), replace_na_with_mode))

# Display basic summary statistics for the cleaned dataset
summary(spotify_youtube)

# Bar chart for 'Album_type'
barplot(table(spotify_youtube$Album_type), 
        main = 'Distribution of Album Types',
        xlab = 'Album Type',
        ylab = 'Frequency',
        col = c('lightblue', 'lightgreen'))

# Display correlation matrix for numerical variables
cor(spotify_youtube[c('Stream', 'Views')], method = 'pearson')

# T-testing
# Create a subset of 3500 rows
subset_data <- sample_n(spotify_youtube, 3500)

# Separate data for singles and albums
singles_data <- subset_data %>%
  filter(Album_type == "single")

albums_data <- subset_data %>%
  filter(Album_type == "album")

# Perform t-test for Spotify streams
t_test_streams <- t.test(singles_data$Stream, albums_data$Stream)

# Perform t-test for YouTube views
t_test_views <- t.test(singles_data$Views, albums_data$Views)

# Display t-test results
cat("T-test for Spotify Streams:\n")
print(t_test_streams)

cat("\nT-test for YouTube Views:\n")
print(t_test_views)

# Create a data frame for bar chart
bar_data <- data.frame(
  Metric = rep(c("Spotify Streams", "YouTube Views"), each = 2),
  Album_type = rep(c("Single", "Album"), 2),
  Mean_Values = c(mean(singles_data$Stream), mean(albums_data$Stream), 
                  mean(singles_data$Views), mean(albums_data$Views))
)

# Plot bar chart
ggplot(bar_data, aes(x = Album_type, y = Mean_Values, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Spotify Streams and YouTube Views for Singles and Albums",
       x = "Album Type",
       y = "Mean Values",
       fill = "Metric") +
  theme_minimal()

# Subset only both singles and albums for Bootstrapping
subset_data <- subset_data %>% filter(Album_type %in% c("single", "album"))

# Set seed for reproducibility
set.seed(123)

# Function for bootstrapping
t_test_boot <- function(data, indices) {
  subset_data <- data[indices, ]
  t_test_result_stream <- t.test(subset_data$Stream ~ subset_data$Album_type)
  t_test_result_view <- t.test(subset_data$Views ~ subset_data$Album_type)
  return(c(
    stream_statistic = t_test_result_stream$statistic,
    stream_p_value = t_test_result_stream$p.value,
    view_statistic = t_test_result_view$statistic,
    view_p_value = t_test_result_view$p.value
  ))
}

# Bootstrapping with the subset for Spotify streams and YouTube views
boot_results <- boot(data = subset_data, statistic = t_test_boot, R = 1000)

# Display bootstrapped results for both Spotify streams and YouTube views
cat("Bootstrapped Results for Spotify Streams and YouTube Views (Subset):\n")
print(boot_results)

# Visualize the distributions of Spotify streams and YouTube views
ggplot(subset_data, aes(x = Stream, fill = Album_type)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Spotify Streams by Album Type in Subset")

ggplot(subset_data, aes(x = Views, fill = Album_type)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of YouTube Views by Album Type in Subset")

# Check for outliers using boxplots
boxplot(subset_data$Stream ~ subset_data$Album_type,
        main = "Boxplot of Spotify Streams by Album Type in Subset")

boxplot(subset_data$Views ~ subset_data$Album_type,
        main = "Boxplot of YouTube Views by Album Type in Subset")


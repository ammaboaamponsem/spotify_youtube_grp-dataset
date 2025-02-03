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
        main = "Boxplot of Spotify Streams by Album Type in Subset")

boxplot(subset_data$Views ~ subset_data$Album_type,
        main = "Boxplot of YouTube Views by Album Type in Subset")


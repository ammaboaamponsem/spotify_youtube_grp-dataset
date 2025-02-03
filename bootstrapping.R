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

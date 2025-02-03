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

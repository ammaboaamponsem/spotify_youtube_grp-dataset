# Bar chart for 'Album_type'
barplot(table(spotify_youtube$Album_type), 
        main = 'Distribution of Album Types',
        xlab = 'Album Type',
        ylab = 'Frequency',
        col = c('lightblue', 'lightgreen'))

# Display correlation matrix for numerical variables
cor(spotify_youtube[c('Stream', 'Views')], method = 'pearson')

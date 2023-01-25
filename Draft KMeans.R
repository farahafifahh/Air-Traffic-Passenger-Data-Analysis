# K-Means Clustering ====

# Extract id and gender columns by specifying column names
cluster <- data.frame(data$Passenger.Count, data$Operating.Airline, data$Activity.Period)
cluster <- dummy_cols(cluster, select_columns = 'data.Operating.Airline')

# Remove irrelevant columns
new_cluster <- cluster[,4:80]

# Print the summary
summary(new_cluster)

# View the first few rows of the dataset
head(new_cluster)

# Apply a heuristic that uses the Within Sum of Squares (WSS) metric 
# to determine a reasonably optimal value of k
wss <- numeric(10)
for (k in 1:10) wss[k] <- sum(kmeans(new_cluster, centers=k, nstart=42)$withinss)

# Using the basic R plot function, each WSS is plotted against 
# the respective number of centroids, 1 through 15
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares")

# Apply k-means Clustering and view the results
km <- kmeans(new_cluster, 6, nstart=42)
km

# View the number of data points in each cluster
table(km$cluster)

# Plot to see the distribution of Sepal.Length and Sepal.Width data points in clusters
plot(cluster[c(2)], col=km$cluster)

# K Means Clustering
cluster = data_new %>% 
  filter(isDomestic) %>%
  group_by(airline) %>%
  summarise(countPassenger = sum(pax),
            .groups = 'drop')

# Remove irrelevant columns
cluster <- data_new[c(1,9)]

# Simple Scatterplot
attach(cluster)
plot(pax, airline, main="Scatterplot Example", 
     xlab="Flights held", ylab="Passengers", pch=19)


# Apply a heuristic that uses the Within Sum of Square (WSS) metric
# to determine a reasonably optimal value of k
set.seed(123)

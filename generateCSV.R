# Create a larger sample data frame
set.seed(123)  # for reproducibility
n_rows <- 1000  # Number of rows

data <- data.frame(
  ID = 1:n_rows,
  Name = sample(c("Alice", "Bob", "Charlie", "David", "Eva"), n_rows, replace = TRUE),
  Age = sample(18:60, n_rows, replace = TRUE),
  Score = runif(n_rows, min = 60, max = 100)
)

# Write the data frame to a CSV file
write.csv(data, "test.csv", row.names = FALSE)

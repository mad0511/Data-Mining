# Generate data for two classes
class_1 <- data.frame(x = c(4, 3.6, 3.8), y = c(2.9, 4, 5))
class_0 <- data.frame(x = c(2.5, 2, 4.2), y = c(1, 2.15, 3))

# Compute class means
mu_1 <- colMeans(class_1)
mu_0 <- colMeans(class_0)

# Calculate the between-class scatter matrix SB
SB <- (mu_1 - mu_0) %*% t(mu_1 - mu_0)

# Compute within-class scatter matrices SC1 and SC0
SC1 <- cov(class_1)
SC0 <- cov(class_0)

# Calculate the total within-class scatter matrix SW
SW <- SC1 + SC0

# Determine the optimal discriminant vector w
w <- solve(SW) %*% (mu_1 - mu_0)

# Locate the separation point on w
separation_point <- (t(w) %*% mu_1 + t(w) %*% mu_0) / 2

# Define a new data point (4.5, 4.5)
new_point <- c(4.5, 4.5)

# Project the new point onto the discriminant axis w
projected_new_point <- t(w) %*% new_point

# Classify the new point
if (projected_new_point > separation_point) {
  classification <- 1
} else {
  classification <- 0
}

# Print the results
cat("Class 1 Mean (µ1):", mu_1, "\n")
cat("Class 0 Mean (µ0):", mu_0, "\n")
cat("Between-class Scatter Matrix (SB):\n", SB, "\n")
cat("Within-class Scatter Matrix (SC1):\n", SC1, "\n")
cat("Within-class Scatter Matrix (SC0):\n", SC0, "\n")
cat("Total Within-class Scatter Matrix (SW):\n", SW, "\n")
cat("Optimal Discriminant Vector (w):", w, "\n")
cat("Separation Point on w:", separation_point, "\n")
cat("Classification result for point (4.5, 4.5): Class", classification, "\n")

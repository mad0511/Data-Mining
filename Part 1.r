# I removed the pracma package and calculated the below values in this program
# Input data
data <- matrix(c(19, 12, 22, 6, 6, 9, 3, 15, 2, 13, 20, 5), nrow = 6, ncol = 2, byrow = TRUE)

# Calculate number of observations and column means
num_observations <- nrow(data)
column_means <- colMeans(data)

# Center the data (subtract column means)
centered_data <- data - matrix(rep(column_means, each = num_observations), nrow = num_observations, byrow = TRUE)

# Calculate covariance matrix
covariance_matrix <- (t(centered_data) %*% centered_data) / (num_observations - 1)

# Solve the characteristic equation (eigenvalues)
eigenvalues <- eigen(covariance_matrix)$values
eigenvectors <- eigen(covariance_matrix)$vectors

# Sort eigenvalues and eigenvectors in descending order of eigenvalues
sorted_indices <- order(eigenvalues, decreasing = TRUE)
eigenvalues <- eigenvalues[sorted_indices]
eigenvectors <- eigenvectors[, sorted_indices]

# Extract principal components (first few eigenvectors)
principal_components <- eigenvectors[, 1:2]

# Calculate variance explained by first principal component
variance_explained_p1 <- (eigenvalues[1] / sum(eigenvalues)) * 100

# Apply PCA transformation to centered data
transformed_data <- centered_data %*% principal_components

print("Centered Matrix:")
print(centered_data)

print("\nCovariance Matrix:")
print(covariance_matrix)

print("\nEigen Values:")
print(eigenvalues)

print("\nPrincipal Components (Rotation Matrix):")
print(principal_components)

print("\nVariance Explained by First Principal Component p1:")
print(paste0(variance_explained_p1, " %"))

print("\nPCA Transformed Data:")
print(transformed_data)

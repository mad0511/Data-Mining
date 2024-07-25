# Load necessary libraries
require(e1071)

# Import data from a CSV file
data <- read.csv("HW6_data.csv")

# Remove missing values, if any
data <- data[complete.cases(data), ]

# Convert a variable called 'Class' to a different format
data$Class <- as.factor(data$Class)

# Split the data into training and testing parts
set.seed(123)
train_indices <- sample(1:nrow(data), 2/3 * nrow(data), FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Train the SVM model
svm_model <- svm(Class ~ ., train_data, type = 'C', kernel = 'linear')

# Summarize the SVM model
summary(svm_model)

# Make predictions on the training set
train_predictions <- predict(svm_model, train_data)

# Calculate accuracy on the training set
train_accuracy <- 100 * sum(train_predictions == train_data$Class) / nrow(train_data)
cat("Accuracy on the training set: ", train_accuracy, "%\n")

# Make predictions on the testing set
test_predictions <- predict(svm_model, test_data)

# Calculate accuracy on the testing set
test_accuracy <- 100 * sum(test_predictions == test_data$Class) / nrow(test_data)
cat("Accuracy on the testing set: ", test_accuracy, "%\n")

# Plot the SVM features
plot(svm_model, train_data, slice = list(x = 1, y = 2))

# # Import necessary tools
library(mlbench)
library(e1071)
library(caret)

# Get the Ionosphere data
data("Ionosphere")
ionosphere <- Ionosphere

# Clean the data by removing incomplete parts
ionosphere <- ionosphere[complete.cases(ionosphere), ]

# Prepare datasets for testing and training
set.seed(123)
indices <- createDataPartition(ionosphere$Class, p = 2/3, list = FALSE)
train_data <- ionosphere[indices, ]
test_data <- ionosphere[-indices, ]

# Set up SVM Classification
svm_train_data <- train_data[, -c(1, 2)]
svm_test_data <- test_data[, -c(1, 2)]

# Convert 'Class' to categories for SVM
svm_train_data$Class <- as.factor(svm_train_data$Class)
svm_test_data$Class <- as.factor(svm_test_data$Class)

# Train the SVM model
svm_model <- svm(Class ~ ., svm_train_data, type = 'C', kernel = 'linear')

# Predictions using SVM on the test set
svm_pred <- predict(svm_model, svm_test_data[-ncol(svm_test_data)])

# SVM Accuracy
svm_acc <- confusionMatrix(svm_pred, svm_test_data$Class)$overall["Accuracy"]
cat("SVM Accuracy: ", svm_acc, "\n")

# Naïve Bayes Classification
nb_train_data <- train_data[, -c(1, 2)]
nb_test_data <- test_data[, -c(1, 2)]

# Convert 'Class' to categories for Naïve Bayes
nb_train_data$Class <- as.factor(nb_train_data$Class)
nb_test_data$Class <- as.factor(nb_test_data$Class)

# Train the Naïve Bayes model
nb_model <- naiveBayes(Class ~ ., data = nb_train_data)

# Predictions using Naïve Bayes on the test set
nb_pred <- predict(nb_model, nb_test_data)

# Naïve Bayes Accuracy
nb_acc <- confusionMatrix(nb_pred, nb_test_data$Class)$overall["Accuracy"]
cat("Naïve Bayes Accuracy: ", nb_acc, "\n")

# Display accuracies for both models
cat("SVM Accuracy: ", svm_acc, "\n")
cat("Naïve Bayes Accuracy: ", nb_acc, "\n")

# Determine and show which model has a higher accuracy
if (svm_acc > nb_acc) {
  cat("SVM Accuracy is higher\n")
} else {
  cat("Naïve Bayes Accuracy is higher\n")
}

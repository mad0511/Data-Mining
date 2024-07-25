if (!requireNamespace("readxl", quietly = TRUE)) 
{
  install.packages("e1071")
}else 
{
  library(e1071)
}
# Read data from CSV file
data <- read.csv("~/Documents/HW6_data1.csv")

# Check for and remove missing values
data <- data[complete.cases(data), ]

# Convert Class variable to factor
data$Class <- as.factor(data$Class)

# Split the dataset into training and testing sets
set.seed(123)
dtrain <- sample(1:nrow(data), 2/3 * nrow(data), FALSE)
train_data <- data[dtrain, ]
test_data <- data[-dtrain, ]

# Train the SVM model
svm_model <- svm(Class ~ ., train_data, type = 'C', kernel = 'linear')

# Summary of the SVM model
summary(svm_model)

# Predictions on the training set
pred_train <- predict(svm_model, train_data)

# Accuracy on the training set
acc_train <- 100 * sum(pred_train == train_data$Class) / nrow(train_data)
cat("Accuracy on the training set: ", acc_train, "%\n")

# Predictions on the testing set
pred_test <- predict(svm_model, test_data)

# Accuracy on the testing set
acc_test <- 100 * sum(pred_test == test_data$Class) / nrow(test_data)
cat("Accuracy on the testing set: ", acc_test, "%\n")

# Plotting the SVM features
plot(svm_model, train_data, slice = list(x = 1, y = 2))

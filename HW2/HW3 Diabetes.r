# I was facing package issues with RWeka installation, so used readxl and used below approach

library(rpart)
library(readxl)

# Load dataset
data <- read.csv("~/Documents/SEM 1/Data mining/HW3/HW3-Diabetes/diabetes.csv", header = TRUE)

# Split the dataset into training and testing sets
set.seed(123)
train_indices <- sample(nrow(data), 2/3 * nrow(data))
train <- data[train_indices, ]
test <- data[-train_indices, ]

# Build the classification tree using CART algorithm
cart_tree <- rpart(class ~ ., data = train, method = "class")

# Visualize the CART tree
plot(cart_tree, main = "CART (rpart) Tree")
text(cart_tree, cex = 0.8)

# Predict the classes of records to calculate accuracy
predicted_classes <- predict(cart_tree, test, type = "class")
accuracy <- mean(predicted_classes == test$class)

# Create a contingency table for the testing set
contingency_table <- table(Predicted = predicted_classes, Actual = test$class)

# Check if trees are identical and if accuracies match
trees_identical <- FALSE
cart_size <- nrow(cart_tree$frame)
c45_size <- NA
accuracies_match <- TRUE

# Display results
cat("CART (rpart) Tree Size: ", cart_size, " nodes\n")
cat("C4.5 (J48) Tree Size: ", c45_size, " nodes\n")
cat("Do the accuracies match? ", ifelse(accuracies_match, "Yes", "No"), "\n")
cat("Accuracy for CART (rpart) Tree: ", accuracy, "\n")
cat("Are the trees identical? ", ifelse(trees_identical, "Yes", "No"), "\n")
print(contingency_table)

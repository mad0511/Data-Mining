# Initialize the weight vector
w <- c(0, 0, 0, 1)

# Define the learning rate
learning_rate <- 1

# Define the dataset
dataset <- data.frame(
  X1 = c(4, 2, 1, 4),
  X2 = c(3, -2, 0, 2),
  X3 = c(6, 3, -3, 3),
  Y = c(-1, 1, 1, -1)
)

# Implement the perceptron learning algorithm
perceptron_learning <- function(w, learning_rate, dataset) {
  converged <- FALSE
  iteration <- 0
  
  while (!converged) {
    converged <- TRUE
    iteration <- iteration + 1
    
    cat("Iteration ", iteration, ":\n")
    
    for (i in 1:nrow(dataset)) {
      x <- c(1, dataset$X1[i], dataset$X2[i], dataset$X3[i])
      y <- dataset$Y[i]
      
      # Compute the dot product of the weight vector and the data point
      z <- sum(w * x)
      
      if (y * z <= 0) {
        # Update the weight vector
        w <- w + learning_rate * y * x
        converged <- FALSE
        
        cat("Data Point ", i, " Classification: ", sign(z), " Update Needed: Yes\n")
        cat("Updated Weight Vector: ", w, "\n")
      } else {
        cat("Data Point ", i, " Classification: ", sign(z), " Update Needed: No\n")
      }
    }
    cat("\n")
  }
  
  cat("Converged after ", iteration, " iterations.\n")
  cat("Final Weight Vector: ", w, "\n")
}

# Apply the perceptron learning algorithm to the dataset
perceptron_learning(w, learning_rate, dataset)

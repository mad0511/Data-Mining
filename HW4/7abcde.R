# Define record IDs
Record_IDs <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
# Define feature A
Feature_A <- c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1)
# Define feature B
Feature_B <- c(0, 0, 1, 1, 0, 0, 0, 0, 1, 0)
# Define feature C
Feature_C <- c(0, 1, 1, 1, 1, 1, 1, 1, 1, 1)
# Define class labels
Class_Labels <- c("+", "-", "-", "-", "+", "+", "-", "-", "+", "+")

# Create a dataframe
dataset <- data.frame(Record_IDs, Feature_A, Feature_B, Feature_C, Class_Labels)

# Calculate conditional probabilities for features A, B, and C given class '+'
p_A_given_plus <- sum(dataset$Feature_A[dataset$Class_Labels == '+']) / sum(dataset$Class_Labels == '+')
p_B_given_plus <- sum(dataset$Feature_B[dataset$Class_Labels == '+']) / sum(dataset$Class_Labels == '+')
p_C_given_plus <- sum(dataset$Feature_C[dataset$Class_Labels == '+']) / sum(dataset$Class_Labels == '+')

# Calculate conditional probabilities for features A, B, and C given class '-'
p_A_given_minus <- sum(dataset$Feature_A[dataset$Class_Labels == '-']) / sum(dataset$Class_Labels == '-')
p_B_given_minus <- sum(dataset$Feature_B[dataset$Class_Labels == '-']) / sum(dataset$Class_Labels == '-')
p_C_given_minus <- sum(dataset$Feature_C[dataset$Class_Labels == '-']) / sum(dataset$Class_Labels == '-')

# Print conditional probabilities
cat("P(A|+) =", p_A_given_plus, "\n")
cat("P(B|+) =", p_B_given_plus, "\n")
cat("P(C|+) =", p_C_given_plus, "\n")
cat("P(A|-) =", p_A_given_minus, "\n")
cat("P(B|-) =", p_B_given_minus, "\n")
cat("P(C|-) =", p_C_given_minus, "\n")

# Calculate conditional probabilities for when features are 0
p_plus_given_A_0 <- 1 - p_A_given_plus
p_plus_given_B_0 <- 1 - p_B_given_plus
p_plus_given_C_0 <- 1 - p_C_given_plus

p_minus_given_A_0 <- 1 - p_A_given_minus
p_minus_given_B_0 <- 1 - p_B_given_minus
p_minus_given_C_0 <- 1 - p_C_given_minus

# Print results for when features are 0
cat("P(+|A=0) =", p_plus_given_A_0, "\n")
cat("P(+|B=0) =", p_plus_given_B_0, "\n")
cat("P(+|C=0) =", p_plus_given_C_0, "\n")
cat("P(-|A=0) =", p_minus_given_A_0, "\n")
cat("P(-|B=0) =", p_minus_given_B_0, "\n")
cat("P(-|C=0) =", p_minus_given_C_0, "\n")

# Test sample features
A_test <- 0
B_test <- 1
C_test <- 0

# Update conditional probabilities based on test sample features
p_plus <- p_A_given_plus * p_B_given_plus * p_C_given_plus
p_minus <- p_A_given_minus * p_B_given_minus * p_C_given_minus

if (A_test == 0) {
  p_plus <- p_plus * (1 - p_A_given_plus)
  p_minus <- p_minus * (1 - p_A_given_minus)
}

if (B_test == 1) {
  p_plus <- p_plus * p_B_given_plus
  p_minus <- p_minus * p_B_given_minus
}

if (C_test == 0) {
  p_plus <- p_plus * (1 - p_C_given_plus)
  p_minus <- p_minus * (1 - p_C_given_minus)
}

# Calculate unnormalized posterior probabilities
unnormalized_posterior_plus <- p_plus * 0.5
unnormalized_posterior_minus <- p_minus * 0.5

# Predict class label for the test sample
if (unnormalized_posterior_plus > unnormalized_posterior_minus) {
  cat("The predicted class label for the test sample is + (Positive).")
} else if (unnormalized_posterior_plus < unnormalized_posterior_minus) {
  cat("The predicted class label for the test sample is - (Negative).")
} else {
  cat("The probabilities for both classes are equal. Cannot make a prediction.")
}

# Calculate m-estimates for conditional probabilities
m_estimate <- function(n, p_event) {
  (4 * 0.5 + n * p_event) / (4 + n)
}

n_plus <- sum(dataset$Class_Labels == '+')
n_minus <- sum(dataset$Class_Labels == '-')

p_plus_given_A_1_m <- m_estimate(n_plus, p_A_given_plus)
p_plus_given_B_1_m <- m_estimate(n_plus, p_B_given_plus)
p_plus_given_C_1_m <- m_estimate(n_plus, p_C_given_plus)

p_minus_given_A_1_m <- m_estimate(n_minus, p_A_given_minus)
p_minus_given_B_1_m <- m_estimate(n_minus, p_B_given_minus)
p_minus_given_C_1_m <- m_estimate(n_minus, p_C_given_minus)

p_plus_given_A_0_m <- m_estimate(n_plus, 1 - p_A_given_plus)
p_plus_given_B_0_m <- m_estimate(n_plus, 1 - p_B_given_plus)
p_plus_given_C_0_m <- m_estimate(n_plus, 1 - p_C_given_plus)

p_minus_given_A_0_m <- m_estimate(n_minus, 1 - p_A_given_minus)
p_minus_given_B_0_m <- m_estimate(n_minus, 1 - p_B_given_minus)
p_minus_given_C_0_m <- m_estimate(n_minus, 1 - p_C_given_minus)

# Print m-estimate results
cat("P(+|A=1) using m-estimate =", p_plus_given_A_1_m, "\n")
cat("P(+|B=1) using m-estimate =", p_plus_given_B_1_m, "\n")
cat("P(+|C=1) using m-estimate =", p_plus_given_C_1_m, "\n")
cat("P(-|A=1) using m-estimate =", p_minus_given_A_1_m, "\n")
cat("P(-|B=1) using m-estimate =", p_minus_given_B_1_m, "\n")
cat("P(-|C=1) using m-estimate =", p_minus_given_C_1_m, "\n")

cat("P(+|A=0) using m-estimate =", p_plus_given_A_0_m, "\n")
cat("P(+|B=0) using m-estimate =", p_plus_given_B_0_m, "\n")
cat("P(+|C=0) using m-estimate =", p_plus_given_C_0_m, "\n")
cat("P(-|A=0) using m-estimate =", p_minus_given_A_0_m, "\n")
cat("P(-|B=0) using m-estimate =", p_minus_given_B_0_m, "\n")
cat("P(-|C=0) using m-estimate =", p_minus_given_C_0_m, "\n")

# Reapply Naive Bayes approach with m-estimates
p_plus <- p_plus_given_A_1_m * p_plus_given_B_1_m * p_plus_given_C_1_m
p_minus <- p_minus_given_A_1_m * p_minus_given_B_1_m * p_minus_given_C_1_m

# Consider test sample features
if (A_test == 0) {
  p_plus <- p_plus * (1 - p_A_given_plus)
  p_minus <- p_minus * (1 - p_A_given_minus)
}

if (B_test == 1) {
  p_plus <- p_plus * p_B_given_plus
  p_minus <- p_minus * p_B_given_minus
}

if (C_test == 0) {
  p_plus <- p_plus * (1 - p_C_given_plus)
  p_minus <- p_minus * (1 - p_C_given_minus)
}

# Calculate unnormalized posterior probabilities with m-estimates
unnormalized_posterior_plus <- p_plus * 0.5
unnormalized_posterior_minus <- p_minus * 0.5

# Predict class label for the test sample with m-estimates
if (unnormalized_posterior_plus > unnormalized_posterior_minus) {
  cat("The predicted class label for the test sample is + (Positive).")
} else if (unnormalized_posterior_plus < unnormalized_posterior_minus) {
  cat("The predicted class label for the test sample is - (Negative).")
} else {
  cat("The probabilities for both classes are equal. Cannot make a prediction.")
}

print("Explanation of m-estimate \n")
print("The m-estimate is often preferred over Maximum Likelihood Estimation (MLE) when there's limited data or prior information available.\n") 
print("It combines prior beliefs with observed data, providing more robust estimates, particularly in real-world scenarios.\n") 

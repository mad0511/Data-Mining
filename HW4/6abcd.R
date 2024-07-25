# Part A
undergrad_smoke_rate <- 0.15  # Smoking rate among undergraduate students
grad_smoke_rate <- 0.23  # Smoking rate among graduate students
prop_grad_students <- 1/5  # Proportion of students who are graduate students

# Step 1: Calculate total smoking proportion among all students
total_smoking_prop <- (prop_grad_students * grad_smoke_rate) + ((1 - prop_grad_students) * undergrad_smoke_rate)

# Step 2: Determine probability of randomly selecting a graduate student
prob_grad_student <- prop_grad_students

# Step 3: Apply Bayes' theorem to find probability of a smoker being a graduate student
prob_smoker_is_grad <- (prob_grad_student * grad_smoke_rate) / total_smoking_prop

# Output the result
print(prob_smoker_is_grad)

# Part B
prop_grad_students <- 1/5  # Proportion of students who are graduate students

# Calculate probability of selecting an undergraduate student
prob_undergrad_student <- 1 - prop_grad_students

# Calculate probability of selecting a graduate student
prob_grad_student <- prop_grad_students

# Compare probabilities
if (prob_grad_student > prob_undergrad_student) {
  print("A randomly chosen college student is more likely to be a graduate student.")
} else if (prob_grad_student < prob_undergrad_student) {
  print("A randomly chosen college student is more likely to be an undergraduate student.")
} else {
  print("The probabilities of choosing a graduate student and an undergraduate student are equal.")
}

# Part C
undergrad_smoke_rate <- 0.15  # Smoking rate among undergraduate students
grad_smoke_rate <- 0.23  # Smoking rate among graduate students
prop_grad_students <- 1/5  # Proportion of students who are graduate students

# Calculate probability of selecting an undergraduate smoker
prob_undergrad_smoker <- (1 - prop_grad_students) * undergrad_smoke_rate

# Calculate probability of selecting a graduate smoker
prob_grad_smoker <- prop_grad_students * grad_smoke_rate

# Compare probabilities
if (prob_grad_smoker > prob_undergrad_smoker) {
  print("A randomly chosen smoking college student is more likely to be a graduate student.")
} else if (prob_grad_smoker < prob_undergrad_smoker) {
  print("A randomly chosen smoking college student is more likely to be an undergraduate student.")
} else {
  print("The probabilities of choosing a graduate smoking student and an undergraduate smoking student are equal.")
}

# Part D
grad_dorm_rate <- 0.3  # Proportion of graduate students who live in a dorm
undergrad_dorm_rate <- 0.1  # Proportion of undergraduate students who live in a dorm

# Calculate probability of a smoking student in a dorm being a graduate student
prob_grad_smoker_dorm <- grad_dorm_rate * grad_smoke_rate

# Calculate probability of a smoking student in a dorm being an undergraduate student
prob_undergrad_smoker_dorm <- undergrad_dorm_rate * undergrad_smoke_rate

# Compare probabilities
if (prob_grad_smoker_dorm > prob_undergrad_smoker_dorm) {
  print("A smoking student living in a dorm is more likely to be a graduate student.")
} else if (prob_grad_smoker_dorm < prob_undergrad_smoker_dorm) {
  print("A smoking student living in a dorm is more likely to be an undergraduate student.")
} else {
  print("The probabilities of a smoking student living in a dorm being a graduate student and an undergraduate student are equal.")
}

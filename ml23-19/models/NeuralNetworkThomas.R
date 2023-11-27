install.packages("neuralnet")
install.packages("keras")
library(keras)
library(neuralnet)
library(caret)

#Loading the data
train <- read.csv('New Data/train.csv')
test <- read.csv('New Data/test.csv')
predictions <- read.csv('Original Data/predictions.csv')

set.seed(1234)
# Create a data frame to store cross-validation results
cv_results <- data.frame(neurons = num_neurons, MSE = numeric(length(num_neurons)))

# Perform cross-validation for each number of neurons
for (i in seq_along(num_neurons)) {
  n <- num_neurons[i]
  
  # Create the neural network model
  model <- neuralnet(
    return_rate ~ .,
    data = train,
    hidden = n,
    linear.output = TRUE
  )
  
  # Perform cross-validation and evaluate performance
  cv <- train(
    return_rate ~ .,
    data = train,
    method = "neuralnet",
    trControl = trainControl(method = "cv", number = 5),  # 5-fold cross-validation
    tuneGrid = data.frame(size = n)  # Use the current value of n
  )
  
  # Store MSE in cv_results
  cv_results$MSE[i] <- cv$results$RMSE[1]^2  # Square RMSE to get MSE
}

# Print the results
print(cv_results)

# Find the value of n with the lowest MSE
best_n <- cv_results$neurons[which.min(cv_results$MSE)]
cat("Best number of neurons:", best_n, "\n")

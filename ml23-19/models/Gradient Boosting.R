install.packages("xgboost")
install.packages("gbm")
library(xgboost)
library(gbm)

#Loading the data
train <- read.csv('New Data/train.csv')
test <- read.csv('New Data/test.csv')
predictions <- read.csv('Original Data/predictions.csv')

fit <- gbm(return_rate~.,data=train, distribution="gaussian",n.trees=100,interaction.depth=1,shrinkage=0.2)
preds <- predict(fit, newdata = test)
acc <- mean((preds-test$return_rate)^2)

# Convert data to DMatrix for XGBoost
#train_dmatrix <- xgb.DMatrix(data = as.matrix(train[, -which(names(train) == "return_rate")]), label = train$return_rate)
#test_dmatrix <- xgb.DMatrix(data = as.matrix(test[, -which(names(test) == "return_rate")]), label = test$return_rate)

# Train XGBoost
#xgb_model <- xgboost(data = train_dmatrix, nrounds = 500, objective = "reg:squarederror")

# Make predictions on the test set
#xgb_predictions <- predict(xgb_model, newdata = test_dmatrix)

predictions$return_rate <- preds
###Remove the row names in order to get the correct file type for our submission in the competition
rownames(predictions) <- NULL
predictions 
mean(predictions$return_rate)

### Write out our predictions to 
write.csv(predictions, 'New Data/submission.csv', row.names = FALSE)

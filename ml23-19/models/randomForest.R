install.packages("randomForest")
library(randomForest)

#Loading the data
train <- read.csv('New Data/train.csv')
test <- read.csv('New Data/test.csv')
predictions <- read.csv('Original Data/predictions.csv')

# RANDOM FORESTS, BASED ON #PREDICTORS p/3
rf.model <- randomForest(return_rate ~ ., data=train,mtry = ceiling((ncol(train_x)+60)/3), ntree = 10)
summary(rf.model)
importance(rf.model)
varImpPlot(rf.model)

# PREDICTION ON VALIDATION SET
predictions_rf <- predict(rf.model, validation_x)
pred_actual_rf <- data.frame(actual=validation$average_daily_rate, predicted= predictions_rf)
RMSE_rf<-sqrt(mean((pred_actual_rf$actual - pred_actual_rf$predicted)^2))

# BAGGING
bag.model <- randomForest(average_daily_rate ~ ., data=train, mtry = ncol(train_x)+60, ntree=10, importance = TRUE)
summary(bag.model)
varImpPlot(bag.model)

# VALIDATION OF THE MODEL
predictions_bag <- predict(bag.model, validation_x)
pred_actual_bag <- data.frame(actual=validation$average_daily_rate, predicted= predictions_bag)
RMSE_bag<-sqrt(mean((pred_actual_bag$actual - pred_actual_bag$predicted)^2))


predictions$return_rate <- rf_predictions
###Remove the row names in order to get the correct file type for our submission in the competition
rownames(predictions) <- NULL
predictions 
mean((predictions$return_rate-test$return_rate)^2)

### Write out our predictions to 
write.csv(predictions, 'New Data/submission.csv', row.names = FALSE)

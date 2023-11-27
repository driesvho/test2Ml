install.packages("caret")
install.packages("glmnet")
install.packages("leaps")
install.packages("stringr")
install.packages("lubridate")
library(caret)
library(glmnet)
library(leaps)
library(stringr)
library(lubridate)

#TRAINING OF OUR MODEL
#loading the data 
#original_train <- read.csv('Original Data/sales_yr1.csv')
train <- read.csv('New Data/train.csv')
predictions <- read.csv('Original Data/predictions.csv')

##We'll perform K-cross validation manually by changing the seed value and changing our training 
##and test data in each fold
##We'll choose the model with the lowest mean squared error 
##Deze methode skippen vanaf nu 
#set.seed(1) #IS HET NIET BETER HIER EEN VASTE SEED TE ZETTEN? KRIJG JE ONDERAAN BIJ MEAN(RR) SLECHTERE WAARDE
#mse <- rep(0,10)
#for(i in 1:10){
  #set.seed(i)
 # test.indices <- rbind(sample(1:nrow(train), nrow(train)/10))
  #lm.fit <- lm(return_rate ~ price + channel, data = train[-test.indices, ])
  #preds <- predict(lm.fit, train[test.indices, ], )
  #mse[i] <- mean((preds - train$return_rate[test.indices])^2)
#}

###We'll inspect our mean squared errors to check if no errors happened
#print(mse)
###Select the lowest mean squared error
#i <- which.min(mse)
### Run the model again, this is our best model 
#set.seed(i)
#test.indices <- rbind(sample(1:nrow(train), nrow(train)/10))
#best.lm.fit <- lm(return_rate ~ price + channel, data = train[-test.indices, ])

#CV VOLGENS INTERNET 
set.seed(1)
train_control <- trainControl(method = "cv",
                              number = 10)

# training the model by assigning sales column
# as target variable and rest other column
# as independent variable
regfit.full <- regsubsets(return_rate ~ ., data = train, nvmax = 10,really.big = T)
reg.summary <- summary(regfit.full)
reg.summary
par(mfrow = c(1, 1))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

best.lm.fit <- train(return_rate ~ price + channel + category_2+brand_quality, data = train, 
               method = "lm",
               trControl = train_control)

predictions_train <- predict(best.lm.fit, train)
mse_train <- mean((predictions_train - train$return_rate)^2)

#PREDICTING THE RETURN RATES
##We'll load the test set  
test <- read.csv('New Data/test.csv')

### Use this data set and our best linear regression model to perform the predictions
preds <- predict(best.lm.fit, test)
predictions$return_rate <- preds
###Remove the row names in order to get the correct file type for our submission in the competition
rownames(predictions) <- NULL
predictions 
mean((predictions$return_rate-test$return_rate)^2)
mean(predictions$return_rate)
### Write out our predictions to 
write.csv(predictions, 'New Data/submission.csv', row.names = FALSE)


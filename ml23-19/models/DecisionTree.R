install.packages(tree)
library(tree)

#Loading the data
train <- read.csv('New Data/train.csv')
train <- subset(train,select = -c(X))
test <- read.csv('New Data/test.csv')
predictions <- read.csv('Original Data/predictions.csv')

#Training our model
tree.model <- tree(return_rate ~ ., data = train)
summary(tree.model)

# PRUNE THE TREE
cv.tree.model <- cv.tree(tree.model)
plot(cv.tree.model$size, cv.tree.model$dev, type = 'b')
size.cv <- cv.tree.model$size[which.min(cv.tree.model$dev)]
prune.model <- prune.tree(tree.model, best = size.cv)
plot(prune.model)
text(prune.model, pretty = 0)

# VALDIATION OF THE MODEL
predictions_tree <- predict(prune.model, test)
pred_actual_tree <- data.frame(actual=test$return_rate, predicted= predictions_tree)
RMSE_tree<-sqrt(mean((pred_actual_tree$actual - pred_actual_tree$predicted)^2))


predictions$return_rate <- predictions_tree
###Remove the row names in order to get the correct file type for our submission in the competition
rownames(predictions) <- NULL
predictions 
mean((predictions$return_rate-test$return_rate)^2)
mean(predictions$return_rate)
### Write out our predictions to 
write.csv(predictions, 'Submissions/decisionTree.csv', row.names = FALSE)






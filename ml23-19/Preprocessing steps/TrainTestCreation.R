#first the necessary libraries are loaded
install.packages("dplyr")
install.packages("glmnet")
install.packages("boot")
install.packages("dummy")
install.packages("corrplot")
install.packages("fastDummies")
install.packages("mice")
library(fastDummies)
library(dplyr)
library(glmnet)
library(boot)
library(dummy)
library(corrplot)
library(mice)

#We then load in the csv files with the original data 
products <- read.csv("Original Data/products.csv")

#We will create a table with for each product only one row
calculate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Change price to numeric value
products <- products %>%
  mutate(price = as.numeric(sub("€", "", price)))
products <- products[complete.cases(products$price), ]

unique_products <- products %>%
  group_by(pid) %>%
  summarise(
    brand = calculate_mode(brand),
    #brand_quality = calculate_mode(brand_quality),
    category = calculate_mode(category),
    price = mean(price),
    season = calculate_mode(season),
    sku_size = calculate_mode(sku_size),
    subcategory = calculate_mode(subcategory),
    subsubcategory = calculate_mode(subsubcategory)
  ) %>%
  ungroup()

unique_products <- as.data.frame(unique_products)

#CREATION OF TRAINING SET 
## We'll load the table that has for each product id of year 1 the return rate 
return_rate_yr1 <- read.csv('New Data/return_rate_yr1.csv')
return_rate_yr1 <- subset(return_rate_yr1,select = c(pid,return_rate,channel,date_id))

train <- merge(unique_products, return_rate_yr1, by = "pid", all = FALSE)

modes_train <- sapply(train, calculate_mode)
for (col in names(train)) {
  train[, col][is.na(train[, col])] <- modes_train[col]
}

#CREATION OF TEST SET 
## We'll load the table that has for each product id of year 1 the return rate 
return_rate_yr2 <- read.csv('New Data/return_rate_yr2.csv')
return_rate_yr2 <- subset(return_rate_yr2,select = c(pid,return_rate,channel,date_id))

test <- merge(unique_products, return_rate_yr2, by = "pid", all.y = T)

#Replace NA values in test set with modus of training set 
for (col in names(test)) {
  test[, col][is.na(test[, col])] <- modes_train[col]
}

#DATA PREPROCESSING
#Make dummies for variables subcategory and season 
assign_subcategory <- function(subcategory) {
  if (endsWith(subcategory, "Adult")) {
    return(1)
  } else if (endsWith(subcategory, "Petite")) {
    return(2)
  } else if(endsWith(subcategory,"Items")){
    return(0)
  }
}

assign_season <- function(season){
  if(endsWith(season,"Winter"))
    {return(1)}
  else if (endsWith(season,"Summer"))
    {return(0)}
}

train <- train %>%
  mutate(subcategory = sapply(subcategory, assign_subcategory),
         season = sapply(season, assign_season))
test <- test %>%
  mutate(subcategory = sapply(subcategory, assign_subcategory),
         season = sapply(season, assign_season))

#Dummies for brand based on the sales in year 1
brand_dummies <- read.csv("New Data/brand_dummies.csv")
brand_dummies <- subset(brand_dummies, select = -X)
train <- merge(brand_dummies,train,by="brand")
test <- merge(brand_dummies,test,by="brand",all.y = TRUE)

#Creating dummies for variable category 
train <- train %>%
  mutate(category = case_when(
    category %in% c('Footwear') ~ 1,
    category %in% c('Trousers') ~ 2,
    category %in% c('Sweaters', 'Tops','Button-Ups', 'Outerwear','Blazers') ~ 3,
    category %in% c('Formal Attire') ~ 4,
    TRUE ~ 5
  ))

test <- test %>%
  mutate(category = case_when(
    category %in% c('Footwear') ~ 1,
    category %in% c('Trousers') ~ 2,
    category %in% c('Sweaters', 'Tops','Button-Ups', 'Outerwear','Blazers') ~ 3,
    category %in% c('Formal Attire') ~ 4,
    TRUE ~ 5
  ))

#Creating dummies for sku_size
train <- subset(train, select = -c(sku_size))
test <- subset(test, select = -c(sku_size))

sku_size_dummies <- read.csv('New Data/sku_size_dummies.csv')
sku_size_dummies <- sku_size_dummies %>%
  group_by(pid) %>%
  summarize(mean_size = mean(sku_size))
train <- merge(train,sku_size_dummies,by="pid")
test <- merge(test,sku_size_dummies,by="pid")

train <- subset(train, select = -c(pid,brand,date_id,subsubcategory))
test <- subset(test, select = -c(pid,brand,date_id,subsubcategory))

#training set
#Now create the real dummy variables for 'subcategory', 'sku_size', and 'category'
dummy_cols <- dummy_cols(train, select_columns = c("subcategory","category"))
dummy_cols <- subset(dummy_cols,select = -c(brand_quality,price,season,return_rate,channel))

# Add the dummy columns to the original data frame
train <- cbind(train, dummy_cols)
train <- subset(train,select = -c(subcategory,category))
train <- subset(train,select = -c(subcategory,category))

#test set
#Now create the real dummy variables for 'subcategory', 'sku_size', and 'category'
dummy_cols <- dummy_cols(test, select_columns = c("subcategory","category"))
dummy_cols <- subset(dummy_cols,select = -c(brand_quality,price,season,return_rate,channel))

# Add the dummy columns to the original data frame
test <- cbind(test, dummy_cols)
test <- subset(test,select = -c(subcategory,category))
test <- subset(test,select = -c(subcategory,category))

#Filling the NA brand with zero as they can not be classified as bad already
test$brand_quality[is.na(test$brand_quality)] <- 0

#Scale price and sku_size with the values from training set
#apply on training set
train$price <- as.numeric(train$price)
test$price <- as.numeric(test$price)

mean_train_price <- mean(train$price)
sd_train_price <- sd(train$price)
train[, "price"] <- scale(train[, "price"], center = TRUE, scale = TRUE)

# apply on test set
test[, "price"] <- scale(test[, "price"], center = mean_train_price, scale = sd_train_price)

mean_train_size <- mean(train$mean_size)
sd_train_size <- sd(train$mean_size)
train[, "mean_size"] <- scale(train[, "mean_size"], center = TRUE, scale = TRUE)

# apply on test set
test[, "mean_size"] <- scale(test[, "mean_size"], center = mean_train_size, scale = sd_train_size)

#Leaving out reference value for each dummy variable
train <- subset(train,select=-c(category_1,subcategory_1,mean_size.1))
test <- subset(test,select=-c(category_1,subcategory_1,mean_size.1))


#Checking correlation btwn dummy variables WEET NIET OF HET NUTTIG IS 
dummy_data <- subset(train, select = -c(price,return_rate,mean_size))
column_pairs <- combn(names(dummy_data), 2, simplify = TRUE)

# Calculate phi coefficient for each pair
phi_coefficients <- apply(column_pairs, 2, function(pair) {
  table_data <- table(dummy_data[, pair])
  phi_coefficient <- sqrt(chisq.test(table_data)$statistic / sum(table_data))
  return(phi_coefficient)
})

# Create a dataframe to store the results
association_results <- data.frame(
  Variable1 = column_pairs[1, ],
  Variable2 = column_pairs[2, ],
  Phi_Coefficient = phi_coefficients
)

cross_tab <- table(train$category_2, train$category_3)
print(cross_tab)

write.csv(train,"New Data/train.csv")
write.csv(test, "New Data/test.csv")

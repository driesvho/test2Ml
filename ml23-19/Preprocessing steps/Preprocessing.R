##In this file, all preprocessing steps necessary to perform a correct analysis 
##and make correct models on our data are performed. However sometimes, dummy 
##variables are created in seperate files, to keep a clean overview of how these
##are created
##Both the training and test set 

#First, we install the necessary packages
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
library(lubridate)

##DATA EXPLORATION
#Read in the necessary data 
original_train <- read.csv('Original Data/sales_yr1.csv')
products_data <- read.csv('Original Data/products.csv')

#We inspect the data
original_train_summary <- summary(original_train)
original_train_summary 

products_summary <- summary(products_data)
products_summary

#We merge the data based on product id
#We perform an outer join in order to not lose any product ids and any information
total_train_data <- merge(original_train, products_data, on = 'pid', all = TRUE)
summary(total_train_data)
na_counts <- colSums(is.na(total_train_data))
print(na_counts)





# Select rows for the year 2021
total_train_data$date_id <- as.Date(as.character(total_train_data$date_id), format = "%Y%m%d")
total_train_data$year <- year(total_train_data$date_id)
year_to_select <- 2021
total_train_data <- subset(total_train_data, year == year_to_select)
na_counts <- colSums(is.na(total_train_data))
print(na_counts)

###We see here that all NAs are gone in our dataframe 


##For the next steps, we will create dummy variables so we can use the same training set for all 
##models we'll create during the project

##We create a dummy variable for channel, differentiating between online and physical channels 
total_train_data$channel <- ifelse(total_train_data$channel %in% c("CH//Online", "CH//amazon.com"), 1, 0)

##We change the date type of price from char to a numeric type and omit the € sign, this allows us to 
##make predictions based on the price of a product
total_train_data <- total_train_data %>%
  mutate(price = as.numeric(sub("€", "", price)))
total_train_data <- total_train_data[complete.cases(total_train_data$price), ]

## We want to create dummy variables based on brand
## To do this, we differentiate between those brands that have historically higher than average 
## return rates, and those that don't.
### We will create a table that has all unique product ids and as features the ones that appear the most often
### Function to calculate mode
calculate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

###We read in a newly created csv frame, which is created in the R file "brandDummies.R"
### To create these dummies, we use all historical data 
brand_dummies <- read.csv("New Data/brand_dummies.csv")
brand_dummies <- subset(brand_dummies, select = -X)
total_train_data <- merge(brand_dummies, total_train_data,by="brand")
total_train_data <- subset(total_train_data, select = -c(brand))


##We create a dummy variable for channel, differentiating between online and physical channels 

##We create a dummy variable for channel, differentiating between online and physical channels 


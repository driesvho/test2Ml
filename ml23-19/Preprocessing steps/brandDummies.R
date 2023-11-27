# Load the necessary library
install.packages("dplyr")
install.packages("dummy")
install.packages("stringr") 
install.packages("ggplot2")
library(dplyr)
library(dummy)
library(stringr)
library(ggplot2)

#Defining some functions 
impute <- function(x, method = mean, val = NULL) {
  if (is.null(val)) {
    val <- method(x, na.rm = TRUE)
  }
  x[is.na(x)] <- val
  return(x)
}
modus <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

# Read the CSV file into an R data frame
products <- read.csv("Original Data/products.csv")
sales_train <- read.csv("Original Data/sales_yr1.csv")
sales_train$date_id <- as.Date(as.character(sales_train$date_id), format = "%Y%m%d")
sales_train$date_id <- impute(sales_train$date_id, method = mean) 
sales_train$net_sales_amount <- impute(sales_train$net_sales_amount, method = mean) 

returnrateBrands <- merge(products,sales_train,by="pid")

returnrateBrands <- subset(returnrateBrands, select = c(brand, net_sales_amount))

# Perform the calculations
return_rate_brands <- returnrateBrands %>%
  mutate(net_sales_amount = as.numeric(net_sales_amount)) %>%  
  group_by(brand) %>%  # Group data by product ID ('brand')
  summarize(
    total_returned = sum(net_sales_amount[net_sales_amount < 0], na.rm = TRUE),  # Sum negative amounts
    total_sold = sum(net_sales_amount[net_sales_amount > 0], na.rm = TRUE),  # Sum positive amounts
    return_rate = -total_returned / total_sold  # Calculate return rate
  ) %>%
  mutate(return_rate = ifelse(is.nan(return_rate) | is.infinite(return_rate), 0, return_rate)) %>%  
  ungroup() 

# Create the scatter plot
ggplot(return_rate_brands, aes(x = return_rate, y = total_sold, color = brand,legend=FALSE)) +
  geom_point() +
  labs(title = "Scatter Plot of Return Rate vs Total Sold",
       x = "Return Rate",
       y = "Total Sold",
       color = "Brand") +
  theme_minimal()

# Create the dummy variable
return_rate_brands$brand_quality = ifelse(return_rate_brands$return_rate >= 0.25, 1, 0)

brand_dummies <- subset(return_rate_brands,select = c(brand,brand_quality))
write.csv(brand_dummies,"New Data/brand_dummies.csv")


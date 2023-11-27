#dummies sku_size Formal Attire

library(dplyr)
products$sku_size <- trimws(products$sku_size)
size_mappingsFA <- c("46"=1, "24"=1, "48"=1, "25"=2, "50"=2, "52"=3, "26"=3, "54"=4, "27"=4,"56"=5, "58"=5, "28"=5)
products$FormalAttireDummy <- NA  # Initialize the column with NA

for (i in 1:nrow(products)) {
  if (products$category[i] == "Formal Attire" && as.character(products$sku_size[i]) %in% names(size_mappingsFA)) {
    products$FormalAttireDummy[i] <- size_mappingsFA[[as.character(products$sku_size[i])]]
  }
}
head(products)


# dummies sku_size Add-Ons

library(dplyr)
products$sku_size <- trimws(products$sku_size)
size_mappingsAO <- c("S"=1, "M"=2, "L"=3, "XL"=4, "XXL"=5, "XXXL"=5)
products$AddOnsDummy <- NA  # Initialize the column with NA

for (i in 1:nrow(products)) {
  if (products$category[i] == "Add-Ons" && as.character(products$sku_size[i]) %in% names(size_mappingsAO)) {
    products$AddOnsDummy[i] <- size_mappingsAO[[as.character(products$sku_size[i])]]
  }
}



# dummies sku_size Blazers

library(dplyr)
products$sku_size <- trimws(products$sku_size)
size_mappingsB <- c("46"=1, "48"=1, "24"=1, "94"=1, "25"=2, "50"=2, "52"=3, "26"=3, "54"=4, "27"=4, "56"=5, "58"=5, "60"=5, "28"=5, "29"=5, "S"=1, "M"=2, "L"=3, "XL"=4, "XXL"=5, "XXXL"=5, "4XL"=5, "94"=1, "98"=2, "102"=3, "106"=4, "114"=5)
products$BlazersDummy <- NA  # Initialize the column with NA

for (i in 1:nrow(products)) {
  if (products$category[i] == "Blazers" && as.character(products$sku_size[i]) %in% names(size_mappingsB)) {
    products$BlazersDummy[i] <- size_mappingsB[[as.character(products$sku_size[i])]]
  }
}
# Assuming your data frame is named 'products'
blazers_head <- products %>%
  filter(category == "Blazers") %>%
  head()

print(blazers_head)


products <- read.csv("Original Data/products.csv")
products$CategorySizeDummy <- NA  # Initialize the column with NA
size_mappingsFA <- c("46"=1, "24"=1, "48"=1, "25"=2, "50"=2, "52"=3, "26"=3, "54"=4, "27"=4,"56"=5, "58"=5, "28"=5)
size_mappingsAO <- c("S"=1, "M"=2, "L"=3, "XL"=4, "XXL"=5, "XXXL"=5)
size_mappingsBlazers <- c("46"=1, "48"=1, "24"=1, "94"=1, "25"=2, "50"=2, "52"=3, "26"=3, "54"=4, "27"=4, "56"=5, "58"=5, "60"=5, "28"=5, "29"=5, "S"=1, "M"=2, "L"=3, "XL"=4, "XXL"=5, "XXXL"=5, "4XL"=5, "94"=1, "98"=2, "102"=3, "106"=4, "114"=5)

for (i in 1:nrow(products)) {
  if (products$category[i] == "Formal Attire" && as.character(products$sku_size[i]) %in% names(size_mappingsFA)) {
    products$CategorySizeDummy[i] <- size_mappingsFA[[as.character(products$sku_size[i])]]
  }
  else if (products$category[i] == "Blazers" && as.character(products$sku_size[i]) %in% names(size_mappingsBlazers)) {
    products$CategorySizeDummy[i] <- size_mappingsBlazers[[as.character(products$sku_size[i])]]
  }
  else if (products$category[i] == "Add-Ons" && as.character(products$sku_size[i]) %in% names(size_mappingsAO)) {
    products$CategorySizeDummy[i] <- size_mappingsAO[[as.character(products$sku_size[i])]]
  }
  # Add more else if blocks for other categories
}

# Assuming your data frame is named 'products'
blazers_head <- products %>%
  filter(category == "Blazers") %>%
  head()

#
print(blazers_head)
# Assuming products is your data frame with columns 'sku_size' and 'category'
products <- read.csv("Original Data/products.csv")
products$sku_size <- trimws(products$sku_size)
unique(products$sku_size)

#Formal Attire CHECK
unique(products$sku_size[products$category == 'Formal Attire'])
products <- products %>%
  mutate(sku_size = ifelse(category == 'Formal Attire',
                           case_when(
                             sku_size %in% c('0',NA) ~ 0,
                             sku_size %in% c('46','24','48') ~ 1,
                             sku_size %in% c('25', '50', 'M') ~ 2,
                             sku_size %in% c('52', '26') ~ 3,
                             sku_size %in% c('54', '27') ~ 4,
                             TRUE ~ 5
                           ),
                           sku_size
  ))

#Add-Ons CHECK
unique(products$sku_size[products$category == 'Add-Ons'])
products <- products %>%
  mutate(sku_size = ifelse(category == 'Add-Ons',
                           case_when(
                             sku_size %in% c('0',NA) ~ 0,
                             sku_size %in% c('S') ~ 1,
                             sku_size %in% c('M') ~ 2,
                             sku_size %in% c('L') ~ 3,
                             sku_size %in% c('XL') ~ 4,
                             TRUE ~ 5
                           ),
                           sku_size
  ))

#Blazers CHECK
unique(products$sku_size[products$category == 'Blazers'])
products <- products %>%
  mutate(sku_size = ifelse(category == 'Blazers',
                           case_when(
                             sku_size %in% c('0',NA) ~ 0,
                             sku_size %in% c('46','24','48','94','S') ~ 1,
                             sku_size %in% c('25', '50','M','98') ~ 2,
                             sku_size %in% c('52', '26','L','102') ~ 3,
                             sku_size %in% c('54', '27','XL','106') ~ 4,
                             TRUE ~ 5
                           ),
                           sku_size
  ))

#Miscellaneous CHECK
unique(products$sku_size[products$category == 'Miscellaneous'])
products <- products %>%
  mutate(sku_size = ifelse(category == 'Miscellaneous',
                           case_when(
                             sku_size %in% c('0','1',NA) ~ 0,
                             sku_size %in% c('3942','S','8,5','57','36','85') ~ 1,
                             sku_size %in% c('M','4146','9','58','90') ~ 2,
                             sku_size %in% c('4346','L','9,5','59','95') ~ 3,
                             sku_size %in% c('XL','10,5','60','100') ~ 4,
                             TRUE ~ 5
                           ),
                           sku_size
  ))

#Outerwear CHECK
unique(products$sku_size[products$category == 'Outerwear'])
products <- products %>%
  mutate(sku_size = ifelse(category == 'Outerwear',
                           case_when(
                             sku_size %in% c('0',NA) ~ 0,
                             sku_size %in% c('48','S','46','XS','38','40','44','46') ~ 1,
                             sku_size %in% c('50','M') ~ 2,
                             sku_size %in% c('52','L') ~ 3,
                             sku_size %in% c('54','XL') ~ 4,
                             TRUE ~ 5
                           ),
                           sku_size
  ))

#High-End Items CHECK
unique(products$sku_size[products$category == 'High-End Items'])
products <- products %>%
  mutate(sku_size = ifelse(category == 'High-End Items',
                           case_when(
                             sku_size %in% c('0',NA,'GEEN') ~ 0,
                             sku_size %in% c('S','44','46','48','25','7','7,5','8','40', "42","40","41","43","45","8,5","38",'39',"28/32","29/32") ~ 1,
                             sku_size %in% c('M','50','26','9',"32/32" ,"31/32") ~ 2,
                             sku_size %in% c('L','52','27','9,5','95','30/34',"29/34") ~ 3,
                             sku_size %in% c('XL','54','28','10,5','10','34/34',"31/34","32/34","33/34",'34/32') ~ 4,
                             TRUE ~ 5
                           ),
                           sku_size
  ))

#Sweaters CHECK
unique(products$sku_size[products$category == 'Sweaters'])
products <- products %>%
  mutate(sku_size = ifelse(category == 'Sweaters',
                           case_when(
                             sku_size %in% c('0',NA) ~ 0,
                             sku_size %in% c('S','3','XS','48') ~ 1,
                             sku_size %in% c('M','4','50') ~ 2,
                             sku_size %in% c('L','5','52') ~ 3,
                             sku_size %in% c('XL','6','54') ~ 4,
                             TRUE ~ 5
                           ),
                           sku_size
  ))

#Footwear CHECK
unique(products$sku_size[products$category == 'Footwear'])
products <- products %>%
  mutate(sku_size = ifelse(category == 'Footwear',
                           case_when(
                             sku_size %in% c('0',NA) ~ 0,
                             sku_size %in% c('39','40','41','5','6','6,5',"8","7","8,5",'9,5','7,5','9') ~ 1,
                             sku_size %in% c('42','43','10','10,5','M') ~ 2,
                             sku_size %in% c('44','11','11,5') ~ 3,
                             sku_size %in% c('45','46','12','12,5') ~ 4,
                             TRUE ~ 5
                           ),
                           sku_size
  ))

#Trousers CHECK
unique(products$sku_size[products$category == 'Trousers'])
products <- products %>%
  mutate(sku_size = ifelse(category == 'Trousers',
                           case_when(
                             sku_size %in% c('0',NA) ~ 0,
                             sku_size %in% c('S','3','XS','27','25','24','48','28','26','46','29/32','28/32','44','42','30','31','33','36','32','34','35','30/30','38','40','28/30','29/30','29') ~ 1,
                             sku_size %in% c('M','4','50','31/32','30/32','32/30','31/30') ~ 2,
                             sku_size %in% c('L','5','52','32/32','33/32', '29/34','33/30','98','28/34') ~ 3,
                             sku_size %in% c('XL','6','54','32/34','31/34','33/34','34/34','34/32','34/30','30/34','102') ~ 4,
                             TRUE ~ 5
                           ),
                           sku_size
  ))

#Tops CHECK
unique(products$sku_size[products$category == 'Tops'])
products <- products %>%
  mutate(sku_size = ifelse(category == 'Tops',
                           case_when(
                             sku_size %in% c('0',NA) ~ 0,
                             sku_size %in% c('S','XS','48','3','46','42',"40","43","41","37","38","39") ~ 1,
                             sku_size %in% c('M','50','4') ~ 2,
                             sku_size %in% c('L','52','5') ~ 3,
                             sku_size %in% c('XL','54','6') ~ 4,
                             TRUE ~ 5
                           ),
                           sku_size
  ))

#Button-Ups CHECK
unique(products$sku_size[products$category == 'Button-Ups'])
products <- products %>%
  mutate(sku_size = ifelse(category == 'Button-Ups',
                           case_when(
                             sku_size %in% c('0',NA) ~ 0,
                             sku_size %in% c('S','XS','46','48',"40","43","39","44","42","41","45","38","47","37") ~ 1,
                             sku_size %in% c('M','50') ~ 2,
                             sku_size %in% c('L','52','5') ~ 3,
                             sku_size %in% c('XL','54','6') ~ 4,
                             TRUE ~ 5
                           ),
                           sku_size
  ))


# Print the updated data frame
unique(products$sku_size)
sku_size_dummies <- subset(products,select = c(pid,sku_size))
write.csv(sku_size_dummies,'New Data/sku_size_dummies.csv')

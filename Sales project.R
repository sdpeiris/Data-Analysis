rm(list=ls())
cat("\014") 

####setting working directory
setwd("C:/Users/Senal Peiris/OneDrive/Desktop/R dataset")

#options("scipen"=99999, digits=3)

library(tidyverse)
#library(dplyr)
library(lubridate)
#library(ggplot2)


data <- read.csv('Sales.csv')

str(data)

### convert invoice date to a date/time

data$InvoiceDate <- as.POSIXct(data$InvoiceDate, format = "%m/%d/%Y %H:%M")
str(data) ## checking to see that data conversion worked fine

data$year <- year(data$Date)
data$month <- month(data$Date)
data$day <- day(data$Date)
data$hour <- hour(data$Date)
data$minute <- minute(data$Date)


##main dataset which will be used in analysis
data <- filter(data,UnitPrice>=0 ,Quantity >= 0)

##dropping all NA values from my analysis
data <- drop_na(data)

sum(is.na(data$CustomerID))

colnames(data) <- c("Invoice_no","Stock_code","Desc","Qty","Date","Price","Cust_ID","Country")

data <- data %>% mutate(Revenue =Price * Qty)

sum(data$Highsales)

### taking the logs of the variable price in a new column
data <- data %>% mutate(log_price =log(data$Price))
### as log zero is infinity i am changing infinity to zero again
data <- data %>% mutate(log_price = ifelse(data$log_price =='-Inf',0,data$log_price))


dta2 <- data %>% group_by(Stock_code,Country) %>%
  summarise(weighted_price = sum(Price * Qty) / sum(Qty)) 

dta3 <- data %>% group_by(Country) %>%
  summarise(weighted_price = sum(Price * Qty) / sum(Qty),
            Revenue = sum(Revenue)) 


Top5 <- data %>% group_by(Country) %>%
  summarise(weighted_price = sum(Price * Qty) / sum(Qty),
            Revenue = sum(Revenue),
            Quantity = sum(Qty)) %>% 
  arrange(Top5,Desc(Revenue))

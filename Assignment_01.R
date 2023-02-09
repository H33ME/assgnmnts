# set the working directory
setwd("C:/Users/Herman/Documents/Assignment_Midterm")

# load the packages
library(tidyverse) # for data manipulation and visualization
library(readxl) #for loading the data

# load the data
# set the column names
col_names <- c("Location", "Price", "Crime_Rating")
housing_data <- readxl::read_excel("File for Midterm.xlsx",
                                   skip = 1,
                                   col_names = col_names)

# cleaning the data
# check for missing values
table(is.na(housing_data))# no missing values
# refactoring Location variable
housing_data<- housing_data %>% 
  mutate(
   Location = factor(Location),
   Crime_Rating = factor(Crime_Rating)
  )
# check the data type 
str(housing_data)

# a density plot for the house price and Locations
options(scipen = 1e6)
housing_data %>%
  ggplot(aes(x = Price,
             fill = Location))+
  geom_density(alpha = 0.5) +
  labs(title = "A density plot for House Price colored with different location",
       x = "House Prices")

# a bar plot for crime rating
housing_data %>% 
  ggplot(aes(x = Crime_Rating,
             y = ..count..))+
  geom_bar(aes(fill = Location),
           position = "dodge")+
  labs(title = "A bar Plot displaying the distribution of different crime rates 
       in NY and FL locations",
       x = "Crime Rating",
       y = "Count")

# a bar plot for crime rate, locations and house prices
housing_data %>% 
  ggplot(aes(x = Crime_Rating,
             y = Price))+
  geom_col(aes(fill = Location),
           position = "dodge")+
  labs(title = "A bar plot showing distribution of house price on different 
  locations with different crime rating",
       x = "Crime Rating",
       y = "House Prices")
# average house price
avg_house_price <- 100000

# create a new data with new variables
new_housing_data <- housing_data %>% 
  filter(Price >= 100000) %>% 
  mutate(Avg_Income = ifelse(test = Location == "NY",
                             yes = 120000,
                             no = 75000),
         Able_to_pay_house = ifelse(test = Price>= 175000,
                                    yes = "Able to pay FL",
                                    no = "Not Able to pay FL"),
         Able_to_pay_house = ifelse(test = Price<= 220000,
                                    yes = "Able to pay NY",
                                    no = "Not Able to pay NY"),
         Able_to_pay_house = factor(Able_to_pay_house))

# A pie chart for the houses shes able to pay off
new_housing_data %>% 
  ggplot(aes(x = "", 
             y = Price,
             fill = Able_to_pay_house)) +
  geom_col()+
  coord_polar(theta = "y")+
  scale_fill_brewer()
                                        
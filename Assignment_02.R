
# load the required packages
library(tidyverse)
library(readr)

# load the data
# column names 
column_names <- c("Location", "Price", "Crime_Rating")
house_df <- read_xlsx(path = "C:/Users/Herman/Documents/Assignment_Midterm/File for Midterm.xlsx",
                      skip = 1,
                      col_names = column_names)

# structure of data
str(house_df)

# cleaning the dataset
# factoring the variables
house_df <- house_df %>% 
  mutate(
    Location = factor(Location),
    Crime_Rating = factor(Crime_Rating)
  )

# a histogram for the House Prices
house_df %>% 
  ggplot(aes(x = Price,
             fill = Location))+
  geom_histogram(bins =20 )+
  labs(title = "A Histogram that shows how the house prices are 
       distributed on different locations",
       x = "House Price")

# a barplot for the crime rate in NY and FL
house_df %>% 
  ggplot(aes(x = Crime_Rating,
             y = ..count..))+
  geom_bar(aes(fill = Location),
           position = "dodge")+
  coord_flip()+
  labs(title = "A bar plot showing the distribution of crime rate in 
       different regions",
       x = "Crime Rates",
       y = "Count")
# a box plot for the relationship between house price, crime rate and location
house_df %>% 
  ggplot(aes(x = Crime_Rating,
             y = Price,
             fill = Location))+
  geom_boxplot()+
  labs(title = "A Box Plot for the relationship between house price, 
       crime rates on different locations.",
       x = "Crime Rates",
       y = "House price")

# create new variable
new_house_df <- house_df %>%
  filter(Price <= 220000) %>% # filter price lower than 220000
  mutate(
    pay_off_house = ifelse(  # new variable
      test = Price <= 175000,
      yes = "Pays FL House Price",
      no = "Pays NY House Price"
    ),
    pay_off_house = factor(pay_off_house) # factoring the variable
  )

# a donut chart for the new data
# compute the percentage
new_house_df$fraction <- new_house_df$Price /sum(new_house_df$Price)
# compute the cumulative percentage( top of each rectangle)
new_house_df$ymax <- cumsum(new_house_df$fraction)
# compute the bottom of each rectangle
new_house_df$ymin <- c(0, head(new_house_df$ymax, n = -1))
# compute the label position
new_house_df$labelPosition <- (new_house_df$ymax+new_house_df$ymin)/2
# compute a good label
new_house_df$label <- paste0(new_house_df$pay_off_house, "\n value:", 
                             new_house_df$Price)
# make the plot
new_house_df %>% 
  ggplot(aes(ymax = ymax, ymin = ymin, xmin = -2, xmax = 2, fill = pay_off_house))+
  geom_rect()+
  scale_fill_brewer(palette = 4)+
  scale_color_brewer(palette = 3)+
  coord_polar(theta = "y")+
  geom_text(x =2, aes(y=labelPosition, label = label, color = pay_off_house))+
  theme_void()

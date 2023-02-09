# set the working directory
setwd("./new_project")

# load the data and the package
library(tidyverse)
index_data <- read_csv("./index_data_oecd.csv")
dat <- readxl::read_excel("./index-data-oecd.xls")

# select the main columns since most have been repeated
# clean the data
clean_index_data <- index_data %>%
  select(c(
    "Country",
    "Indicator",
    "Inequality",
    "Unit",
    "Value"
  )) %>%
  mutate(
    "Country" = factor(Country),
    "Indicator" = factor(Indicator),
    "Inequality" = factor(Inequality),
    "Unit" = factor(Unit)
  )

# select 5 countries
country <- c("United States","Canada", "France", "United Kingdom", "Germany")

clean_index_data %>%
  filter(Country %in% country,
         Value <= 400) %>%
  ggplot(aes(y = Value,
             x = Country)) +
  geom_bar(aes(fill = Inequality),
           stat = "identity",
           position = 'dodge') +
  labs(
    title = "A Bar plot that shows the distribution of Inequality of different Countries",
    x = "five different Countries",
    y = "Rate of levels of inequality",
    subtitle = "The distribution inequality is high in United States followed by women and men in that order",
    caption = "Source of the data: https://stats.oecd.org/Index.aspx?DataSetCode=BLI"
  )

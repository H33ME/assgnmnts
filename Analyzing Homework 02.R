# read the data and package required
library(tidyverse)
index_df <- read_csv("./BLI_08022023142635783.csv")

# a function that converts the character variables to factor
is_fact_func <- function(dat) {
  listdat <- list()
  for (i in seq_along(dat)) {
    if (is.character(dat[[i]])) {
      listdat[[i]] <- factor(dat[[i]])
    } else{
      listdat[[i]] <- dat[[i]]
    }
  }
  newdat <- data.frame(listdat)
  colnames(newdat) <- colnames(dat)
  return(as_tibble(newdat))
}

# clean the data
index_df <- index_df %>%
  is_fact_func() %>% # convert to factors
  select(c(
    "LOCATION",
    "Country",
    "Inequality",
    "Indicator",
    "Unit",
    "Value"
  ))

# select 6 countries at random
set.seed(12345)
six_countries <- sample(x = index_df$Country, size = 6)
# select indicators
five_indicators <-
  c("Air pollution",
    'Employment rate',
    "Homicide rate",
    'Life expectancy',
    'Water quality')
# create a plot
index_df %>%
  filter(Country %in% six_countries,
         Value <= 5000,
         Indicator %in% five_indicators) %>%
  ggplot(aes(x = Indicator,
             y = Value)) +
  geom_col(aes(fill = Unit)) +
  facet_wrap( ~ Country)+
  scale_x_discrete(guide = guide_axis(n.dodge = 3))+
  labs(
    title = "Different bar plots that shows how different countries indicators relates to the units variable",
    x = "Country's Indicators",
    y = "Different Unit's value",
    subtitle = "the highest units is Percentage of Water quality indicator in Netherlands and Austria countries.
    The lowest unit is the Ratio of Homicide rates in all countries used in the plots",
    caption = "This data was obtained from the OECD website with the following URL: https://stats.oecd.org/Index.aspx?DataSetCode=BLI"
  )

library(tidyverse)
library(dplyr)

# After opening the un-report project from yesterday with
# File > Open Project

gapminder_df = read_csv("data/gapminder_data.csv")
gapminder_df[1:5,] # Print the columns names, data types, and first few rows of
tail(gapminder_df) # Same for bottom 

###

gapminder_df[6,] # Show the 6th row of the data
gapminder_df$country[1] # Show the value of country for 1st row of the data

trial = c(1,2,3) # This is a vector of three numeric values
class(trial) # Tells us the data type of the vector (numeric)

index = c(TRUE, FALSE, TRUE) # This is a vector of 3 boolean values
class(index) 

#Boolean indexing
trial[index] # Select only rows 1,3 (because index is TRUE for elements 1,3)

# What is the mean life expectancy over all the data
gapminder_df %>% summarize(mean_le = mean(lifeExp))

# Assign that mean life expectancy to a variable
mean_le = gapminder_df %>% summarize(mean_le = mean(lifeExp))

# Extract all the years from the data
yr = gapminder_df$year

# Now use the unique function
un_yr = unique(yr)

# Print the unique years
un_yr

# Filter the gapminder data by the year 2002
yr_2002 = gapminder_df %>% filter(year == 2002)

# Find the maximum life expectancy for the year 2002
le_2002 = gapminder_df %>% 
  filter(year == 2002) %>%
  summarize(max_le = max(lifeExp))

# Find the unique countries
un_cty = unique(gapminder_df$country)
un_cty

# Find the mean life expectancy of each country
# Here we introduce the group_by() function
mean_country = gapminder_df %>% 
  group_by(country) %>%
  summarize(mean_le = mean(lifeExp))

##summary so far
# summarise, unique, filter, group_by, head, tail, mean, max, %>%, extracting columns
# subsetted tables
# generated summary stats

# Summarise returns fewer rows than input
?mutate()
head(gapminder_df)

# calculate the total GDP
gapminder_df %>% 
  mutate(gdp = pop * gdpPercap)

# choose a subset of columns
gapminder_df %>% 
  select(pop, year)

# choose a subset of columns to hide
gapminder_df %>% 
  select(-continent)

# Exercise: select columns and create a new dataframe 
# with only the country, continent, year, & LifeExp columns

new_df <- gapminder_df %>%
  select(country, continent, year, lifeExp)
new_df

# Data can be "wide" (many columns) or "long" (many rows) [aka tall]
# gapminder_df is currently "long"

wide_df <- gapminder_df %>%
  select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp)
wide_df$'1952'

new_df %>%
  pivot_wider(names_from = year, values_from = lifeExp)

library(tidyr)

head(gapminder_df)%>%
  select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp)

gapminder_2007_df <- gapminder_df %>%
  filter(year == 2007) %>%
  filter(continent == 'Americas') %>%
  select(-year, -continent)
gapminder_2007_df


getwd()

### Cleaning up data

# read_csv assumes the first line defines the column names.
# In our case the file header doesn't provide good column names
# so we'll skip the first 2 lines and define the columns names explicitly.
read_csv("data/co2-un-data.csv", 
         skip=2,
         col_names=c('region', 'country', 'year', 'series', 
                     'value', 'footnotes', 'source')) %>%
  select(country, year, series, value)
  
# The existing data in series column have an unwieldy format.
# The mutate command can convert the values to make them more concise.
read_csv("data/co2-un-data.csv", 
           skip=2,
           col_names=c('region', 'country', 'year', 'series', 
                       'value', 'footnotes', 'source')) %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, 
                         "Emissions (thousand metric tons of carbon dioxide)" = "total",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita"))
  

# finally we assign it
co2_emission_long <- read_csv("data/co2-un-data.csv", 
         skip=2,
         col_names=c('region', 'country', 'year', 'series', 
                     'value', 'footnotes', 'source')) %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, 
                         "Emissions (thousand metric tons of carbon dioxide)" = "total",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita"))

head(co2_emission_long)

co2_emission_wide <- co2_emission_long %>%
  pivot_wider(names_from=series, values_from=value) %>%
  filter(year==2005) %>%
  select(-year)

head(co2_emission_wide)
#

gapminder_2007 <- read_csv("data/gapminder_data.csv") %>%
  filter(year ==2007 & continent == "Americas") %>%
  select(-year, -continent)
gapminder_2007

# test joining our data
co2_emission
gapminder_2007

gapminder_co2 <- inner_join(gapminder_2007, co2_emission, by="country")
ggplot(gapminder_co2, aes(x=gdpPercap, y=per_capita)) +
  geom_point()


?mutate

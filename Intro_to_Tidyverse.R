### Introduction to Tiyverse - Datacamp ###

install.packages("gapminder")

library(dplyr)
library(gapminder)

View(gapminder)

gapminder <- gapminder

## FILTER verb - It is used to filter out the required data from the data frame

## Example

gapminder %>% filter(year == 2007)

gapminder %>% filter(country == "India")

gapminder %>% filter(year == 2007, country == "India")

## ARRANGE - It is used to arrange/sort the dataframe based on a particular column

## Example

gapminder %>% arrange(year)

gapminder %>% arrange(desc(year))

gapminder %>% arrange(continent)

gapminder %>% arrange(desc(continent))

### MUTATE - its is used to make modifications to a column or add a new column

## Example 

gapminder %>% mutate(gdp = gdpPercap/pop)

## SUMMARIZE - It is used to summarize the observations based on a column

gapminder %>% summarize(mean_lifeExp = mean(lifeExp))

gapminder %>% filter(year == 2007) %>% summarize(mean_lifeExp = mean(lifeExp))

gapminder %>% filter(year == 2007) %>% group_by(continent) %>% summarize(mean_lifeExp = mean(lifeExp))

## GROUP BY - It is used to group the data

gapminder %>% group_by(continent)

gapminder %>% group_by(continent, year)

############## GGPLOT2###########################

library(ggplot2)

##### Scatter Plot #############

ggplot(gapminder, aes(x = gdpPercap, y = pop)) + geom_point()

ggplot(gapminder, aes(x = gdpPercap, y = pop)) + geom_point() + scale_x_log10() +  scale_y_log10()


##### Line Plot #############

# Summarize the median gdpPercap by year, then save it as by_year
by_year <- gapminder %>% group_by(year) %>% summarize(medianGdpPercap = median(gdpPercap))

by_year

# Create a line plot showing the change in medianGdpPercap over time

ggplot(by_year, aes(x = year, y = medianGdpPercap)) + geom_line() + expand_limits (y=0)

# Summarize the median gdpPercap by year & continent, save as by_year_continent

by_year_continent <- gapminder %>% group_by(continent, year) %>% summarize (medianGdpPercap = median(gdpPercap))

by_year_continent

# Create a line plot showing the change in medianGdpPercap by continent over time

ggplot(by_year_continent, aes(x = year, y = medianGdpPercap, color = continent)) + geom_line() + expand_limits(y = 0)


################## BAR PLOT ###################


# Summarize the median gdpPercap by year and continent in 1952
by_continent <- gapminder %>% filter(year == 1952) %>% group_by(continent) %>% summarize(medianGdpPercap = median(gdpPercap))

by_continent


# Create a bar plot showing medianGdp by continent
ggplot(by_continent, aes(x=continent, y = medianGdpPercap)) + geom_col()

# Filter for observations in the Oceania continent in 1952
oceania_1952 <- gapminder %>% filter(continent == 'Oceania', year == 1952) 
head(oceania_1952)

# Create a bar plot of gdpPercap by country

ggplot(oceania_1952, aes(x = country, y = gdpPercap)) + geom_col()

###### Histogram ##########

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

head(gapminder_1952)

# Create a histogram of population (pop)
ggplot(gapminder_1952, aes(x=pop)) + geom_histogram() + scale_x_log10()

######### BOX PLOT #############

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

head(gapminder)

# Create a boxplot comparing gdpPercap among continents
ggplot(gapminder_1952, aes(x = continent, y = gdpPercap)) + geom_boxplot() + scale_y_log10()

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Add a title to this graph: "Comparing GDP per capita across continents"
ggplot(gapminder_1952, aes(x = continent, y = gdpPercap)) +
  geom_boxplot() + ggtitle("Comparing GDP per capita across continents")
scale_y_log10()
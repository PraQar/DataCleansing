# Introduction to tidyverse
# analysis of dataset gapminder 

#install package -- tidyverse 
install.packages("tidyverse")

#install package -- gapminder
install.packages("gapminder")

#load package gapminder
library(gapminder)

#load package dplyr
library(dplyr)

#view the dataset gapminder
head(gapminder)

#filter the dataset for year #2007
gapminder %>% filter(year == 2007)

#filter from dplyr
gapminder %>% filter(year == 2007 , country == 'China')

#filter the data where life expectency is > 70 %
gapminder %>% filter(lifeExp > 70)

#Sort the data by population 
gapminder %>% arrange(pop)
gapminder %>% arrange(desc(pop)) # sorting direction 

#Sort and Filter 
gapminder %>% 
  filter(year == 1957  ) %>% 
  arrange(desc(pop))

#create a new column in our dataset
gapminder %>% 
  filter(year == 2007 , country == 'China') %>%
  #mutate(lifeExp = lifeExp * 12) # existing column is modified
  #mutate(lifeExp * 12) # will generate a new cloumn with column as exp
  mutate(lifeExpMonths = lifeExp * 12) # new column as 'LifeExpMonth'
  

#load ggplot2
library(ggplot2)

gapminder_1952 <- gapminder %>% filter(year == 1952)

#scatter plot pop vs gdpPerCapita
ggplot(gapminder_1952,aes(x = pop, y = gdpPercap)) + geom_point()

#scatter plot pop vs LifeExpectency on log scale on both axis
ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) + 
    geom_point() + 
    scale_y_log10() + 
    scale_x_log10()

#scatter plot with colors on continents 
ggplot(gapminder_1952, aes(x= pop, y = lifeExp, color = continent)) + 
  geom_point() + 
  scale_x_log10() +
  scale_y_log10()

# scatter plot with colors on continents and 
# size of points based on gdp per capita

ggplot(gapminder_1952,aes(x=pop,y=lifeExp,color=continent,size=gdpPercap))+
  geom_point() +
  scale_x_log10() +
  scale_y_log10()


#scatter plot with faceting 
ggplot(gapminder_1952, aes(x = pop,y = lifeExp)) +
  geom_point() +
  scale_x_log10() + 
  scale_y_log10() +
  facet_wrap(~continent)

ggplot(gapminder, aes(x = gdpPercap,y = lifeExp, color = continent, size = pop)) +
  geom_point() +
  scale_x_log10() + 
  #scale_y_log10() +
  facet_wrap(~year)



#summarize the columns
by_year <- gapminder %>% 
  group_by( year ) %>%
  summarise(medianLifeExp = median(lifeExp),maxGdp = max(gdpPercap))

#plot the summarize data
ggplot(by_year, aes(x=year,y=medianLifeExp)) + 
  geom_point() + 
  expand_limits( y = 0)


#visualize median GDP per capital per continent over time 
by_year_continent <- gapminder %>% 
                      group_by(continent,year) %>%
                      summarize(medianGdp = median(gdpPercap))

ggplot(by_year_continent, aes(x=year, y = medianGdp,color = continent)) +
  geom_point() +
  expand_limits(y = 0)


#Compare Median Life Exp vs Median Gdp per continent for year 2007
gapminder_2007 <- gapminder %>%
                    filter(year == 2007) %>%
                    group_by(continent) %>%
                    summarize(medianLifExp = median(lifeExp), 
                              medianGDP = median(gdpPercap))

ggplot(gapminder_2007, aes(x=medianLifExp, y = medianGDP, colour = continent)) +
  geom_point() 

#linePLot
ggplot(gapminder_2007, aes(x=medianLifExp, y = medianGDP)) +
  geom_line() 


#visualize median GDP per capital by continent for 1952 using bar graph
by_gdp_cap_continent <- gapminder %>%
                          filter(year == 1952) %>%
                          group_by(continent) %>%
                          summarise(medianGDP = median(gdpPercap))

ggplot(by_gdp_cap_continent, aes(x=continent,y = medianGDP)) +
  geom_col()

#Histogram
gapminder_1952_pop <- gapminder %>% filter(year == 1952) %>%
  mutate(pop_by_mil = pop / 1000000)

ggplot(gapminder_1952_pop,aes(x=pop_by_mil)) + geom_histogram() +
  scale_x_log10()

#boxplot
ggplot(gapminder_1952,aes(x= continent,y = lifeExp)) + geom_boxplot()

#add a title to the graphy
#Continent Vs Life
ggplot(gapminder_1952,aes(x= continent,y = lifeExp)) + geom_boxplot() +
  ggtitle('Continent Vs Life')

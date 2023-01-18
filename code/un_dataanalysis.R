library(tidyverse)
getwd()
setwd("D:/R-workshop/un-repor/un-report")
gapminder_data<-read.csv("data/gapminder_data.csv")

summarise(gapminder_data, mean(lifeExp))
summarise(gapminder_data, averagellifeExp=mean(lifeExp), medianlifeExp=median(lifeExp))

# Learning to pipe
# pipe like reading a book in order

gapminder_summary<-gapminder_data%>%
  summarise(averagellifeExp=mean(lifeExp))

#Filtering
gapminder_summary_2007<-gapminder_data%>%
  filter(year==2007)%>%
  summarise(average=mean(lifeExp))
# == logical option 
#!= every year beside 2007,except 2007

# Finding average gdpPercap for the first year in the dataset
gapminder_data%>%
  filter(year==min(year))%>%
  summarise(average_GDP=mean(gdpPercap))
# use min(year) to find the first year

# Using group_by() make subset of data
# find statistic within every group.

gapminder_data%>%
  group_by(year)%>%
  summarise(average= mean(lifeExp))

gapminder_data%>%
  group_by(year, continent)%>%
  summarise(average= mean(lifeExp), 
            error=sd(lifeExp))
# Mutate function: expand the data ,build a third column based on the already two columns
gapminder_data%>%
  mutate(gdp=pop*gdpPercap)# get a new column name gdp

#mutate a new column which is population in millions:/
gapminder_data%>%
  mutate(popinMillions=pop/100000)
# mutate do not change the origin data just print in the console


# select function to select specfic columns
gapminder_data%>%
  select(pop,year)

gapminder_data%>%
  select(-continent, -pop)# - to remove the unwanted column, won't change the origin data because not assigned.

# reshape data, make the right format to let R understand
# Messy data: 1.U. 2.sequencer. 3.plate reader. 4.pictures/PDF file format. 5. Microsoft office

# change wide format into long format: every row has only a single observation.
# Pivot_wider function

gapminder_data%>%
  select(country, continent, year, lifeExp)%>%
  pivot_wider(names_from = year, values_from = lifeExp)%>%
  View()

# working with messy data

read.csv("co2-un-data.csv", skip=1)# use skip to get rid of the first row so R can recognize the column name correctly.

# specify columns
co2_emissions_dirty<-read.csv("co2-un-data.csv", skip=2,
         col.names = c("region","country","year","series","value","footnotes","source"))

co2_emissions_dirty

?read_csv
?read.csv

co2_emissions<-co2_emissions_dirty%>%
  select(country,year,series,value)%>%
  mutate(series= recode(series, 
  "Emissions (thousand metric tons of carbon dioxide)"="total emissions",
  "Emissions per capita (metric tons of carbon dioxide)"="per_capita_emissions"))%>%
  pivot_wider(names_from = series, values_from = value)%>%
  filter(year==2005)%>%
  select(-year)

# Filter for year 2005
# Select to remove the year column
# store as an object with a descriptive name

co2_2005<-co2_emissions_dirty%>%
  filter(year==2005)%>%
  select(-year)%>%
View()

# Bringing in 2007 Population data
gapminder_data_2007<-read.csv("data/gapminder_data.csv")%>%
  filter(year==2007)%>%
  select(country, pop, lifeExp, gdpPercap)


inner_join(co2_emissions, gapminder_data_2007, by="country")# by Country = Country or change one of the country to another, to keep them the same.
anti_join(co2_emissions, gapminder_data_2007, by ="country")# find rows that do not match with each other. use the first dataset as the critiera, the order matters in anti_join.
# the join function , the column should be common, which means be existing in both dataset.
anti_join(gapminder_data_2007, co2_emissions, by ="country")

full_join(co2_emissions, gapminder_data_2007)%>%
View()


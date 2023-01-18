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

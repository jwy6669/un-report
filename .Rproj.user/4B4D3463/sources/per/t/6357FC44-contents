gapminder_1997<-read.csv("gapminder_1997.csv")
ggplot(data = gapminder_1997)+
  aes(x=gdpPercap)+
  labs(x="GDP Per Capita")
?aes
ggplot(data=gapminder_1997)+
  aes(x=gdpPercap, y=lifeExp)+
  labs(x="GDP Per Capita")
ggplot(data=gapminder_1997)+
  aes(x=gdpPercap)+
  labs(x="GDP Per Capita")+
  aes(y=lifeExp)+
  labs(y="Life Expentancy (yrs)")+
  geom_point()+
  labs(title = "Do people in wealthy countries live longer?")+
  aes(color=continent)+
  scale_color_brewer(palette = "Set1")+
  aes(size=pop/1000000)+
  labs(size="Population(in millions)")+
  aes(shape=continent)

# Short handed ggplot
ggplot(data=gapminder_1997)+
       aes(x= gdpPercap,y=lifeExp,color=continent,
           shape=continent,size=pop)+
  labs(x="GDP Per Capita", y= "Life Expentancy(yrs)", title="Do people in wealthy countries live longer?",size="Population (in millions)")+
  geom_point()

# read in all of the data from gapminder(more years than 1997!)
gapminder_data<-read.csv("gapminder_data.csv")
View(gapminder_data)
dim(gapminder_data)
head(gapminder_data)
tail(gapminder_data)

# Challenge:predicting the output
ggplot(data=gapminder_data)+
aes(x=year,y=lifeExp,color=continent)+
geom_point()

ggplot(data=gapminder_data)+
  aes(x=year,y=lifeExp,color=continent,group=country)+
  geom_line()

#learn about the data
str(gapminder_data)

# challenge: boxplot, x=continent, y=life expectancy
ggplot(data=gapminder_data)+
  aes(x=continent,y=lifeExp)+
  geom_boxplot()

ggplot(data=gapminder_1997)+
  aes(x=continent,y=lifeExp)+
  geom_boxplot()

ggplot(data=gapminder_data)+
  aes(x=continent,y=lifeExp)+
  geom_violin()+
  geom_point()


ggplot(data=gapminder_1997)+
  aes(x=continent,y=lifeExp)+
  geom_violin()+
  geom_jitter()

ggplot(data=gapminder_1997)+
  aes(x=continent,y=lifeExp)+
  geom_jitter()+
  geom_violin()
# violin will cover the points, the order of plot code shows the presenting order

ggplot(data=gapminder_1997)+
  aes(x=continent,y=lifeExp)+
  geom_violin()+
  geom_jitter(aes(size=pop)) # aes only apply to one piece, here the popultion. this aes only apply to this geom_jitter, function within function.every geom has an aes


ggplot(data=gapminder_1997)+
  aes(x=continent,y=lifeExp)+
  geom_violin(color="pink")+# only the outline of the violin become pink
  geom_jitter(aes(size=pop))

ggplot(data=gapminder_1997)+
  aes(x=continent,y=lifeExp)+
  geom_violin(fill="pink",color="purple")+
  geom_jitter(aes(size=pop))

ggplot(data=gapminder_1997)+
  aes(x=continent,y=lifeExp,color=continent)+
  geom_violin()+# 
  geom_jitter(aes(size=pop))


# histgram
ggplot(gapminder_1997)+
  aes(x=lifeExp)+
  geom_histogram(binwidth = 20)#width of each column

ggplot(gapminder_1997)+
  aes(x=lifeExp)+
  geom_histogram(bins = 20)+
  theme_classic()


ggplot(gapminder_1997)+
  aes(x=lifeExp)+
  geom_histogram(bins = 20)+
  theme_bw() # cahnge the theme, fun to play with, change for what you want


install.packages("ggthemes")
?theme_prism

install.packages("ggprism")
library(ggprism)

# to ggsave a specific figure,two ways
lifeExp_hist_prism<-ggplot(gapminder_1997)+
  aes(x=lifeExp)+
  geom_histogram(bins = 20)+
  theme_prism()

ggsave(plot = lifeExp_hist_prism,
       filename = "cool_prism_plot.png",
       device = "png",
       width = 4,height = 4)

ggsave("cool_prism_plot2.png",lifeExp_hist_prism,device="png",
       width=4,height = 4)


ggplot(gapminder_1997)+
  aes(x= gdpPercap,y = lifeExp)+
  geom_point()+
  facet_wrap(vars(continent))

ggplot(gapminder_1997)+
  aes(x= gdpPercap,y = lifeExp)+
  geom_point()+
  facet_grid(rows=vars(continent))

ggsave("awesome_plot.jpg", width = 6, height = 4)
?ggsave

ggsave("awesome_plot.tiff",device = tiff, width = 6, height = 4)

ggsave("awesome_plot.tiff",device = tiff, width = 6, height = 4, units = "cm")

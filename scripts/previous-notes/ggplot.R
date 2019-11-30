library(tidyverse)

read_csv("Data/gapminder.csv")

gapminder <- read_csv("Data/gapminder.csv")

gapminder_1977 <- filter(gapminder, year == 1977)

glimpse(gapminder_1977)

# better have a quick view of the data to check the dataframe and the data type

str(gapminder_1977)

# another alternative : using structure(str) function

# ggplot2 is a attaching package in tidyverse

ggplot(data = gapminder_1977)

# first argument is data

# nothing in the plot

ggplot(data = gapminder_1977,
       mapping = aes(x = gdpPercap, 
                     y = lifeExp,
                     colours = continent,
                     size = pop))

# second argument is mapping, what variable to what aes

# now you have the x y axis, lable, nbr, but not colour and size yet

ggplot(data = gapminder_1977,
       mapping = aes(x = gdpPercap, 
                     y = lifeExp,
                     colour = continent,
                     size = pop)) +
  geom_point()

# add geom, it can take automatically the mapping within the ggplot function

ggplot(data = gapminder_1977,
       mapping = aes(x = gdpPercap, 
                     y = lifeExp,
                     colour = continent,
                     size = pop)) +
  geom_point() +
  scale_x_log10()

# scale the x axis if you want to change the default scale by R

# do not use pipe cuz it will take the data from the lef side 
# and use it as the first argument of the right side
# remember using + 

# Ex

ggplot(data = gapminder_1977,
       mapping = aes(x = lifeExp, 
                     y = gdpPercap,
                     colour = continent,
                     size = pop)) +
  geom_point()

# or

ggplot(data = gapminder_1977,
       mapping = aes(x = pop, 
                     y = lifeExp,
                     colour = continent,
                     size = gdpPercap)) +
  geom_point()

# or

ggplot(data = gapminder_1977,
       mapping = aes(x = country, 
                     y = lifeExp,
                     colour = continent,
                     size = gdpPercap)) +
  geom_point()

# or

ggplot(data = gapminder_1977,
       mapping = aes(x = continent, 
                     y = lifeExp,
                     colour = gdpPercap,
                     size = pop)) +
  geom_point()

# define the mapping directly in the geom function

ggplot(data = gapminder_1977) +
  geom_point(mapping = aes(x = gdpPercap, 
                           y = lifeExp,
                           colour = continent,
                           size = pop)) +
  scale_x_log10()

# same plot as before

?geom_point

# stroke (outside circle) works with fill

# alpha: transparency

# Ex : setting within geom_point function

ggplot(data = gapminder_1977,
       mapping = aes(x = gdpPercap, 
                     y = lifeExp,
                     colour = continent,
                     size = pop)) +
  geom_point(alpha = 0.5) +
  scale_x_log10()

# another try

ggplot(data = gapminder_1977,
       mapping = aes(x = gdpPercap, 
                     y = lifeExp,
                     colour = continent,
                     size = pop)) +
  geom_point(alpha = 0.5, size = 7) +
  scale_x_log10()

# another try

ggplot(data = gapminder_1977,
       mapping = aes(x = gdpPercap, 
                     y = lifeExp,
                     colour = continent)) +
  geom_point(alpha = 0.5, size = 7) +
  scale_x_log10()

# another try

ggplot(data = gapminder_1977,
       mapping = aes(x = gdpPercap, 
                     y = lifeExp,
                     colour = continent,
                     size = pop)) +
  geom_point(alpha = 0.5, colour = "red") +
  scale_x_log10()

# another try

ggplot(data = gapminder_1977,
       mapping = aes(x = gdpPercap, 
                     y = lifeExp,
                     size = pop)) +
  geom_point(alpha = 0.5, colour = "red") +
  scale_x_log10()

# Ex 

ggplot(data = gapminder,
       mapping = aes(x = year,
                     y = lifeExp,
                     size = pop,
                     colour = continent)) +
  geom_point(alpha = 0.3)

# using lines

ggplot(data = gapminder, 
       aes(x = year, 
           y = lifeExp, 
           group = country, 
           color = continent)) +
  geom_line()

# adding more than one geom

# both points and lines (by adding another layer)

ggplot(data = gapminder, 
       aes(x = year, 
           y = lifeExp, 
           group = country, 
           color = continent)) +
  geom_line() +
  geom_point()

# Ex

ggplot(data = gapminder, 
       aes(x = year, 
           y = lifeExp, 
           group = country, 
           color = continent)) +
  geom_line() +
  geom_point(colour = "black")

#

ggplot(data = gapminder, 
       aes(x = year, 
           y = lifeExp, 
           group = country)) +
  geom_point() +
  geom_line(aes(colour = continent))

# put it into the aes when mapping a valuable 

ggplot(data = gapminder, 
       aes(x = year, 
           y = lifeExp, 
           group = country)) +
    geom_line(aes(colour = continent)) +
  geom_point()

# the order of the geom_oint and geom_line determin the order of the layers

# transformation and statistics

ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point(alpha = 0.3) +
  scale_x_log10() +
geom_smooth() # add trendline using default method

ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point(alpha = 0.3) +
  scale_x_log10() +
  geom_smooth(method = "lm", size = 1.5) # choose a method and change the thickness

# change the scale

ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(colour = continent)) + # defaut colour 
  scale_x_log10() 

# change the colour 

ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(colour = continent)) + 
  scale_x_log10() +
  scale_colour_manual(values = c("red", "green", "purple", "blue", "black"))

# assign a colour to a continent

ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(colour = continent)) + 
  scale_x_log10() +
  scale_colour_manual(values = c(Europe = "red", Africa = "green", Oceania = "purple", 
                                 Asia = "blue", 
                                 Americas = "black"))

# Ex 9

ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(size = 3, colour = "red") + 
  scale_x_log10() + 
  geom_smooth(method = "lm", size = 1.5)

# shading is the std error of the trendlines

# Ex 10

ggplot(data = gapminder, aes(x = gdpPercap, 
                             y = lifeExp, 
                             colour = continent, 
                             shape = continent)) +
  geom_point() +
  scale_x_log10() + 
  geom_smooth(method = "lm", size = 1)

# do not have to redefine the colour of the trendline again within the last line

# Ex 11

ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() + 
  scale_x_log10() +
  scale_colour_manual(values = c("red", "green", "blue", "purple", "black"))


# try the color() function

colours()


# try the scale_colour_brewer() function

ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() + 
  scale_x_log10() +
  scale_colour_brewer(palette = "RdBu")

# define the shape

ggplot(data = gapminder, aes(x = gdpPercap, 
                             y = lifeExp, 
                             colour = continent, 
                             shape = continent)) +
  geom_point() +
  scale_x_log10()+
  scale_shape_manual(values = c(7, 8, 9, 10, 18))

# be careful, in a vector, should be just one data type, do not mix the chr with dbl

# separating figures

a_countries <- gapminder %>% 
  filter(str_starts(country, "A")) # string function

ggplot(data = a_countries, 
       mapping = aes(x = year, 
                     y = lifeExp, 
                     colour = continent, 
                     group = country)
       ) +
  geom_line() +
  facet_wrap(~country) # break our figure by country

# facet_wrap doesn't change the underline, the mapping...
# just breaking up into separate panels

# Ex

ggplot(data = gapminder,
       mapping = aes(x = gdpPercap, 
                     y = lifeExp,
                     colour = continent, # colour without s
                     size = pop)) +
  geom_point() +
  scale_x_log10() +
  facet_wrap(~year)
  
# geom_text

ggplot(data = gapminder_1977,
       mapping = aes(x = gdpPercap, 
                     y = lifeExp,
                     colour = continent,
                     size = pop,
                     label = country)) + # add a new mapping
  geom_point() +
  scale_x_log10() +
  geom_text() # remember mapping the label to tell it what to give a text

#

gapminder_rich <- filter(gapminder_1977, gdpPercap > 30000) # 30000 is the real value,
# not the log10, log10 is just for the scale

ggplot(data = gapminder_1977,
       mapping = aes(x = gdpPercap, 
                     y = lifeExp,
                     colour = continent,
                     size = pop,
                     label = country)) + 
  geom_point() +
  scale_x_log10() +
  geom_text(data = gapminder_rich) # define which data to give the text

# ggplot can be saved as valuable and be used later on 

rough_plot <- ggplot(data = a_countries, 
       mapping = aes(x = year, 
                     y = lifeExp, 
                     colour = continent, 
                     group = country)
) +
  geom_line() +
  facet_wrap(~country)

rough_plot + 
  scale_color_brewer(palette = "Dark2") # add a new layer to the plot

# add the theme (default these grey)

rough_plot +
  labs(title = "life expectancy overtime for 'A' countries",
       caption = "Data from Gapminder", # add the caption
       x = "Year",
       y = "Life expectancy",
       colour = "Continent")

# add a new theme

rough_plot +
  theme_bw() # black and white

rough_plot +
  theme_linedraw() # for contrast

# theme function allow you to set things one by one

rough_plot +
  labs(title = "life expectancy overtime for 'A' countries",
       caption = "Data from Gapminder", # add the caption
       x = "Year",
       y = "Life expectancy",
       colour = "Continent") +
  theme(
    panel.grid.major = element_blank(), # remove the grid lines
    plot.title = element_text(size = 14), # change the size of the title
    axis.line = element_line(colour = "blue", size = 4)
    )

# save the plot

ggsave("figures/my_first_plot.png", plot = rough_plot) 

# the only thing you should tell is the name of the file
# save it as .png
# by default, it will save the last plot that you've run

ggsave("figures/my_first_plot.jpg", 
       plot = rough_plot, 
       width = 12, 
       height = 10, 
       units = "cm") # change the size of the figure
# resolution can be defined by dpi = 
# getwd() to see the current working directory
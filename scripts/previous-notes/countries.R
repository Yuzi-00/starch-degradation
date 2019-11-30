library(tidyverse) 
#Load the package, should do this everytime we open a new script

read_csv("Data/gapminder.csv") 
#Load the data, chr: charactor, 
#dbl(double): another way that computer recongnise as nbrs

gapminder <- read_csv("Data/gapminder.csv") 
#read the function "read_csv" from the gapminder folder
#In Environment: 1704 raws with 6 cols

nrow(gapminder) 
#Show the nbr of raws, no"" here cuz this is a valubale 

ncol(gapminder) #show the nbr of cols

colnames(gapminder) #show the names of the cols

glimpse(gapminder) #summary of the data, an overview

#int: integer
#ord: ordered factor, the data have a implied order

#a vector can just contain a single type of data

select(gapminder, year, country, pop) 
#select these 3 cols of data and give us a new dataframe

select(gapminder, 3, 1, 5) #replace the name of cols by nbrs

select(gapminder, 3:5) #cols 3, 4, 5 (3:5 is a vector)

select(gapminder, -lifeExp, -pop) 
#remove lifeExp and pop from the dataframe

select(gapminder, year:lifeExp)
select(gapminder, -(year:lifeExp))

just_population <- select(gapminder, year, country, pop)
#save saparately a new dataframe 
#(you'll find it under Environment panel)

just_population

select(gapminder, country, year, pop, gdpPercap)
select(gapminder, 1, 3, 5, 6)
select(gapminder, -2, -4)

?select #get help about the function

select(gapminder, starts_with("co")) #select cols contain "co"

select(gapminder, contains("e")) #select cols contain "e"

select(gapminder, contains("p"))

select(gapminder, ends_with("p"))

select(gapminder, population = pop, country) 
#select two cols with pop renamed

rename(gapminder, population = pop) 
#keep all the cols and just rename the pop col
#DO NOT change the order of population and pop

filter(gapminder, country == "Australia")
#select all the raws of AUS

filter(gapminder, year >= 1997)
#select all the raws of >= 1997

filter(gapminder, lifeExp >= 80)
filter(gapminder, lifeExp >= 80, continent == "Europe")
filter(gapminder, lifeExp >= 80, gdpPercap >= 30000)

filter(gapminder, lifeExp >= 80 | gdpPercap >= 30000)
#either healthy or rich

filter(gapminder, continent %in% c("Europe", "Africa", "Asia"))
#Extract the rows from all countries in Africa, Asia, or Europe

#20/09/2019

gapminder_ver2 <- select(gapminder, country, year, pop)
gapminder_ver3 <- filter(gapminder_ver2, country == "Australia")

select(filter(gapminder, country == "Australia"), country, year, pop)
#U can insert a function into another function

filter(select(gapminder, country, year, pop), country == "Australia")

select(gapminder, year, pop)

gapminder %>% select(year, pop)
#Pipe the gapminder data into the select function
#This is just an alternative to do what we did before

filter(gapminder, country == "Australia", year >= 1997)
#inspect within the col "year" and give us all the raws >= 1997

filter(gapminder, country == "Australia", year)
#Wrong command, can not select Australia and choose all the year col

small <- gapminder %>% 
  filter(country == "Australia", year >= 1997)

gapminder %>% 
  filter(country == "Australia") %>% 
  select(country, pop, year)
#Use %>% to pipe the data from left side to the right side function

gapminder_ver4 <- gapminder %>% 
  filter(country == "Australia") %>% 
  select(country, pop, year) %>% 
  rename(population = pop)

#mutate: creat new cols

mutate(gapminder, gdp = gdpPercap * pop)
#creat a new col called gdp 
#and the value is created by taking the value of gdpPercap*pop
with_gdp <- mutate(gapminder, gdp = gdpPercap * pop)

mutate(gapminder, popinM = pop/1000000)
mutate(gapminder, popinM = pop/1e6)
#creat a new col with population in 1000000 

mutate(gapminder, log_of_pop = log(pop))
#creat a new col with the log of pop

str_sub("A long bit of text", start = 1, end = 5)
#short for string subset, creat sth. for the first to the 5th chr

mutate(gapminder, country_abbr = str_sub(country, start = 1, end = 3))
#let str_sub to look into the col of country and replace it by country_abbr
#And all the names of the country will be just 3 chrs

str_length("some words")
#Counts nbr of the chr in the ""

mutate(gapminder, nbr_chr_country = str_length(country))
#creat a new col with the nbr of chr in a country's name
#!!!if it's a defined variale, do not use ""
#if you don't give a name to the col, it'll be named as the name of the function

mutate(gapminder, gdp = gdpPercap * pop, log_of_pop = log(pop))
#creat a few cols at the same time

mutate(
  gapminder,
  gdp = gdpPercap * pop,
  log_of_pop = log(pop),
  log_of_gdp = log(gdp)
)
#make it easier to see
#mutate can do the calculation with the new col freshly created within mutate function



mutate(
  gapminder,
  lifeExp_days = lifeExp * 365,
  gdp = gdpPercap * pop,
  gdp_M = gdp / 1e9
)
#Exercice: creat new cols

#Summarise data

summarise(gapminder, mean_life_exp = mean(lifeExp))
#mean() is a function to caculate the mean value
#This creat also a dataframe, which means we can take and save it in order to work on it later

summarise(
  gapminder, 
  mean_life_exp = mean(lifeExp), 
  sd_life_exp = sd(lifeExp), 
  biggest_gdp = max(gdpPercap)
)
#creat multiple summarise
#sd(): std Deviation -> how disperse these data are

#Ex: to summarise: mean and the median of the pop
summarise(
  gapminder,
  mean_pop = mean(pop),
  median_pop = median(pop)
)

summarise_if(gapminder, is.numeric, mean)
#if + is numeric -> all cols which are nbr

by_country <- group_by(gapminder, country)
#group by country -> tell us there are 142 countries 
by_country

summarise(by_country, mean_pop = mean(pop))
#Mean values calculated for diff countries 

#Ex: groupe by continent and calculate the mean the median
by_continent <- group_by(gapminder, continent)
summarise(
  by_continent,
  mean_pop = mean(pop),
  median_pop = median(pop)
)

gapminder %>% 
  group_by(continent) %>% 
  summarise(mean_pop = mean(pop), median_pop = median(pop))
#Use a pipe, do the right side thing by using the data from the left side

#arrange

arrange(gapminder, gdpPercap)
#sending ordered by gdpPercap

arrange(gapminder, desc(gdpPercap))
#Desending ordered

#Ex : find the countries which have the max and the min life exp 
#get gapminder data
#group data be country
#mean life exp by group
#order result
gapminder %>% 
  group_by(country) %>% 
  summarise(mean_life_exp = mean(lifeExp)) %>% 
  arrange(mean_life_exp) %>% 
  filter(mean_life_exp == min(mean_life_exp))

#another way to do
gapminder %>% 
  group_by(country) %>% 
  summarise(mean_life_exp = mean(lifeExp)) %>% 
  filter(mean_life_exp == min(mean_life_exp) | 
           mean_life_exp == max(mean_life_exp))

#another way to do
LifeExp <- summarise(by_country, mean_life_exp = mean(lifeExp))
arrange(LifeExp, mean_life_exp)
arrange(LifeExp, desc(mean_life_exp))

summarise(gapminder, num_rows = n())
#n: a helper function
#summarise the nbr of the rows

counts <- summarise(by_country, num_rows = n())
#how many rows of data we have for each country

View(counts)
#open another window to view "counts"

View(summarise(by_country, num_rows = n()))


# read in excel files 

read_excel("Data/gapminder.xlsx")
#error: read_excel function comes from a diff package
#it is not in the tidyverse package

library(readxl)

gapminder_excel <- read_excel("Data/gapminder.xlsx")
#Wokrs now

read_excel("Data/gapminder.xlsx", range = "A1:E4")
#read just a part of the data, cell from A1 to E4

read_excel("data/gapminder.xlsx", sheet = "gapminder")
#choose a sheet, if don't choose, R will just select the first sheet

# writing data out

write_csv(gapminder_excel, "results/gapminder_output.csv")
#write_csv(dataframe, "path(put it into one of the files and give it a name)")
#Do not forget .csv (csv is the best choice)

#Ex: writting out just the australia data from gapminder

Australia <- gapminder %>% 
  filter(country == "Australia")
write_csv(Australia, "results/AUS_output.csv")

#another way to do
gapminder %>% 
  filter(country == "Australia") %>%
  write_csv("results/Australia_output.csv")

#Ex:what steps might you need to determine which countries 
#are in the top ten life expectancy lists for both 1987 
#and 2007?

#select the col of life exp, year and country
#select rows containing 1987 and 2007
#order result

result <- gapminder %>% 
  select(country, year, lifeExp) %>% 
  filter(year == 1987 | year == 2007) %>% 
  arrange(desc(lifeExp))

#choose the first 10 rows with the function head()

gapminder %>% 
  select(country, year, lifeExp) %>% 
  filter(year == 1987 | year == 2007) %>% 
  arrange(desc(lifeExp)) %>% 
  head(n=10)

#right answer
#start with gapminder
#just work with data from 1987 and from 2007
#sort our data by life exp (by year)
#get just the top 10 (by year): top_n()
#count the nbr of times a country appears:group_by and then summarising with n
#just keep the countries that appear twice: filter
gapminder %>%
  filter(year == 1987 | year == 2007) %>%
  group_by(year) %>% 
  arrange(desc(lifeExp)) %>%
  top_n(10) %>% 
  group_by(country) %>% 
  summarise(country_app=n()) %>% 
  filter(country_app == 2)

#Exercise:Using your gapminder data set, filter it to 1957 data only
#Summarise your new 1957 data frame to find the maximum gdp per cap, by continent

gapminder %>%
  filter(year == 1957) %>% 
  group_by(continent) %>% 
  summarise(max_gpdPercap = max(gdpPercap))

gapminder <- read_csv("data/gapminder.csv") # store gapminder data in a variable
gapminder%>% # calls gapminder
  filter(year==1957)%>% # filter to year 1957
  group_by(continent)%>% #groups by continent
  summarise(max_gdpPercap = max(gdpPercap)) %>% #return maximum gdp per continent
  arrange(desc(max_gdpPercap)) # arrange in descending order

# == when we want to compare things
#source: run all the codes in the script

?gather

gapminder_2012 <- read_csv(file="data/gapminder_2012.csv")
gapminder_2012

#combining data using bind_rows()

?bind_rows #find the examples
gapminder_extra <- bind_rows(gapminder, gapminder_2012) 
#works just when the cols has the same numbers and the names 
gapminder_extra

renamed_2012 <- rename(gapminder_2012, population = pop) #rename the cols
mismatched_names <- bind_rows(gapminder, renamed_2012)
mismatched_names
tail(mismatched_names)
#because gapminder don't have population and gapminder_2012 doesn't have pop
#So NA appears in both pop and population

#inner_join takes just the part of data which is the same
#full_join keeps all the data
#left_join move the right things into the left 
#(retain all the data on the left, and merge in just the same type of data on the right)

# joins lesson

#vector:a collection of data all of the same type which can be created by c function
#c is short for combine

example_vector <- c(1,4,2,7)
example_vector

string_vector <- c('hello', 'this', 'is', 'a', 'vector')
string_vector
example_vector[3] # choose the 3rd one within this vector
broken_vector <- c('hello', 2)
broken_vector # R transferd 2 to a text

# always the same data type in each col
# tidydata : each valuable has its own col, each observation has its own row

df1 <- tibble(sample = c(1,2,3), measure1 = c(4.2, 5.3, 6.1))
#creat a tibble with 2 cols
df1

df2 <- tibble(sample = c(1,3,4), measure2 = c(7.8, 6.4, 9.0))
df2

inner_join(df1, df2)
full_join(df1, df2)
left_join(df1, df2)
# join by sample cuz they have the same name

#case with different names 
df3 <- tibble(ID = c(1,2,4), measure3 = c(4.7, 34, 2.6))
df3
full_join(df1, df3) # wrong code
full_join(df1, df3, by = c("sample" = "ID", "measure1" = "measure3")) 
# tell the computer the sample is the same as ID
full_join(df1, df2, by = c("sample"))

df2 <-  tibble(sample=c(1,2,5), measure2=c(3.3,4,20))
df3 <- tibble(id=c(1,2,5), measure2=c(3.3,4,20), measure4=c(56,7,4))
full_join(df2,df3, by =c("sample"="id"))

df2 <-  tibble(sample=c(1,2,5), measure2=c(3.3,4,20))
df3 <- tibble(id=c(1,2,5), measure2=c(3.3,5,20), measure4=c(56,7,4))
full_join(df2,df3, by =c("sample"="id", "measure2"))
# otherwise R will think the measures2 of the df2 and df3 are different

# reshaping data

cows <- tibble(id = c(1, 2, 3),
               weight1 = c(203, 227, 193),
               weight2 = c(365, 344, 329))
cows

cows_tidy <- gather(cows, rep, weight, -id)
# rep : where to store the col names 
# weight : where to store the data values 
# without changing the id col
cows_tidy

#another alternative
gather(cows, key = rep, value = weight, -id)

# gather(dataframe, where_to_store_col_names, where_to_store_values, what_to_gather)

cows_tidy %>% 
  arrange(id)
# arrange by id -> more human readable...hhh

spread(cows_tidy, rep, weight)
# tell the function "spread" the cols where we need to get the data

# another alternative using key and value
spread(cows_tidy, key = rep, value = weight)

# Ex: gather
table4a

table4a_tidy <- gather(table4a, year, number, -country)
table4a_tidy %>% 
  arrange(country)

# an althernative for -country
table4a_tidy <- gather(table4a, year, number, 2:3)

#Ex : spread

table4a_spread <- spread(table4a_tidy, year, number)
table4a_spread

#gather will only creat two cols 
# -> find one block of col and turnning it into two new cols

# spread(dataset, which_col_to_create_new_col_names, which_col_to_get_the_value)

#Ex 
table2

#col names of spread dataset : 1999, 2000

spread(table2, type, count)

# separate function

cows_with_breed <- cows %>% mutate(id = c("1_A", "2_A", "3_B"))
cows_with_breed %>% 
  separate(col = id, into = c("ID", "breed"))
# separate the id col into two cols : ID and breed

separate(cows_with_breed, col = id, into = c("ID", "breed"), sep = "_")
#use F1 for help (like ?)
#computer will separate when anything is not a letter or a 

#another alternative
separate(cows_with_breed, id, c("ID", "breed"), "_")


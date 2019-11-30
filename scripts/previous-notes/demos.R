1+100

3+5*2  
3+5*2^2
2/10000
5e3
5e-3 #demonstrating scientific notation

#equality

1==1
1!=2 # not equal to
1!=1

1 < 2
1 > 10
2 <= 5
2 <= 2

pi == 3.141593

#store value and use it later

x <- 1/40 
x
y <- 1000
y = 500

x + y

rm(x) # remove the assignment of x

y <- 10
log(x)

x <- x + 1
x <- NA # missing value, should be CAPITAL NA

x == NA
is.na(x) #is x equals to NA

my_data <- 1
my_data

min_height <- 1
max.height <- 2
_age <- 1 #can't start a name with _
.mass <- 1
MaxLength <- 1
min-length <- 1 #min is already a function name
2widths <- 1 #don't start with a number
celsius2kelvin <- 1
.mass <- 2 #creat a hidden variable
min_length <- 1

1:5 #sequence of nbrs, a vector in R, in a particular order
2^(1:5) #2 to the power of each value in the vector

my_vector <- c(4,2,7,2,3)
my_vector
2^my_vector

ls() #show all the valuables
ls #all the R code how the function beheaves
rm(max.height)
ls()
installed.packages() #review of all the packages 
installed.packages("tidyverse")

read.csv("Data/gapminder.csv") 
#read a csv file, #read_csv if a function from tidyverse



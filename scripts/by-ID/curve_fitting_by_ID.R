library(tidyverse)

library(broom)

# read in the dataset

data_6P <- read_csv("data/tidydata/data_6P_cal_3nd.csv")


###################################### extrat parameters ############################################


# built the model and put it into the loop

find_paramters <- function(data_6P){ # creat a function called find_parameters
  
  hydro_data <- data_6P %>% 
    filter(!is.na(HE)) %>% # remove the missing value
    filter(!(Plate == 1 & Sample == "208" & Time == 20)) 
  # remove this point as it's contaminated, otherwise it will fail our modelling later on 

  model <- nls(formula = HE ~ Xinf*(1-exp(-k*Time**(1-h))), 
                                          data = hydro_data,
                                          start = list(Xinf = 100,
                                                       k = 0.001,
                                                       h = 0.001))
# built the model with some guessing values of the unknown parameters

result <- tidy(model) %>% # turn the results into a tidy tibble
  select(term, estimate) %>% # extract the values of these pamameters
  spread(term, estimate) # split into three separate columns
  result # print out the result
}

parameters <- data_6P %>% 
  split(.$Sample) %>% # split the initial dataset into 182 subsets by sample
  map_df(find_paramters) 
# the map function transform its input by applying the function find_parameters 
# and save the outputs all together into a list
# while the map_df function will do the same but save the outputs all together into a data frame

# if we want to check the type of the data 

class(parameters) # we can see that it's a data frame (a tibble)

# add the sample name to the parameter list 

sample_name <- data_6P %>% 
  filter(!is.na(HE)) %>% # remove the missing value
  filter(!(Plate == 1 & Sample == "208" & Time == 20)) %>% # remove this sample with the same reason as before
  select(Sample) %>% # select just the sample column
  unique() %>% # remove the duplicated sample names
  arrange(Sample) # get the same order as the output of the split(.$Sample) step

# combine the fitted parameters with their sample names (by order)

fitted_parameters <- bind_cols(sample_name, parameters)

# save the fitted parameters into the analysis folder 

write_csv(fitted_parameters, "analysis/fitted_parameters.csv")

################################ goodness of the model #############################################

find_goodness <- function(data_6P){ # creat a function called find_goodness
  
  hydro_data <- data_6P %>% 
    filter(!is.na(HE)) %>% # remove the missing value
    filter(!(Plate == 1 & Sample == "208" & Time == 20)) 
  # remove this point as it's contaminated, otherwise it will fail our modelling later on 
  
  model <- nls(formula = HE ~ Xinf*(1-exp(-k*Time**(1-h))), 
               data = hydro_data,
               start = list(Xinf = 100,
                            k = 0.001,
                            h = 0.001))
  # built the model with some guessing values of the unknown parameters
  
  result <- glance(model) # get the overview of the fitting
  result # print out the result
}

goodness_model <- data_6P %>% 
  split(.$Sample) %>% # split the initial dataset into 182 subsets by sample
  map_df(find_goodness) # transform the input (splitted data_6P) into the function find_goodness
# and save the output into a single data frame

# save the output under the analysis file

write_csv(goodness_model, "analysis/goodness_fitting.csv")

# check the residuals of the model

plot(residuals(model))


######################################## plot ######################################################


# firstly try it with a single sample, let's say : sample 92

# filter the data frame to have just the sample 92

sample_92 <- data_6P %>% 
  filter(Sample == "92") %>% 
  filter(!is.na(HE)) # remove the missing value

model_92 <- nls(formula = HE ~ Xinf*(1-exp(-k*Time**(1-h))), 
             data = sample_92,
             start = list(Xinf = 100,
                          k = 0.001,
                          h = 0.001))

summary(model_92) # take a look at the fitting result (will give you RSE)

glance(model_92) # another way to check the goodness (will give you sigma)

# plot the mesuring values

sample_92 %>% 
  select(Time, HE) %>% 
  plot(ylim = c(0, 100)) # set the range of the y axis up to 100

# plot the fitted line

## creat a data frame that contains 100 time points

 new.data <- data.frame(Time = seq(min(sample_92$Time), max(sample_92$Time), len = 100))

## draw the new line (fitted line)
 
 lines(new.data$Time, predict(model_92, newdata = new.data), col='red')
 
 
## check the residuals
 
 plot(residuals(model_92)) # it seems that there is a pattern there ?
 
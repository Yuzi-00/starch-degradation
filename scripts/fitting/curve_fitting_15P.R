

#                                       **  Using the Weibull function  **  

#           ** using three replicates separately to generate one single h k and Xinf for each sample **


library(tidyverse)

library(broom)


#                                             ** modelling with control samples ** 


# read in the dataset

data_15P_cal_HE_outlier_replaced <- read_csv("data/tidydata/data_15P_cal_HE_outlier_replaced.csv")

# built the model and put it into the loop

find_paramters <- function(data_15P_cal_HE_outlier_replaced){ # creat a function called find_parameters
  
  # remove the missing value
  
  hydro_data <- data_15P_cal_HE_outlier_replaced %>% 
    filter(!is.na(HE)) 
  
    # built the model with some guessing values of the unknown parameters
    
  model <- nls(formula = HE ~ Xinf*(1-exp(-k*Time**(1-h))), # using the Weibull function 
               data = hydro_data,
               start = list(Xinf = 100,
                            k = 0.001,
                            h = 0.001))
  
  # turn the results into a tidy tibble
  
  result <- tidy(model) %>% # the tidy function comes from the broom package
    select(term, estimate) %>% # extract the values of these pamameters
    spread(term, estimate) # split into three separate columns
  result # print out the result
}

# extract the parameters 

parameters <- hydro_data %>% 
  split(.$Sample) %>% # split the initial dataset into 182 subsets by sample
  map_df(find_paramters) 
# the map function transform its input by applying the function find_parameters 
# and save the outputs all together into a list
# while the map_df function will do the same but save the outputs all together into a data frame

# if we want to check the type of the data 

class(parameters) # we can see that it's a data frame (a tibble)

# check the model 

plot(residuals(model)) # this gives us the residuals in order, don't use this one

plot(model) # this is the residuals against fitted values, the variance is quite big
# let's remove the control samples and see if it's better


#                                             ** modelling without control samples ** 


# remove the control samples 

data_control_removed <- data_15P_cal_HE_outlier_replaced %>% 
  filter(Sample != "C+" & Sample != "C-")

# built the model and put it into the loop

find_paramters <- function(data_control_removed){ # creat a function called find_parameters
  
  # remove the missing value
  
  hydro_data <- data_control_removed %>% 
    filter(!is.na(HE)) 
  
  # built the model with some guessing values of the unknown parameters
  
  model <- nls(formula = HE ~ Xinf*(1-exp(-k*Time**(1-h))), # using the Weibull function 
               data = hydro_data,
               start = list(Xinf = 100,
                            k = 0.001,
                            h = 0.001))
  
  # turn the results into a tidy tibble
  
  result <- tidy(model) %>% # the tidy function comes from the broom package
    select(term, estimate) %>% # extract the values of these pamameters
    spread(term, estimate) # split into three separate columns
  result # print out the result
}

# extract the parameters 

parameters <- hydro_data %>% 
  split(.$Sample) %>% # split the initial dataset into 182 subsets by sample
  map_df(find_paramters) 
# the map function transform its input by applying the function find_parameters 
# and save the outputs all together into a list
# while the map_df function will do the same but save the outputs all together into a data frame

# if we want to check the type of the data 

class(parameters) # we can see that it's a data frame (a tibble)

# check the model 

plot(residuals(model)) # this gives us the residuals in order, don't use this one

plot(model) # the variance is reduced, but still...

# have to find the two points below -5 according to the plot

# add a column for residuals 

hydro_data$residuals = (residuals(model))

# find the minimum value of the residuals

which.min(hydro_data$residuals) # it's the 259

# find the corresponding sample 

hydro_data$Sample[259] # it's the sample 92, which is not surprising 


#                                             ** modelling without sample 92 ** 


# remove the control samples 

data_92_removed <- data_control_removed %>% 
  filter(Sample != "92")

# built the model and put it into the loop

find_paramters <- function(data_92_removed){ # creat a function called find_parameters
  
  # remove the missing value
  
  hydro_data <- data_92_removed %>% 
    filter(!is.na(HE)) 
  
  # built the model with some guessing values of the unknown parameters
  
  model <- nls(formula = HE ~ Xinf*(1-exp(-k*Time**(1-h))), # using the Weibull function 
               data = hydro_data,
               start = list(Xinf = 100,
                            k = 0.001,
                            h = 0.001))
  
  # turn the results into a tidy tibble
  
  result <- tidy(model) %>% # the tidy function comes from the broom package
    select(term, estimate) %>% # extract the values of these pamameters
    spread(term, estimate) # split into three separate columns
  result # print out the result
}

# extract the parameters 

parameters <- hydro_data %>% 
  split(.$Sample) %>% # split the initial dataset into 182 subsets by sample
  map_df(find_paramters) 
# the map function transform its input by applying the function find_parameters 
# and save the outputs all together into a list
# while the map_df function will do the same but save the outputs all together into a data frame

# if we want to check the type of the data 

class(parameters) # we can see that it's a data frame (a tibble)

# check the model 

plot(residuals(model)) # this gives us the residuals in order, don't use this one

plot(model) # the variance is reduced, but still...

# have to find the two points below -5 according to the plot

# add a column for residuals 

hydro_data$residuals = (residuals(model))

# find the minimum value of the residuals

which.min(hydro_data$residuals) # it's the 259

# find the corresponding sample 

hydro_data$Sample[259] # it's the sample 92, which is not surprising 


#                    ** add the sample name to the parameter list ** 


# extract the subset

sample_name <- hydro_data %>% 
  select(Sample) %>% # select just the sample column
  unique() %>% # remove the duplicated sample names
  arrange(Sample) # get the same order as the output of the split(.$Sample) step

# combine the fitted parameters with their sample names (by order)

fitted_parameters <- bind_cols(sample_name, parameters)

# save the fitted parameters into the analysis folder 

write_csv(fitted_parameters, "analysis/fitted_Weibull_parameters_15P.csv")

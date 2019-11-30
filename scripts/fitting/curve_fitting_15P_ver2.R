

#                                       **  Using the Weibull function  **  

#           ** using three replicates separately to generate three h k and Xinf (for 3 replicates) for each sample **

library(tidyverse)

library(broom)

# read in the dataset

data_15P <- read_csv("data/tidydata/data_15P_outlier_deleted.csv")

# add a column that distinguish each sample

data_15P <- data_15P %>% 
  mutate(Well = paste(Plate, Row, Column, sep = "_")) 

###################################### extrat parameters ############################################

# built the model and put it into the loop

find_paramters <- function(data_15P){ # creat a function called find_parameters
  
  # remove the missing value
  
  hydro_data <- data_15P %>% 
    filter(!is.na(HE)) 
  
  # built the model with some guessing values of the unknown parameters
  
  model <- nls(formula = HE ~ Xinf*(1-exp(-k*Time**(1-h))), # using the Weibull function 
               data = hydro_data,
               start = list(Xinf = 73,
                            k = 0.003,
                            h = 0.000005))
  
  # turn the results into a tidy tibble
  
  result <- tidy(model) %>% 
    select(term, estimate) %>% # extract the values of these pamameters
    spread(term, estimate) # split into three separate columns
  result # print out the result
}

# extract the parameters 

parameters <- hydro_data %>% 
  split(.$Well) %>% # split the initial dataset into 182 subsets by sample
  map_df(find_paramters) 
# the map function transform its input by applying the function find_parameters 
# and save the outputs all together into a list
# while the map_df function will do the same but save the outputs all together into a data frame

# if we want to check the type of the data 

class(parameters) # we can see that it's a data frame (a tibble)

################################## add the sample name to the parameter list ####################################################

# extract the subset

sample_name <- hydro_data %>% 
  select(Sample) %>% # select just the sample column
  unique() %>% # remove the duplicated sample names
  arrange(Sample) # get the same order as the output of the split(.$Sample) step

# combine the fitted parameters with their sample names (by order)

fitted_parameters <- bind_cols(sample_name, parameters)

# save the fitted parameters into the analysis folder 

write_csv(fitted_parameters, "analysis/fitted_Weibull_parameters_15P.csv")

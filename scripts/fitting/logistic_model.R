#                                       **  Using the logistic model  **  

#           ** using three replicates separately to generate three h k and Xinf (for 3 replicates) for each sample **

library(tidyverse)

library(broom)

# read in the dataset

data_15P_cal_HE_outlier_replaced <- read_csv("data/tidydata/data_15P_cal_HE_outlier_replaced.csv")



#                                             ** modelling with control samples **


# let's use two functions to generate the fitted model and the estimated parameters

fit_model <- function(data_15P_cal_HE_outlier_replaced){ # creat a function called fit_model
  
  # remove the missing value
  
  hydro_data <- data_15P_cal_HE_outlier_replaced %>% 
    filter(!is.na(HE)) 
  
  # built the model with some guessing values for the unknown parameters
  
  model <- nls(formula = HE ~ Xinf/(1+((Xinf/X0)-1)*exp(-k*Time)), # using the Weibull function 
               data = hydro_data,
               algorithm = "port", # add this if setting the constrains 
               start = list(Xinf = 73,
                            X0 = 1,
                            k = 0.003),
               lower = list(Xinf = 0, 
                            k = 0))
  # control = list(warnOnly = TRUE)) # change the setting, give us just the waining messages instead of error messages
  
  return(model) # give back the model
  
}


find_paramters_with_control <- function(model){ # creat a function called find_parameters
  
  result <- tidy(model) %>% 
    select(term, estimate) %>% # extract the values of these pamameters
    spread(term, estimate) # split into three separate columns
  result # give back the result
  
}


# create a list of models

model_list <- hydro_data %>% 
  split(.$Well) %>% # split the initial dataset by Well
  map(fit_model) # save all the model in a list

parameters_with_control <- model_list %>% 
  map_df(find_paramters_with_control)

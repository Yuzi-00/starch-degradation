

library(tidyverse)

library(broom)

# read in the dataset

data_15P_cal_HE_outlier_replaced <- read_csv("data/tidydata/data_15P_cal_HE_outlier_replaced.csv")

# remove the missing value and t0

hydro_data <- data_15P_cal_HE_outlier_replaced %>% 
  filter(!is.na(HE)) 

# creat a function to generate the fitted model

fit_model <- function(dfr){ # a dataframe will be passed through the following things 
  
  
  # built the model
  
  model <- nls(formula = HE ~ Xinf*(1-exp(-k*Time)), 
               data = dfr,
               algorithm = "port", # add this if setting the constrains 
               start = list(Xinf = 73, # some guessing values of the parameters
                            k = 0.003),
               lower = list(Xinf = 0, # set the constrains for some of the parameters 
                            k = 0))
  # control = list(warnOnly = TRUE)) ## change the setting, give us just the waining messages instead of error messages
  
  return(model) # give back the content of the model
  
}

# creat a function to generate the estimated parameters

find_paramters_with_control <- function(model){ # a model list will be passed through the following things
  
  result <- tidy(model) %>%
    select(term, estimate) %>% # extract the values of these pamameters
    spread(term, estimate) # split into three separate columns
  result # give back the result
  
}

# fitting the model using the complete hydro_data

model_list <- hydro_data %>% 
  split(.$Well) %>% # split the initial dataset by Well into a list (680 wells)
  map(fit_model)

# extract the residuals into a dataframe 

residual_data <- model_list %>% 
  map_df(augment, .id = "Well")

# get all the estimated parameters from the models

parameters_with_control <- model_list %>%
  map_df(find_paramters_with_control)

# extract the well names 

well <- hydro_data %>% # this is the complete hydro_data
  select(Well) %>% # select just the well column
  unique() %>% # remove the duplicated wells
  arrange(Well) # get the same order as the residual list

# save the residual data

write_csv(residual_data, "analysis/first-order_residuals_well_with_T0.csv")

# combine the estimated parameters with the well names (by order)  

parameter_well <- bind_cols(well, parameters_with_control)

# save the estimated parameters

write_csv(parameter_well, "analysis/fitted_first-order_parameters_well_with_T0.csv")

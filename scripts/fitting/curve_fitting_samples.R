

#                                       **  Using the Weibull function  **  

#           ** using three replicates separately to generate three h k and Xinf (for 3 replicates) for each sample **

library(tidyverse)

library(broom)

# read in the dataset

data_15P_cal_HE_outlier_replaced <- read_csv("data/tidydata/data_15P_cal_HE_outlier_replaced.csv")

# remove the missing value

hydro_data <- data_15P_cal_HE_outlier_replaced %>% 
  filter(!is.na(HE)) 

# creat a function to generate the fitted model

fit_model <- function(dfr){ # a dataframe will be passed through the following things 
  
  
  # built the model
  
  model <- nls(formula = HE ~ Xinf*(1-exp(-k*Time**H)), # using the Weibull function 
               data = dfr,
               algorithm = "port", # add this if setting the constrains 
               start = list(Xinf = 73, # some guessing values of the parameters
                            k = 0.003,
                            H = 1-0.0005),
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
  split(.$Sample) %>% # split the initial dataset by Well into a list (680 wells)
  map(fit_model) # pass each element of the list into the fit_model function and save all the created models in a new list


# if can't fit properly, try the following codes to check where the probelm comes from 

# get_isconv <- function(x)x$convInfo$isConv # create a function to get all the conteng in "isConv"
# 
# # sum(map_chr(model_list, class) != "nls", na.rm = TRUE) this is to show how many items in the model_list are not "nls"
# 
# isconv <- model_list %>% 
#   map_lgl(.f = get_isconv) # map_lgl: give back a logical vector (lgl stands for logical)
# # this will show us for each item, if it has been converged or not (TRUE or FALSE), and put them together
# 
# names(isconv)[isconv == FALSE] # give us the item that is not converged

# get all the residuals from the models

# residual_list <- model_list %>% 
#   map(residuals) # pass each element of the model_list to the residuals function and give back a new list containing all the residuals 

# extract the residuals into a dataframe 

residual_data <- model_list %>% 
  map_df(augment, .id = "Sample") # will return the raw data, fitted values and residuals from a model into columns in a data frame
# id = "Well": add a column called ‘Well’ containing all the names of each list within model_list

# get all the estimated parameters from the models

parameters_with_control <- model_list %>%
  map_df(find_paramters_with_control) # 680 observations here (correspond to 680 wells)

# convert the residual list into a dataframe

# residual <- data.frame(matrix(unlist(residual_list), nrow=length(residual_list), byrow=T)) # warning message here, need to be checked later 
# 
# # change the column names 
# 
# residual <- residual %>% 
#   rename("0" = X1, "20" = X2, "60" = X3, "120" = X4, "180" = X5, "240" = X6, "360" = X7, "1440" = X8, "1800" = X9)

# add the well names into the residual and estimated parameter dataframe

# extract the well names 

sample <- hydro_data %>% # this is the complete hydro_data
  select(Sample) %>% # select just the well column
  unique() %>% # remove the duplicated wells
  arrange(Sample) # get the same order as the residual list

# combine the residuals with the well names (by order)

# residual_well <- bind_cols(well, residual)
# 
# # tidy the residual_well dataframe
# 
# residual_tidy <- residual_well %>% 
#   gather(name, residual, -Well) %>% 
#   mutate(Time = as.numeric(name)) %>% # add a new column by converting the Time points to dbl
#   select(-name)

# save the residual data

write_csv(residual_data, "analysis/weibull_residuals_samples.csv")

# combine the estimated parameters with the well names (by order)  

parameter_sample <- bind_cols(sample, parameters_with_control)

# save the estimated parameters

write_csv(parameter_sample, "analysis/fitted_weibull_parameters_for_samples.csv")


###########################################################################################################################################


# below are things that need to be tidied/deleted


# need to check the 9_F_5 later after putting Well column in 

# check the residuals against the fitted values

plot(model) # noticed that there are 2 points below -5 

# find thes particular points below -5

# add a residual column to the hydro_data data frame

hydro_data$residuals = (residuals(model))

# find the minimum value 

which.min(hydro_data$residuals) # it's the 259

# find the corresponding sample name for 259

hydro_data$Sample[259] # it's the sample 92, let's remove it from the data frame



#                                             ** modelling without control samples **


# remove the control samples

data_control_removed <- data_15P_cal_HE_outlier_replaced %>% 
  filter(!Sample %in% c("C+", "C-"))


# remove the missing value

hydro_data <- data_control_removed %>% 
  filter(!is.na(HE)) 

# let's use two functions to generate the fitted model and the estimated parameters

fit_model <- function(dfr){ # creat a function called fit_model
  
  
  
  # built the model with some guessing values for the unknown parameters
  
  model <- nls(formula = HE ~ Xinf*(1-exp(-k*Time**H)), # using the Weibull function 
               data = dfr,
               algorithm = "port", # add this if setting the constrains 
               start = list(Xinf = 73,
                            k = 0.003,
                            H = 1-0.0005),
               lower = list(Xinf = 0, # set the lower values fo each parameter
                            k = 0))
  # control = list(warnOnly = TRUE)) # change the setting, give us just the waining messages instead of error messages
  
  return(model) # give back the model
  
}


find_paramters <- function(model){ # creat a function called find_parameters
  
  result <- tidy(model) %>% 
    select(term, estimate) %>% # extract the values of these pamameters
    spread(term, estimate) # split into three separate columns
  result # give back the result
  
}


# create a list of models

model_list <- hydro_data %>% 
  split(.$Well) %>% # split the initial dataset by Well
  map(fit_model) # save all the model in a list

# how to show the different items in the model ? (like convInfo for ex)

get_isconv <- function(x)x$convInfo$isConv # create a function to get all the conteng in "isConv"

# sum(map_chr(model_list, class) != "nls", na.rm = TRUE) this is to show how many items in the model_list are not "nls"

isconv <- model_list %>% 
  map_lgl(.f = get_isconv) # map_lgl: give back a logical vector (lgl stands for logical)
# this will show us for each item, if it has been converged or not (TRUE or FALSE), and put them together

names(isconv)[isconv == FALSE] # give us the item that is not converged

parameters <- model_list %>% 
  map_df(find_paramters) # got 680 observations here 

# check replicates in the working dataset

hydro_data %>% 
  group_by(Well) # 680 wells here which should give us 680 groups of parameters (which is...what we've got !)

# need to check the 9_F_5 later after putting Well column in 

# check the residuals against the fitted values

plot(model) # noticed that there are 2 points below -5 

# find thes particular points below -5

# add a residual column to the hydro_data data frame

hydro_data$residuals = (residuals(model))

# find the minimum value 

which.min(hydro_data$residuals) # it's the 259

# find the corresponding sample name for 259

hydro_data$Sample[259] # it's the sample 92, let's remove it from the data frame


#                                             ** modelling without sample 92 **


# remove the sample 92 from the previous dataset

data_92_removed <- data_control_removed %>% 
  filter(Sample != "92")

# remove the missing value

hydro_data <- data_92_removed %>% 
  filter(!is.na(HE)) 

# let's use two functions to generate the fitted model and the estimated parameters

fit_model <- function(dfr){ # creat a function called fit_model
  
  
  # built the model with some guessing values for the unknown parameters
  
  model <- nls(formula = HE ~ Xinf*(1-exp(-k*Time**H)), # using the Weibull function 
               data = dfr,
               algorithm = "port", # add this if setting the constrains 
               start = list(Xinf = 73,
                            k = 0.003,
                            H = 1-0.0005),
               lower = list(Xinf = 0, # set the lower values fo each parameter
                            k = 0))
  # control = list(warnOnly = TRUE)) # change the setting, give us just the waining messages instead of error messages
  
  return(model) # give back the model
  
}


find_paramters <- function(model){ # creat a function called find_parameters
  
  result <- tidy(model) %>% 
    select(term, estimate) %>% # extract the values of these pamameters
    spread(term, estimate) # split into three separate columns
  result # give back the result
  
}


# create a list of models

model_list <- hydro_data %>% 
  split(.$Well) %>% # split the initial dataset by Well
  map(fit_model) # save all the model in a list

# if can't fit properly, try the following codes to check where the probelm comes from 

# get_isconv <- function(x)x$convInfo$isConv ## create a function to get all the content in "isConv"
# sum(map_chr(model_list, class) != "nls", na.rm = TRUE) this is to show how many items in the model_list are not "nls"
# isconv <- model_list %>% 
# map_lgl(.f = get_isconv) ## map_lgl: give back a logical vector (lgl stands for logical)
## this will show us for each item, if it has been converged or not (TRUE or FALSE), and put them together
# names(isconv)[isconv == FALSE] ## give us the item that is not converged

parameters <- model_list %>% 
  map_df(find_paramters) # got 680 observations here 

# check replicates in the working dataset

hydro_data %>% 
  group_by(Well) # 680 wells here which should give us 680 groups of parameters (which is...what we've got !)

# need to check the 9_F_5 later after putting Well column in 

# check the residuals against the fitted values

plot(model, ylim = c(-10, 10)) # noticed that there are 2 points below -5 

plot(residuals(model))
# find thes particular points below -5

# add a residual column to the hydro_data data frame

hydro_data$residuals = (residuals(model))

# find the minimum value 

which.min(hydro_data$residuals) # it's the 259

# find the corresponding sample name for 259

hydro_data$Sample[259] # it's the sample 92, let's remove it from the data frame

glance(model_list[[1]])

#                                                ** add the Well column to the parameter list **


# extract the subset

well <- hydro_data %>% # this is the first hydro_data of this script
  select(Well) %>% # select just the sample column
  unique() %>% # remove the duplicated sample names
  arrange(Well) # get the same order as the output of the split(.$Sample) step

# combine the fitted parameters with their wells (by order)

fitted_parameters_with_control <- bind_cols(well, parameters_with_control)

# save the fitted parameters into the analysis folder 

write_csv(fitted_parameters_with_control, "analysis/fitted_Weibull_parameters_for_replicates.csv")

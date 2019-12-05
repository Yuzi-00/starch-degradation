

#                                       **  Using the Weibull function  **  

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
  
  model <- nls(formula = HE ~ Xinf*(1-exp(-k*Time**H)), # using the Weibull function 
               data = hydro_data,
               algorithm = "port", # add this if setting the constrains 
               start = list(Xinf = 73,
                            k = 0.003,
                            H = 1-0.0005),
               lower = list(Xinf = 0, # set the lower values fo each parameter
                            k = 0),
               control = list(warnOnly = TRUE)) # change the setting, give us just the waining messages instead of error messages
  
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

# how to show the different items in the model ? (like convInfo for ex)

get_isconv <- function(x)x$convInfo$isConv # create a function to get all the conteng in "isConv"

# sum(map_chr(model_list, class) != "nls", na.rm = TRUE) this is to show how many items in the model_list are not "nls"

isconv <- model_list %>% 
  map_lgl(.f = get_isconv) # map_lgl: give back a logical vector (lgl stands for logical)
# this will show us for each item, if it has been converged or not (TRUE or FALSE), and put them together

names(isconv)[isconv == FALSE] # give us the item that is not converged

parameters_with_control <- model_list %>% 
  map_df(find_paramters_with_control) # got 680 observations here 

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



#                                             ** modelling without control samples **


# remove the control samples

data_control_removed <- data_15P_cal_HE_outlier_replaced %>% 
  filter(!Sample %in% c("C+", "C-"))


# let's use two functions to generate the fitted model and the estimated parameters

fit_model <- function(data_control_removed){ # creat a function called fit_model
  
  # remove the missing value
  
  hydro_data <- data_control_removed %>% 
    filter(!is.na(HE)) 
  
  # built the model with some guessing values for the unknown parameters
  
  model <- nls(formula = HE ~ Xinf*(1-exp(-k*Time**H)), # using the Weibull function 
               data = hydro_data,
               algorithm = "port", # add this if setting the constrains 
               start = list(Xinf = 73,
                            k = 0.003,
                            H = 1-0.0005),
               lower = list(Xinf = 0, # set the lower values fo each parameter
                            k = 0),
               control = list(warnOnly = TRUE)) # change the setting, give us just the waining messages instead of error messages
  
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

# let's use two functions to generate the fitted model and the estimated parameters

fit_model <- function(data_92_removed){ # creat a function called fit_model
  
  # remove the missing value
  
  hydro_data <- data_92_removed %>% 
    filter(!is.na(HE)) 
  
  # built the model with some guessing values for the unknown parameters
  
  model <- nls(formula = HE ~ Xinf*(1-exp(-k*Time**H)), # using the Weibull function 
               data = hydro_data,
               algorithm = "port", # add this if setting the constrains 
               start = list(Xinf = 73,
                            k = 0.003,
                            H = 1-0.0005),
               lower = list(Xinf = 0, # set the lower values fo each parameter
                            k = 0),
               control = list(warnOnly = TRUE)) # change the setting, give us just the waining messages instead of error messages
  
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



#                                                ** add the Well column to the parameter list **


# extract the subset

well <- hydro_data %>% 
  select(Well) %>% # select just the sample column
  unique() %>% # remove the duplicated sample names
  arrange(Well) # get the same order as the output of the split(.$Sample) step

# combine the fitted parameters with their wells (by order)

fitted_parameters_with_control <- bind_cols(well, parameters_with_control)

# find the 9_F_5 and check what's going on

para_problem_well <- fitted_parameters %>% 
  filter(Well == "9_F_5")

# let's try to fit for this single Well

model_problem_well <- nls(formula = HE ~ Xinf*(1-exp(-k*Time**(1-h))), # using the Weibull function 
             data = problem_well,
             start = list(Xinf = 73,
                          k = 0.003,
                          h = 0.0005),
             algorithm = "port", # add this if setting the constrains 
             lower = list(Xinf = 0, # set the lower values fo each parameter
                          k = 0,
                          h = -0.5),
             control = list(warnOnly = TRUE)) # didn't work either

summary(model_problem_well)

problem_well %>% 
  select(Time, HE) %>% 
  plot(ylim = c(0, 100)) # set the range of the y axis up to 100

# plot the fitted line

## creat a data frame that contains 100 time points

new.data <- data.frame(Time = seq(min(problem_well$Time), max(problem_well$Time), len = 100))

## draw the new line (fitted line)

lines(new.data$Time, predict(model_problem_well, newdata = new.data), col='red')

# save the fitted parameters into the analysis folder 

write_csv(fitted_parameters_with_control, "analysis/fitted_Weibull_parameters_15P_with_control_ver2.csv")



#                                       **  Using the Weibull function  **  

#           ** using three replicates separately to generate three h k and Xinf (for 3 replicates) for each sample **

library(tidyverse)

library(broom)

# read in the dataset

data_15P_cal_HE_outlier_deleted <- read_csv("data/tidydata/data_15P_cal_HE_outlier_deleted.csv")


#                                             ** extrat parameters **


# let's use two functions to generate the fitted model and the estimated parameters

fit_model <- function(data_15P_cal_HE_outlier_deleted){ # creat a function called fit_model
  
  # remove the missing value
  
  hydro_data <- data_15P_cal_HE_outlier_deleted %>% 
    filter(!is.na(HE)) 
  
  # built the model with some guessing values of the unknown parameters
  
  model <- nls(formula = HE ~ Xinf*(1-exp(-k*Time**(1-h))), # using the Weibull function 
               data = hydro_data,
               algorithm = "port", # add this if setting the constrains 
               start = list(Xinf = 73,
                            k = 0.003,
                            h = 0.0005),
               lower = list(Xinf = 0,
                            k = 0,
                            h = -0.5),
               control = list(warnOnly = TRUE)) # change the setting, give us just the waining messages instead of error messages
  
return(model) # give back the model

}


find_paramters <- function(model){ # creat a function called find_parameters
  
  result <- tidy(model) %>% 
    select(term, estimate) %>% # extract the values of these pamameters
    spread(term, estimate) # split into three separate columns
  result # give back the result
}


# extract the parameters 

model_list <- hydro_data %>% 
  split(.$Well) %>% # split the initial dataset into 182 subsets by sample
  map(fit_model) # error here, can not generate the estimateed parmaters
 # may be not gussing the right values for the k, h and Xinf

get_isconv <- function(x)x$convInfo$isConv

# sum(map_chr(model_list, class) != "nls", na.rm = TRUE)

isconv <- model_list %>% 
  map_lgl(.f = get_isconv) # give back a logical vector 

names(isconv)[isconv == FALSE]

parameters <- model_list %>% 
  map_df(find_paramters) # error here, can not generate the estimateed parmaters

# not matter we set the constraints or not, the estimate parameters can not be generated 


#                     ** add the sample name to the parameter list **


# extract the subset

sample_name <- hydro_data %>% 
  select(Sample) %>% # select just the sample column
  unique() %>% # remove the duplicated sample names
  arrange(Sample) # get the same order as the output of the split(.$Sample) step

# combine the fitted parameters with their sample names (by order)

fitted_parameters <- bind_cols(sample_name, parameters)

# save the fitted parameters into the analysis folder 

write_csv(fitted_parameters, "analysis/fitted_Weibull_parameters_15P.csv")



library(tidyverse)

library(broom)


# read in the dataset

data_15P_cal_HE_outlier_replaced <- read_csv("data/tidydata/data_15P_cal_HE_outlier_replaced.csv")

# remove the missing value and select just the first 6 hours

hydro_data <- data_15P_cal_HE_outlier_replaced %>% 
  filter(Time != 1800) %>%
  # mutate(HE = if_else(Sample == 75 & Plate == 13, NA_real_,HE)) %>% # remove one replicat 
  # mutate(HE = if_else(Sample == 111 & Plate == 2 & Time == 180, NA_real_,HE)) %>% # remove one point
  # mutate(HE = if_else(Sample == 95 & Plate == 2 & Time == 240, NA_real_,HE)) %>% # remove one point
  # mutate(HE = if_else(Sample == 2 & Plate == 2, NA_real_,HE)) %>% # remove one replicat
  # mutate(HE = if_else(Sample == 8 & Plate == 9 & Time == 120, NA_real_,HE)) %>% # remove one replicat
  # mutate(HE = if_else(Sample == 179 & Plate == 13 & Time == 180, NA_real_,HE)) %>%
  # mutate(HE = if_else(Sample == 210 & Plate == 3 & Time == 60, NA_real_,HE)) %>%
  # mutate(HE = if_else(Sample == 35 & Plate == 13 & Time == 240, NA_real_,HE)) %>%
  # mutate(HE = if_else(Sample == 56 & Plate == 3, NA_real_,HE)) %>%
  # mutate(HE = if_else(Sample == 188 & Plate == 3, NA_real_,HE)) %>%
  # mutate(HE = if_else(Sample == 171 & Plate == 3, NA_real_,HE)) %>%
# mutate(HE = if_else(Sample == 31 & Plate == 2, NA_real_,HE)) %>%
# mutate(HE = if_else(Sample == 136 & Plate == 2, NA_real_,HE)) %>%
# mutate(HE = if_else(Sample == 50 & Plate == 13, NA_real_,HE)) %>%
# mutate(HE = if_else(Sample == 46 & Plate == 3, NA_real_,HE)) %>%
# mutate(HE = if_else(Sample == 52 & Plate == 2, NA_real_,HE)) %>%
# mutate(HE = if_else(Sample == 25 & Plate == 3, NA_real_,HE)) %>%
# filter(Sample != "77") %>% # 待定，需重新检查
# filter(Sample != "118") %>%
# filter(Sample != "92") %>%
# filter(Time != 0) %>%
filter(!is.na(HE))


# hydro_data <- hydro_data %>%
#   filter(Sample == "1") 
# creat a function to generate the fitted model

fit_model <- function(dfr){ # a dataframe will be passed through the following things 
  
  
  # built the model
  
  model <- nls(formula = HE ~ Xinf*(1-exp(-k*Time**H)), # using the Weibull function 
               data = dfr,
               algorithm = "port", # add this if setting the constrains 
               start = list(Xinf = 50, # some guessing values of the parameters
                            k = 0.002,
                            H = 1.5),
               lower = list(Xinf = 0, # set the constrains for some of the parameters
                            k = 0),
               upper = list(Xinf = 100))
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
  map(fit_model) # pass each element of the list into the fit_model function and save all the created models in a new list


# # if can't fit properly, try the following codes to check where the probelm comes from 
# 
# get_isconv <- function(x)x$convInfo$isConv # create a function to get all the conteng in "isConv"
# # 
# sum(map_chr(model_list, class) != "nls", na.rm = TRUE) 
# #this is to show how many items in the model_list are not "nls"
# # 
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
  map_df(augment, .id = "Well") # will return the raw data, fitted values and residuals from a model into columns in a data frame
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

well <- hydro_data %>% # this is the complete hydro_data
  select(Well) %>% # select just the well column
  unique() %>% # remove the duplicated wells
  arrange(Well) # get the same order as the residual list

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

write_csv(residual_data, "analysis/weibull_24h_residuals_well_with_T0.csv")

# combine the estimated parameters with the well names (by order)  

parameter_well <- bind_cols(well, parameters_with_control)

# save the estimated parameters

write_csv(parameter_well, "analysis/fitted_weibull_parameters_24h_well_with_T0.csv")

# add the well colume to the fitted parameter dataset

well <- hydro_data %>% # this is the first hydro_data of this script
  select(Well) %>% # select just the sample column
  unique() %>% # remove the duplicated sample names
  arrange(Well) # get the same order as the output of the split(.$Sample) step

# combine the fitted parameters with their wells (by order)

fitted_parameters_with_control <- bind_cols(well, parameters_with_control)

# save the fitted parameters into the analysis folder 

write_csv(fitted_parameters_with_control, 
          "analysis/fitted_Weibull_24h_parameters_well_with_T0_wellnumber.csv")

#### check the fitting for pos control ####

df <- read_csv("data/tidydata/data_15P_cal_HE_outlier_replaced.csv") %>%
  select(Well, Sample) %>%
  unique()

# add the sample names to the fitted parameter dataset

df2 <- left_join(fitted_parameters_with_control, df) %>%
  mutate(h = 1-H)

# extract the pos control

pos_control <- df2 %>%
  filter(Sample == "C+") 

# calculate the std deviation for the pos control (weibull 24h)

pos_control %>%
  summarise(Mean_k = mean(k, na.rm = TRUE), 
            Sd_k = sd(k, na.rm = TRUE),
            Cov_k = Sd_k / Mean_k * 100,
            Mean_h = mean(h, na.rm = TRUE), 
            Sd_h = sd(h, na.rm = TRUE),
            Cov_h = Sd_h / Mean_h * 100,
            Mean_Xinf = mean(Xinf, na.rm = TRUE), 
            Sd_Xinf = sd(Xinf, na.rm = TRUE),
            Cov_Xinf = Sd_Xinf / Mean_Xinf * 100)
## mean_k = 0.00261, sd_k = 0.00209, cov = 79.8%
## mean_h = -0.209, sd_h = 0.148, cov = -70.8%
## mean_Xinf = 81, sd_Xinf = 3.59, cov = 4.43%

#### let's compare with the k fitted by the weibull function

df <- read_csv("analysis/total_new_convert.csv") %>%
  select(Sample, k, Xinf, h) %>%
  filter(Sample == "C+") 

# calculate the std deviation for the pos control (Weibull 30h)

df %>%
  summarise(Mean_k = mean(k, na.rm = TRUE), 
            Sd_k = sd(k, na.rm = TRUE),
            Cov_k = Sd_k / Mean_k * 100,
            Mean_h = mean(h, na.rm = TRUE), 
            Sd_h = sd(h, na.rm = TRUE),
            Cov_h = Sd_h / Mean_h * 100,
            Mean_Xinf = mean(Xinf, na.rm = TRUE), 
            Sd_Xinf = sd(Xinf, na.rm = TRUE),
            Cov_Xinf = Sd_Xinf / Mean_Xinf * 100)
## mean_k = 0.00281, sd_k = 0.00205, cov = 73%
## mean_h = -0.181, sd_h = 0.139, cov = -77.2%
## mean_Xinf = 82.6, sd_Xinf = 3.58, cov = 4.34%

#### check the fitting for neg control ####

df <- read_csv("data/tidydata/data_15P_cal_HE_outlier_replaced.csv") %>%
  select(Well, Sample) %>%
  unique()

# add the sample names to the fitted parameter dataset

df2 <- left_join(fitted_parameters_with_control, df) %>%
  mutate(h = 1-H)

# extract the neg control

neg_control <- df2 %>%
  filter(Sample == "C-") 

# calculate the std deviation for the neg control (weibull 24h)

neg_control %>%
  summarise(Mean_k = mean(k, na.rm = TRUE), 
            Sd_k = sd(k, na.rm = TRUE),
            Cov_k = Sd_k / Mean_k * 100,
            Mean_h = mean(h, na.rm = TRUE), 
            Sd_h = sd(h, na.rm = TRUE),
            Cov_h = Sd_h / Mean_h * 100,
            Mean_Xinf = mean(Xinf, na.rm = TRUE),
            Mean_Xinf = mean(Xinf, na.rm = TRUE), 
            Sd_Xinf = sd(Xinf, na.rm = TRUE),
            Cov_Xinf = Sd_Xinf / Mean_Xinf * 100)
## mean_k = 0.0103, sd_k = 0.00384, cov = 37.3%
## mean_h = 0.164, sd_h = 0.0703, cov = 43%
## mean_Xinf = 55.3, sd_Xinf = 3.88, cov = 7.02%

#### let's compare with the k fitted by the weibull function

df <- read_csv("analysis/total_new_convert.csv") %>%
  select(Sample, k, Xinf, h) %>%
  filter(Sample == "C-")

# calculate the std deviation for the neg control (Weibull 30h)

df %>%
  summarise(Mean_k = mean(k, na.rm = TRUE), 
            Sd_k = sd(k, na.rm = TRUE),
            Cov_k = Sd_k / Mean_k * 100,
            Mean_h = mean(h, na.rm = TRUE), 
            Sd_h = sd(h, na.rm = TRUE),
            Cov_h = Sd_h / Mean_h * 100,
            Mean_Xinf = mean(Xinf, na.rm = TRUE), 
            Sd_Xinf = sd(Xinf, na.rm = TRUE),
            Cov_Xinf = Sd_Xinf / Mean_Xinf * 100)
## mean_k = 0.011, sd_k = 0.00408, cov = 37.1%
## mean_h = 0.187, sd_h = 0.0679, cov = 36.4%
## mean_Xinf = 57, sd_Xinf = 3.19, cov = 5.59%


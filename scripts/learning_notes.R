
library(tidyverse)


#                        ** creat a column to distinguish each samples **


# import the dataset

data_15P_cal_HE_outlier_deleted <- read_csv("data/tidydata/data_15P_cal_HE_outlier_deleted.csv")

# creat a column to distinguish each samples using "case_when" function

data_add_status <- data_15P_cal_HE_outlier_deleted %>% 
  mutate(status = case_when( # number of conditions (left), discription of the results in that condition
    Sample == "C+" ~ "Positive control", 
    Sample == "C-" ~ "Negative control",
    TRUE ~ "Sample"
  ))

# this is very useful when you want to color different type of samples or when you want to creat different
# subsets 
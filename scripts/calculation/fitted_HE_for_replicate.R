

#                                     **  Using the Weibull function  **  

#           ** using three replicates separately to generate one single h k and Xinf for each replicate **

library(tidyverse)

# read in the dataset of the estimated parameters

parameter <- read_csv("analysis/fitted_Weibull_parameters_for_replicates.csv") 

# read in the experimental data

data_15P <- read_csv("data/tidydata/data_15P_cal_HE_outlier_replaced.csv")

# combine these two datasets together

data_with_para <- left_join(data_15P, parameter) # joined by Well

# calculate the hydrolysis extent using the estimated parameters

data_with_fitted_HE <- data_with_para %>% 
  mutate(fitted_HE = Xinf*(1-exp(-k*Time**H)))

# save the dataset

write_csv(data_with_fitted_HE, "analysis/data_15P_with_fitted_HE.csv")

# add the residual data to the previous dataset

# import the residuals

residual <- read_csv("analysis/weibull_residuals.csv")

# combination

data_fitted_HE_residual <- left_join(data_with_fitted_HE, residual)

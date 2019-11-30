

#                                     **  Using the Weibull function  **  

#           ** using three replicates separately to generate one single h k and Xinf for each sample **


# read in the dataset of the estimated parameters

parameter <- read_csv("analysis/fitted_Weibull_parameters_15P.csv") 

# read in the uncalculated dataset

data_15P <- read_csv("data/tidydata/data_15P_outlier_deleted.csv")

# combien these two datasets together

data_with_para <- left_join(data_15P, parameter) # joined by Sample

# calculate the hydrolysis extent using the estimated parameters

data_with_fitted_HE <- data_with_para %>% 
  mutate(fitted_HE = Xinf*(1-exp(-k*Time**(1-h))))

# save the dataset

write_csv(data_with_fitted_HE, "analysis/data_15P_with_fitted_HE.csv")

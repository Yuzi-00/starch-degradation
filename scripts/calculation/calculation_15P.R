

#    ** use the average of the Slope by plate and the average of all the blk within each plate **

#             ** calculate the [reducing sugar] and the hydrolysis extent **


library(tidyverse)

# import the joined dataset

data_15P <- read_csv("data/tidydata/joined_15P_with_mass_slope_id.csv")

# calculate the concentration of the reducing sugar for the sample and the blank

data_cal <- data_15P %>% 
  mutate(C_sample = OD_sample / mean_slope, # calculate the concentration using the mean slope by plate
         C_blk = OD_blk / mean_slope,
         C_spl_nor = 10 * C_sample / Mass_sample, # normalize the concentraion at 10mg
         C_blk_nor = 10 * C_blk / Mass_blk)

# calculate the mean values of the blank for each plate

data_cal <- data_cal %>% 
  group_by(Plate) %>% 
  mutate(mean_blk = mean(C_blk_nor, na.rm = TRUE))
  
# substrate the blank and calculate the hydrolysis extent

data_15P_cal <- data_cal %>% 
  mutate(C = C_spl_nor - mean_blk, 
         # calculate the final concentration using the mean value of the blank
         HE = C / (10 / 0.9) * 100) # calculate the hydrolysis extent
# in theory, the HE should be 10/0.9 = 11 g/L

# save the calculated dataset

write_csv(data_15P_cal, "data/tidydata/data_15P_cal.csv")


#                          ** calculate the mean, sd and cov **


df <- read_csv("data/tidydata/data_15P_outlier_deleted.csv")

var <- df %>% 
  group_by(Sample, Time) %>% 
  summarise(mean_HE = mean(HE, na.rm = TRUE), sd_HE = sd(HE, na.rm = TRUE), cov = sd_HE / mean_HE * 100) 

# save the variation

write_csv(var, "analysis/variation_15P.csv")

# combine the variation with the replicate's dataset

data_15P_var <- left_join(df, var)

# save the joined dataset

write_csv(data_15P_var, "analysis/data_15P_var.csv")








#             ** calculate the [reducing sugar] and the hydrolysis extent **

#    ** use the average of the Slope by plate and the average of the blk by plate and by time**




library(tidyverse)

# import the dataset

data_15P <- read_csv("data/tidydata/joined_15P_with_mass_slope_id.csv")

# calculate the concentration (C) of the reducing sugar for the sample and the blank: C = OD/slope

data_cal <- data_15P %>% 
  mutate(C_sample = OD_sample / Mean_slope, # calculate the concentration using the mean slope by plate
         C_blk = OD_blk / Mean_slope,
         
         # the mass of the sample was weighed between 9.5mg and 10.5 mg, which are all normalized to 10mg while calculating the concentration
         
         C_spl_nor = 10 * C_sample / Mass_sample, 
         C_blk_nor = 10 * C_blk / Mass_blk)

# calculate the mean values of the blank for each plate (at each time point)

data_cal <- data_cal %>% 
  group_by(Plate, Time) %>% 
  mutate(Mean_blk = mean(C_blk_nor, 
                         na.rm = TRUE,
                         trim = 0.1)) # 10% of the data on both sides will be removed while calculating the means
  
# substrate the blank and calculate the hydrolysis extent

data_15P_cal_HE <- data_cal %>% 
  mutate(C = C_spl_nor - Mean_blk, 
         # calculate the final concentration using the mean value of the blank
         HE = C / (10 / 0.9) * 100) # calculate the hydrolysis extent
# in theory, the HE should be 10/0.9 = 11 g/L

# save the calculated dataset

write_csv(data_15P_cal_HE, "data/tidydata/data_15P_cal_HE.csv")









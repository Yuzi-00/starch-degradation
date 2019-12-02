
library(tidyverse)


#                              ** calculate the mean, sd and cov **


data_15P_cal_HE_outlier_deleted <- read_csv("data/tidydata/data_15P_cal_HE_outlier_deleted.csv")

variation <- data_15P_cal_HE_outlier_deleted %>% 
  group_by(Sample, Time) %>% 
  summarise(Mean_HE = mean(HE, na.rm = TRUE), 
            Sd_HE = sd(HE, na.rm = TRUE), 
            Cov = Sd_HE / Mean_HE * 100) 

# save the dataset

write_csv(variation, "analysis/variation_15P.csv")

# combine the variation with the "data_15P_cal_HE_outlier_deleted" dataset

data_15P_cal_var <- left_join(data_15P_cal_HE_outlier_deleted, variation)

# save the joined dataset

write_csv(data_15P_cal_var, "analysis/data_15P_cal_var.csv")


#                              ** check the high variation samples (Cov > 10) **


variation %>% 
  filter(Cov > 10) %>% # choose just the Cov above 10
  filter(!(Time %in% c(0, 20))) %>% # remove the first two time points 
# 300 out of 2034 observations have a Cov > 10
  filter(Time %in% c(1440, 1800)) # check how many Cov above 10 are at 1440 or 1800min
# just 15 observations here 


#                              ** calculate the standard error **


data_15P_cal_var <- data_15P_cal_var %>% 
  mutate(Se_HE = Sd_HE/sqrt(3)) # sqrt: radical sign
         
# save the dataset

write_csv(data_15P_cal_var, "analysis/data_15P_cal_var.csv")

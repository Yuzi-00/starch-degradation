

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

library(tidyverse)

data_6P <- read_csv("data/tidydata/data_6P_without_cal.csv")
# import the final dataset

cal_6P <- data_6P %>% 
  mutate(C_sample = OD_sample / Slope, # calculate the concentration
         C_blk = OD_blk / Slope,
         C_spl_nor = 10 * C_sample / Mass_sample, # calculate the concentration by normalising 
                                                 # the mass at 10 mg
         C_blk_nor = 10 * C_blk / Mass_blk,
         C = C_spl_nor - C_blk_nor,
         HE = C / (10 / 0.9) * 100) # calculate the hydrolysis extent
                                  # in theory, the HE should be 10/0.9 = 11 g/L


cal_6P_filted <- cal_6P %>% 
  filter(Mass_sample != 0 & Mass_blk != 0) # remove the empty samples
# 5 samples have been removed here, which is correct. Empty samples: 178, 101, 146, 49, 107
  
write_csv(cal_6P_filted, "data/tidydata/data_6P_cal.csv")

pos_control <- cal_6P_filted %>% 
  filter(Sample == "C+", Time == 1800) %>% # select just the positive control at 1800min (30h)
  arrange(desc(HE)) # arrange the HE in order to know the highest value of C+ so that we can 
                  # use this value to filter the sample which are above it
                # the max extent of hydrolysis of C+ is 91.9%

pos_control_NA_removed <- pos_control %>% 
  filter(OD_sample != "NA") # removing the NA (the row at 1800min)

pos_control_aver <- pos_control_NA_removed %>% 
  summarise(mean_pos_control = mean(HE)) # calculating the mean value of the pos control at 1800min, and the value obtained is 87.7%

high_HE <- cal_6P %>% 
  filter(Time == 1800, Sample != "C+") %>%  # selecting the 1800min (30h) and removing the pos control samples 
  group_by(Sample) %>% # group by samples 
  summarise(mean_HE = mean(HE)) %>% # calculating the mean value of the HE for each sample at 1800min
  filter(mean_HE >= 87.7) # retaining just the samples of which the HE are above 87.7% (the average of pos control)
# here, we can see just 2 samples are above the pos control: 67 and 8

sample_67 <- cal_6P %>% 
  filter(Sample == 67, Time == 1800) # take a look at the sample 67
# until the plate 6, the sample 67 hadn't been repeated, so keep an eye on the other two replicats (in plate 7 and 9)

sample_08 <- cal_6P %>% 
  filter(Sample == 8, Time == 1800) # take a look at the sample 08
# until the plate 6, the sample 08 hadn't been repeated, so keep an eye on the other two replicats (in plate 9 and 14)

write_csv(high_HE, "results/high_HE_samples.csv") # save the data of high HE samples

neg_control <- cal_6P_filted %>% 
  filter(Sample == "C-", Time == 1800) %>% # select just the negative control at 1800min (30h)
  arrange(HE)

neg_control_NA_removed <- neg_control %>% 
  filter(OD_sample != "NA") # removing the NA (the row at 1800min)

neg_control_aver <- neg_control_NA_removed %>% 
  summarise(mean_neg_control = mean(HE)) # calculating the mean value of the neg control at 1800min, and the value obtained is 56.2%

low_HE <- cal_6P %>% 
  filter(Time == 1800, Sample != "C-") %>%  # selecting the 1800min (30h) and removing the neg control samples 
  group_by(Sample) %>% # group by samples 
  summarise(mean_HE = mean(HE)) %>% # calculating the mean value of the HE for each sample at 1800min
  filter(mean_HE <= 56.2) # on one is below the neg control(HAMY)


######################################################################################################


# if we use the average of the Slope (the average of all the slope for the 6 plates)

library(tidyverse)

data_6P_2nd <- read_csv("data/tidydata/data_6P_without_cal.csv")
# import the final dataset

cal_6P_2nd <- data_6P_2nd %>% 
  mutate(C_sample = OD_sample / 0.1893, # calculate the concentration, 0.1893 is the average slope
         # 0.1893 was calculated in excel(see slope.xlsx)
         C_blk = OD_blk / 0.1893,
         C_spl_nor = 10 * C_sample / Mass_sample, # calculate the concentration by normalising 
         # the mass at 10 mg
         C_blk_nor = 10 * C_blk / Mass_blk,
         C = C_spl_nor - C_blk_nor,
         HE = C / (10 / 0.9) * 100) # calculate the hydrolysis extent
# in theory, the HE should be 10/0.9 = 11 g/L

cal_6P_2nd_filted <- cal_6P_2nd %>% 
  filter(Mass_sample != 0 & Mass_blk != 0) # remove the empty samples
# 4 samples have been removed here: 178(appears twice), 101, 146, 49

write_csv(cal_6P_2nd_filted, "data/tidydata/data_6P_cal_2nd.csv")

pos_control_2nd <- cal_6P_2nd_filted %>% 
  filter(Sample == "C+", Time == 1800) %>% # select just the positive control at 1800min (30h)
  arrange(desc(HE)) # arrange the HE in order to know the highest value of C+ so that we can 
# use this value to filter the sample which are above it
# the min extent of hydrolysis of C+ is 82.1%

pos_control_2nd_NA_removed <- pos_control_2nd %>% 
  filter(OD_sample != "NA") # removing the NA (the row at 1800min)

pos_control_2nd_aver <- pos_control_2nd_NA_removed %>% 
  summarise(mean_pos_control = mean(HE)) # calculating the mean value of the pos control at 1800min, and the value obtained is 86.4%

high_HE_2nd <- cal_6P_2nd_filted %>% 
  filter(Time == 1800, Sample != "C+") %>%  # selecting the 1800min (30h) and removing the pos control samples 
  group_by(Sample) %>% # group by samples 
  summarise(mean_HE = mean(HE)) %>% # calculating the mean value of the HE for each sample at 1800min
  filter(mean_HE >= 86.4) # retaining just the samples of which the HE are above 86.4% (the average of pos control)
# here, we can see just one sample are above the pos control: 67

write_csv(high_HE_2nd, "results/high_HE_samples_2nd.csv") # save the data of high HE samples

sample_67 <- cal_6P %>% 
  filter(Sample == 67, Time == 1800) # take a look at the sample 67
# until the plate 6, the sample 67 hadn't been repeated, so keep an eye on the other two replicats (in plate 7 and 9)

neg_control_2nd <- cal_6P_2nd_filted %>% 
  filter(Sample == "C-", Time == 1800) %>% # select just the positive control at 1800min (30h)
  arrange(HE)

neg_control_2nd_NA_removed <- neg_control_2nd %>% 
  filter(OD_sample != "NA") # removing the NA (the row at 1800min)

neg_control_2nd_aver <- neg_control_2nd_NA_removed %>% 
  summarise(mean_neg_control = mean(HE)) # calculating the mean value of the neg control at 1800min, and the value obtained is 55.4%

low_HE_2nd <- cal_6P_2nd_filted %>% 
  filter(Time == 1800, Sample != "C-") %>%  # selecting the 1800min (30h) and removing the neg control samples 
  group_by(Sample) %>% # group by samples 
  summarise(mean_HE = mean(HE)) %>% # calculating the mean value of the HE for each sample at 1800min
  filter(mean_HE <= 55.4) # on one is below the neg control(HAMY)
######################################################################################################


# if we use the average of the Slope (the average of all the slope for the 6 plates)

library(tidyverse)

data_6P_2nd <- read_csv("data/tidydata/data_6P_without_cal.csv")
# import the final dataset

cal_6P_2nd <- data_6P_2nd %>% 
  mutate(C_sample = OD_sample / 0.1893, # calculate the concentration, 0.1893 is the average slope
         # 0.1893 was calculated in excel(see slope.xlsx)
         C_blk = OD_blk / 0.1893,
         C_spl_nor = 10 * C_sample / Mass_sample, # calculate the concentration by normalising 
         # the mass at 10 mg
         C_blk_nor = 10 * C_blk / Mass_blk,
         C = C_spl_nor - C_blk_nor,
         HE = C / (10 / 0.9) * 100) # calculate the hydrolysis extent
# in theory, the HE should be 10/0.9 = 11 g/L

cal_6P_2nd_filted <- cal_6P_2nd %>% 
  filter(Mass_sample != 0 & Mass_blk != 0) # remove the empty samples
# 4 samples have been removed here: 178(appears twice), 101, 146, 49

write_csv(cal_6P_2nd_filted, "data/tidydata/data_6P_cal_2nd.csv")

pos_control_2nd <- cal_6P_2nd_filted %>% 
  filter(Sample == "C+", Time == 1800) %>% # select just the positive control at 1800min (30h)
  arrange(desc(HE)) # arrange the HE in order to know the highest value of C+ so that we can 
# use this value to filter the sample which are above it
# the min extent of hydrolysis of C+ is 82.1%

pos_control_2nd_NA_removed <- pos_control_2nd %>% 
  filter(OD_sample != "NA") # removing the NA (the row at 1800min)

pos_control_2nd_aver <- pos_control_2nd_NA_removed %>% 
  summarise(mean_pos_control = mean(HE)) # calculating the mean value of the pos control at 1800min, and the value obtained is 86.4%

high_HE_2nd <- cal_6P_2nd_filted %>% 
  filter(Time == 1800, Sample != "C+") %>%  # selecting the 1800min (30h) and removing the pos control samples 
  group_by(Sample) %>% # group by samples 
  summarise(mean_HE = mean(HE)) %>% # calculating the mean value of the HE for each sample at 1800min
  filter(mean_HE >= 86.4) # retaining just the samples of which the HE are above 86.4% (the average of pos control)
# here, we can see just one sample are above the pos control: 67

write_csv(high_HE_2nd, "results/high_HE_samples_2nd.csv") # save the data of high HE samples

sample_67 <- cal_6P %>% 
  filter(Sample == 67, Time == 1800) # take a look at the sample 67
# until the plate 6, the sample 67 hadn't been repeated, so keep an eye on the other two replicats (in plate 7 and 9)

neg_control_2nd <- cal_6P_2nd_filted %>% 
  filter(Sample == "C-", Time == 1800) %>% # select just the positive control at 1800min (30h)
  arrange(HE)

neg_control_2nd_NA_removed <- neg_control_2nd %>% 
  filter(OD_sample != "NA") # removing the NA (the row at 1800min)

neg_control_2nd_aver <- neg_control_2nd_NA_removed %>% 
  summarise(mean_neg_control = mean(HE)) # calculating the mean value of the neg control at 1800min, and the value obtained is 55.4%

low_HE_2nd <- cal_6P_2nd_filted %>% 
  filter(Time == 1800, Sample != "C-") %>%  # selecting the 1800min (30h) and removing the neg control samples 
  group_by(Sample) %>% # group by samples 
  summarise(mean_HE = mean(HE)) %>% # calculating the mean value of the HE for each sample at 1800min
  filter(mean_HE <= 55.4) # on one is below the neg control(HAMY)


#####################################################################################################


# PS: calculating the variation of blk to see if we can use the average of it while doing the calculation


x <- cal_6P %>% 
  group_by(Plate, Time) %>% 
  summarise(mean_blk = mean(OD_blk), sd_blk = sd(OD_blk), cov = sd_blk / mean_blk * 100)
# most of the variation are < 10%

######################################################################################################


# if we use the average of the Slope by plate 
# and the average of all the blk within each plate

library(tidyverse)

data_6P_3nd <- read_csv("data/tidydata/data_6P_without_cal.csv")
# import the final dataset

mean_slope_6P <- data_6P_3nd %>%  
  filter(Time == 360 | Time == 1440) %>% 
  group_by(Plate) %>% # group by plate
  summarise(Mean_slope = mean(Slope, na.rm = TRUE)) # calculate the average of slope by plate
  
data_with_mean_slope <- left_join(data_6P_3nd, mean_slope_6P) 
# merge the average of slope by plate into the previous dataset

data_filted <- data_with_mean_slope %>% 
  filter(Mass_sample != 0) # remove the empty sample which will disturb the calculation later on 
                          # 4 samples have been removed here: 178(appears twice), 101, 146, 49

df <- data_filted %>% 
  mutate(C_sample = OD_sample / Mean_slope, # calculate the concentration using the mean slope by plate
         C_blk = OD_blk / Mean_slope,
         C_spl_nor = 10 * C_sample / Mass_sample, 
         C_blk_nor = 10 * C_blk / Mass_blk)
         # calculate the concentration by normalising the mass at 10 mg

df <- df %>% 
  group_by(Plate) %>% # group by plate
  mutate(mean_blk = mean(C_blk_nor, na.rm = TRUE)) # calculate the mean of the blank for each plate

cal_6P_3nd <- df %>%
  mutate(C = C_spl_nor - mean_blk, 
         # calculate the final concentration using the mean value of the blank
         HE = C / (10 / 0.9) * 100) # calculate the hydrolysis extent
        # in theory, the HE should be 10/0.9 = 11 g/L

write_csv(cal_6P_3nd, "data/tidydata/data_6P_cal_3nd.csv")

################################################################################################################

# draft

pos_control_2nd <- cal_6P_2nd_filted %>% 
  filter(Sample == "C+", Time == 1800) %>% # select just the positive control at 1800min (30h)
  arrange(desc(HE)) # arrange the HE in order to know the highest value of C+ so that we can 
# use this value to filter the sample which are above it
# the min extent of hydrolysis of C+ is 82.1%

pos_control_2nd_NA_removed <- pos_control_2nd %>% 
  filter(OD_sample != "NA") # removing the NA (the row at 1800min)

pos_control_2nd_aver <- pos_control_2nd_NA_removed %>% 
  summarise(mean_pos_control = mean(HE)) # calculating the mean value of the pos control at 1800min, and the value obtained is 86.4%

high_HE_2nd <- cal_6P_2nd_filted %>% 
  filter(Time == 1800, Sample != "C+") %>%  # selecting the 1800min (30h) and removing the pos control samples 
  group_by(Sample) %>% # group by samples 
  summarise(mean_HE = mean(HE)) %>% # calculating the mean value of the HE for each sample at 1800min
  filter(mean_HE >= 86.4) # retaining just the samples of which the HE are above 86.4% (the average of pos control)
# here, we can see just one sample are above the pos control: 67

write_csv(high_HE_2nd, "results/high_HE_samples_2nd.csv") # save the data of high HE samples

sample_67 <- cal_6P %>% 
  filter(Sample == 67, Time == 1800) # take a look at the sample 67
# until the plate 6, the sample 67 hadn't been repeated, so keep an eye on the other two replicats (in plate 7 and 9)

neg_control_2nd <- cal_6P_2nd_filted %>% 
  filter(Sample == "C-", Time == 1800) %>% # select just the positive control at 1800min (30h)
  arrange(HE)

neg_control_2nd_NA_removed <- neg_control_2nd %>% 
  filter(OD_sample != "NA") # removing the NA (the row at 1800min)

neg_control_2nd_aver <- neg_control_2nd_NA_removed %>% 
  summarise(mean_neg_control = mean(HE)) # calculating the mean value of the neg control at 1800min, and the value obtained is 55.4%

low_HE_2nd <- cal_6P_2nd_filted %>% 
  filter(Time == 1800, Sample != "C-") %>%  # selecting the 1800min (30h) and removing the neg control samples 
  group_by(Sample) %>% # group by samples 
  summarise(mean_HE = mean(HE)) %>% # calculating the mean value of the HE for each sample at 1800min
  filter(mean_HE <= 55.4) # on one is below the neg control(HAMY)
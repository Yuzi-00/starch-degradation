library(tidyverse)

# read in the tidy dataset

data_15P_HE <- read_csv("data/tidydata/data_15P_cal_HE_outlier_replaced.csv")


#                                                            ** add amylose content **


# read in the tidy dataset for the amylose content

amy <- read_csv("data/tidydata/previous_data/amy_ID_sample.csv")

# combine these two datasets

data_15P_HE_amy <- left_join(data_15P_HE, amy) %>%
  rename(Hydro_extent = HE, Amylose_content = mean_amy) # change the column name

write_csv(data_15P_HE_amy, "data/tidydata/HE_amy_15P.csv")

# need to add the Amy% to C+ and C- (replace the NA) !!!!!!!!!!!!!!!!!!!!!!!


#                                                              ** add master size **

# read in the master size dataset

master_size <- read_csv("data/tidydata/master_size.csv") 

# combine these datasets together

HE_amy_size_15P <- left_join(data_15P_HE_amy, master_size) 

write_csv(HE_amy_size_15P, "data/tidydata/HE_amy_size_15P.csv")


#                                                             ** add fitted parameters (Weibull, fit for each sample) **

parameters <- read_csv("analysis/fitted_Weibull_parameters_15P.csv")

HE_amy_size_para_15P <- left_join(HE_amy_size_15P, parameters)

write_csv(HE_amy_size_para_15P, "results/HE_amy_size_para_15P.csv")

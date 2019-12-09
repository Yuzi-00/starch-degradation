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


#                               ** add fitted parameters (Weibull, fit for each sample) **

parameters <- read_csv("analysis/fitted_Weibull_parameters_15P.csv")

HE_amy_size_para_15P <- left_join(HE_amy_size_15P, parameters)

write_csv(HE_amy_size_para_15P, "results/HE_amy_size_para_15P.csv")




#                                               ** add RVA data **




# import the RVA dataset

RVA_data <- read_csv("data/tidydata/previous_data/RVA_tidy.csv")

# merge the RVA data into this dataset

joined_15P <- left_join(HE_amy_size_para_15P, RVA_data)

# write out the new data frame

write_csv(joined_15P, "data/tidydata/joined_15P.csv")




#                                                ** add CLD data **

# import the CLD dataset

CLD_data <- read_csv("data/tidydata/previous_data/CLD_tidy.csv") %>% 
  rename(ID = id) ## change the column name to be consistent with other datasets

# combien the CLD data with the previous joined dataset

joined_15P_update <- left_join(joined_15P, CLD_data)

# write out the new joined dataset

write_csv(joined_15P_update, "data/tidydata/joined_15p_update.csv")


#                                                ** add alpha-amylase data **


# import the amylase dataset

amylase <- read_csv("data/tidydata/previous_data/amylase_tidy.csv")

# combien the amylase data with the previous joined dataset

joined_15P_update <- left_join(joined_15P_update, amylase)

# write out the new joined dataset

write_csv(joined_15P_update, "data/tidydata/joined_15p_update.csv")



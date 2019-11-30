library(tidyverse)

data_6P_HE <- read_csv("data/tidydata/data_6P_cal_3nd.csv")
# read in the tidy data for the first six plates (before calculating)


################################################## add amylose content ######################################################


amy <- read_csv("data/tidydata/previous_data/amy_ID_sample.csv")
# read in the tidy data for the amylose content

data_6P_HE_amy <- left_join(data_6P_HE, amy) %>% # combining the data together
  rename(Hydro_extent = HE, Amylose_content = mean_amy) # change the column name

write_csv(data_6P_HE_amy, "data/tidydata/HE_amy_6P.csv")

# need to add the Amy% to C+ and C- (replace the NA) !!!!!!!!!!!!!!!!!!!!!!!


##################################################### add master size #######################################################


master_size <- read_csv("data/tidydata/master_size.csv") # read in the master size dataset

HE_amy_size_6P <- left_join(data_6P_HE_amy, master_size) # combine these dataset together

write_csv(HE_amy_size_6P, "data/tidydata/HE_amy_size_6P.csv")


#################################################### add fitted parameters ###################################################

parameters <- read_csv("analysis/fitted_parameters_by_ID.csv")

HE_amy_size_para_6P <- left_join(HE_amy_size_6P, parameters)

write_csv(HE_amy_size_para_6P, "results/HE_amy_size_para_6P_by_ID.csv")

################################################### add amylose content for C+ C- ############################################

# select the C+ first

pos_control <- HE_amy_size_para_6P %>% 
  filter(Sample == "C+") %>% 
  select()
  
replace()  


is.na(Amylose_content) = 100

## to be finished..........


################################################## add RVA data ###################################################


# import the RVA dataset

RVA_data <- read_csv("data/tidydata/previous_data/RVA_tidy.csv")

# merge the RVA data into this dataset

joined_6P <- left_join(HE_amy_size_para_6P, RVA_data)

# write out the new data frame

write_csv(joined_6P, "data/tidydata/joined_6P_by_ID.csv")

################################################## add CLD data ###################################################

# import the CLD dataset

CLD_data <- read_csv("data/tidydata/previous_data/CLD_tidy.csv") %>% 
  rename(ID = id) ## change the column name to be consistent with other datasets

# combien the CLD data with the previous joined dataset

joined_6P_update <- left_join(joined_6P, CLD_data)

# write out the new joined dataset

write_csv(joined_6P_update, "data/tidydata/joined_6p_update_by_ID.csv")


# to be continued after tidying the other dataset separately first




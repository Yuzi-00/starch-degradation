
library(tidyverse)

library(readxl)

# import the raw data

amylase <- read_xls("C:/Users/WAN333/Documents/Thesis/Thesis infomation/MAGIC population/Data_MAGIC Population/alpha amylase assays 24-01-11.xls",
                    sheet = "total data",
                    range = "A1:D753")

# remove the empty column and rename the column

amylase <- amylase %>% 
  select(-'...2') %>% 
  rename(ID = '...1', CU_mg_flour = 'CU/mg flour', CU_g_flour = 'CU/g flour')

# calculate the mean values of for each ID

amylase_tidy <- amylase %>% 
  group_by(ID) %>% 
  summarise(mean_amylase = mean(CU_g_flour, na.rm = TRUE))

# save the tidy dataset

write_csv(amylase_tidy, "data/tidydata/previous_data/amylase_tidy.csv")

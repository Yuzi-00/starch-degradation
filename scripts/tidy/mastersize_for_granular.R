library(tidyverse)

library(readxl)

# read in the raw data

raw_data <- read_xls("data/magic_population/MastersizerMagic.xls",
                     sheet = "Total data",
                     range = "A2:DH515")


df <- raw_data[-1,] # remove the first empty row

# select the useful columns 

df_new <- df %>%
  select(-id) %>%
  select(-(2:11)) %>%
  rename(sample = "Sample Name")

# calculate the mean value for each sample 

df_mean <- df_new %>%
  group_by(sample) %>%
  summarise_all(mean)

# save the dataset

write_csv(df_mean, "data/tidydata/mastersize_granular.csv")

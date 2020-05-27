library(tidyverse)

library(janitor)

# read in the raw data

raw_data <- read_csv("data/magic_population/MastersizerMagic_granular.csv")

# calculate the mean value for each sample 

df <- raw_data %>%
  group_by(Sample) %>%
  summarise_all(mean) 

# save the dataset

write_csv(df, "data/tidydata/mastersize_granular.csv")

# this dataset needs further select for the useful columns  
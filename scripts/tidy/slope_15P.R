library(tidyverse)

library(readxl)

# read in the raw data

slope <- read_xlsx("C:/Users/WAN333/Documents/Thesis/Experiments/raw_data/slope_15P.xlsx")

# gather the Days columns in order to calculate the mean values

slope_gathered <- slope %>% 
  gather(Day, Slope, -Plate)

# calculate the mean values by plate

slope_15P <- slope_gathered %>% 
  group_by(Plate) %>% 
  mutate(mean_slope = mean(Slope)) %>% 
  arrange(Plate) %>% 
  select(Plate, mean_slope) %>% # select just the Plate and the mean_slope columns
  unique() # remove the duplicated rows

# save the slope dataset

write_csv(slope_15P, "data/tidydata/slope_15P.csv")

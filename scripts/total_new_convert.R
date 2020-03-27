
library(tidyverse)

df <- read_csv("analysis/total_new.csv")

df_int <- df %>%
  select(-(1:6), -(11:21)) %>% # remove the unseful columns 
  mutate(Sample = as.character(Sample)) %>% # change the sample column into charactors
  group_by(Well, Time) %>%
  summarise_if(is.numeric, mean) # calculte the average for all columns


# split the time column into 9 separate columns (each for one time point)

df_conv <- df_int %>%
  spread(key = Time, value = Hydro_extent) %>% # divide the time into different columns
  rename(HE_0min = "0", HE_20min = "20", HE_60min = "60", HE_120min = "120",
         HE_180min = "180", HE_240min = "240", HE_360min = "360", HE_1440min = "1440",
         HE_1800min = "1800") # rename the time columns 

# extract the sample names and IDs

ident <- df %>%
  select(Well, Sample, ID) %>%
  unique()

# add the sample names and IDs into the converted dataset

df_conv <- left_join(df_conv, ident)

# add a column to distinct parents and descendants

df_conv <- df_conv %>%
  mutate(Category = case_when(
    ID == "Baxter" ~ "parent",
    ID == "Chara" ~ "parent",
    TRUE ~ "descendant"))

# ordering the columns 

df_conv <- df_conv %>%
  select(Well, Sample, ID, Category, Amylose_content, SSA, Surface_weighted_mean, D1, D5, D9,
         mean_Peak, mean_Trough, mean_Final, mean_PastingTemp, low_dp, medium_dp, 
         medium_high_dp, high_dp, mean_amylase, HE_0min, HE_20min, HE_60min, HE_120min,
         HE_180min, HE_240min, HE_360min, HE_1440min, HE_1800min, k, h, Xinf) 

# save the dataset

write_csv(df_conv, "analysis/total_new_convert.csv")



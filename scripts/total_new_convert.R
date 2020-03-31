
library(tidyverse)

df <- read_csv("analysis/total_new.csv")

# remove the unused Well

df <- df %>%
  filter(WellGroupType != "Unused")

# about 82 and 82*. It should be noted that 82* = 81 (we do not have 81 in the design, 
# all is named 82*). To clarify, 82 are in the Well 1B7, 11A7, and 4A7. 82* are in the Well 14E11,
# 15E7 and 15H1. As for the Well 5A9 and 7G9, please remove them as the 82 and 82* might be mixed here

df <- df %>%
  filter(Well != "5_A_9" & Well != "7_G_9") # remove the unclear samples 
## (not sure if they are 82 or 82*)
  
# for the sample 82*, their sample name were recognized as NA in the dataset, let's change them into
# 82* now

# we firstly make sure that the NAs are all correspond to the sample 82* (14E11, 15E7 and 15H1)

missing <- df %>%
  filter(is.na(Sample)) # good, everything's all right

# replace the NAs within the Sample column by 82*

df$Sample <- as.character(df$Sample)

df$Sample[is.na(df$Sample)] <- "81" # if changing into 82*, will bring some problem when open this 
# dataset

# let's have a check

sample82star <- df %>%
  filter(Sample == "81") # great !

# continue tidy the dataset

df_int <- df %>%
  select(-(Plate:WellGroupType), -(Mass_sample:C), -(mean_HE_2h:mean_HE_30h)) %>% # remove the unseful columns 
  mutate(Sample = as.character(Sample)) # change the sample column into charactors
  # group_by(Well, Time) %>%
  # summarise_if(is.numeric, mean) # calculte the average for all columns

# extract the data of the control samples 

dfc <- read_csv("data/tidydata/joined_15p_update.csv")

control <- dfc %>%
  filter(Sample == "C+" | Sample == "C-") %>%
  select(Well, Sample, Time, Hydro_extent, H, k, Xinf) %>%
  mutate(h = 1-H) %>%
  select(-H)

# combine the control samples with the test samples 

df_int <- full_join(df_int, control)

# add a column to distinct parents and descendants

df_int <- df_int %>%
  mutate(Category = case_when(
    ID == "Baxter" ~ "parent_Baxter",
    ID == "Chara" ~ "parent_Chara",
    ID == "Westonia" ~ "parent_Westonia",
    Sample == "C+" ~ "pos_control",
    Sample == "C-" ~ "neg_control",
    TRUE ~ "descendant"))

# save the dataset before converting 

write_csv(df_int, "analysis/total_new_update.csv")

# split the time column into 9 separate columns (each for one time point)

df_conv <- df_int %>%
  spread(key = Time, value = Hydro_extent) %>% # divide the time into different columns
  rename(HE_0min = "0", HE_20min = "20", HE_60min = "60", HE_120min = "120",
         HE_180min = "180", HE_240min = "240", HE_360min = "360", HE_1440min = "1440",
         HE_1800min = "1800") # rename the time columns

# ordering the columns 

df_conv <- df_conv %>%
  select(Well, Sample, ID, Category, Amylose_content, SSA, Surface_weighted_mean, D1, D5, D9,
         mean_Peak, mean_Trough, mean_Final, mean_PastingTemp, low_dp, medium_dp, 
         medium_high_dp, high_dp, mean_amylase, HE_0min, HE_20min, HE_60min, HE_120min,
         HE_180min, HE_240min, HE_360min, HE_1440min, HE_1800min, k, h, Xinf) 

# change the column names

df_conv <- df_conv %>%
  rename(Amylose_Con = Amylose_content, D0.1 = D1, D0.5 = D5, D0.9 = D9, DP6_12 = low_dp, 
         DP13_24 = medium_dp, DP25_36 = medium_high_dp, DP37_47 = high_dp, Peak_Vis = mean_Peak,
         Trough_Vis = mean_Trough, Final_Vis = mean_Final, Pasting_Temp = mean_PastingTemp,
         Amylase_Act = mean_amylase, SWM = Surface_weighted_mean)

# save the dataset

write_csv(df_conv, "analysis/total_new_convert.csv")



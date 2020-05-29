
library(tidyverse)

df <- read_csv("analysis/total_new_convert.csv") %>%
  select(Sample, h, k, Xinf)

# select the test samples

test_sample <- df %>%
  filter(Sample != "C+" & Sample != "C-")

# select the positive controls

pos_control <- df %>%
  filter(Sample == "C+")

# select the negative controls

neg_control <- df %>%
  filter(Sample == "C-")

# calculate the standard deviation for all the test samples

test_sample %>%
  mutate(Sample = as.character(Sample)) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) 
## change the mean into sd for calculate sd 
## mean_h = 0.0261; mean_k = 0.00343; mean_Xinf = 73.9
## sd_h = 0.09; sd_k = 0.00163; sd_Xinf = 4.33

# calculate the standard deviation for all the positive controls

pos_control %>%
  mutate(Sample = as.character(Sample)) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) 
## change the mean into sd for calculate sd 
## mean_h = -0.181; mean_k = 0.00281; mean_Xinf = 82.6
## sd_h = 0.139; sd_k = 0.00205; sd_Xinf = 3.58

# calculate the standard deviation for all the negative controls

neg_control %>%
  mutate(Sample = as.character(Sample)) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) 
## change the mean into sd for calculate sd 
## mean_h = 0.187; mean_k = 0.0110; mean_Xinf = 57.0
## sd_h = 0.0679; sd_k = 0.00408; sd_Xinf = 3.19
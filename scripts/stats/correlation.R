
library(tidyverse)

library(corrplot)

# import the dataset

total <- read_csv("data/tidydata/joined_15P_update.csv") %>% 
  filter(Sample != "C+" & Sample != 'C-') # remove the controls

# add a new column h

total_new <- total %>% 
  mutate(h = 1 - H) %>% 
  select(-H) # remove the H column

# choose a subset

my_subset <- total_new %>% 
  select(-(1:22)) %>% 
  unique()

cor_result <- cor(my_subset, use = "complete.obs")

correlation <- corrplot(cor_result, method = "number")
# calculate the mean value


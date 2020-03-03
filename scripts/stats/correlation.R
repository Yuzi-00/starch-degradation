
library(tidyverse)

install.packages("corrplot") 

library(corrplot)

# import the dataset

total <- read_csv("data/tidydata/joined_15P_update.csv") %>% 
  filter(Sample != "C+" & Sample != 'C-') # remove the controls

# select some subsets of HE at different time points

HE_2h <- total %>% 
  filter(Time == 120) %>% 
  select(ID, Sample, Hydro_extent) %>% 
  rename(HE_2h = Hydro_extent) %>% 
  group_by(Sample) %>% 
  summarise(mean_HE_2h = mean(HE_2h))

HE_6h <- total %>% 
  filter(Time == 360) %>% 
  select(ID, Sample, Hydro_extent) %>% 
  rename(HE_6h = Hydro_extent) %>% 
  group_by(Sample) %>% 
  summarise(mean_HE_6h = mean(HE_6h))

HE_30h <- total %>% 
  filter(Time == 1800) %>% 
  select(ID, Sample, Hydro_extent) %>% 
  rename(HE_30h = Hydro_extent) %>% 
  group_by(Sample) %>% 
  summarise(mean_HE_30h = mean(HE_30h))

# combine these datasets together

HE_half <- full_join(HE_2h, HE_6h)

HE_full <- full_join(HE_half, HE_30h)

# add HE_full back to the total dataset

total_add_HE <- left_join(total, HE_full)

# add a new column h

total_new <- total_add_HE %>% 
  mutate(h = 1 - H) %>% 
  select(-H) # remove the H column

# calculate the mean values by ID

total_update <- total_new %>% 
  group_by(ID) %>% 
  summarise_each(funs = mean)

# correlation plot

# choose a subset

my_subset <- total_update %>% 
  select(-(1:22)) %>% # remove the first 22 columns
  unique()

cor_result <- cor(my_subset, use = "complete.obs")

correlation <- corrplot(cor_result, method = "number",
                        tl.cex = 0.5, number.cex = 0.5) # tl.cex:change the size of table; number.cex:change the size of number

# save the corrplot

pdf(file = "corrplot.pdf")

corrplot(correlation, method = "number", type = "lower", tl.cex = 0.5, 
         title = "relationships", 
         mar = c(0,0,1,0), number.cex = 0.5, number.digits = 2)

dev.off()


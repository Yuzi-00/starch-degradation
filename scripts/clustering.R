
library(tidyverse)

# import the dataset 

total_new <- read_csv("analysis/total_new.csv")

# select the columns 

subdata <- total_new %>% 
  select(ID, Amylose_content, SSA, Surface_weighted_mean, D1, D5, D9, k, Xinf, mean_Peak, mean_Trough,
         mean_Final, mean_PastingTemp, low_dp, medium_dp, medium_high_dp, high_dp, mean_amylase, h)

# remove the NAs

subdata_red <- subdata %>% 
  unique() %>% 
  na.omit()

# remove the Sample column

subdata_num <- subdata_red %>% 
  select(-ID)

# normalization

subdata_nor <- scale(subdata_num)

# calculate the distance 

distance <- dist(subdata_nor)

# dentrogram

hc <- hclust(distance)

plot(hc)

#########################################################################################

# grouping

results <- kmeans(subdata_nor, 5) # choose the number of groups

table(subdata_red$ID, results$cluster)



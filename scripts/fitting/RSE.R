
library(tidyverse)

# import the dataset

rse <- read_csv("analysis/RSE_weibull.csv") 

# add the sample column into the residual_data dataset

df <- read_csv("data/tidydata/joined_15P_update.csv") %>%
  select(Well, Sample) %>%
  unique()

rse <- left_join(rse, df)
  
#### RSE for test samples ####

# calculate the mean value of rse for the whole dataset

rse1 <- rse %>%
  filter(Sample != "C+" & Sample != "C-") 

rse1 %>%
  summarise(mean = mean(sigma, na.rm = TRUE), sd = sd(sigma, na.rm = TRUE)) 
# mean = 1.29, sd = 0.575

boxplot(rse1$sigma)

#### RSE for positive control ####

# calculate the mean value of rse for the whole dataset

rse2 <- rse %>%
  filter(Sample == "C+") 

rse2 %>%
  summarise(mean = mean(sigma, na.rm = TRUE), sd = sd(sigma, na.rm = TRUE)) 
# mean = 2.04, sd = 0.761
boxplot(rse2$sigma)

#### RSE for negative control ####

# calculate the mean value of rse for the whole dataset

rse3 <- rse %>%
  filter(Sample == "C-") 

rse3 %>%
  summarise(mean = mean(sigma, na.rm = TRUE), sd = sd(sigma, na.rm = TRUE)) 
# mean = 1.36, sd = 0.432

boxplot(rse3$sigma)
                                             
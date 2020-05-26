
library(tidyverse)

# import the dataset

rse <- read_csv("analysis/RSE_weibull.csv")

# calculate the mean value of rse for the whole dataset

rse %>%
  summarise(mean = mean(sigma, na.rm = TRUE), sd = sd(sigma, na.rm = TRUE),
            se = sd/sqrt(680)) # sqrt: radical sign
                              # mean = 1.31, sd = 0.586, se = 0.0225

boxplot(rse$sigma)
                                             
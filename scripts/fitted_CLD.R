
library(tidyverse)

# install.packages('stringr')

library(stringr)

# read in the fitted CLD data

fitted_CLD <- read_csv("data/fittedCLD_all_tidy.csv")

# remove the first letter of the sample name column

sample_name <- substr(fitted_CLD$Sample.ID, 2, 4)

# convert it into a data frame

sample_name <- as.data.frame(sample_name)

# remove the first column of the raw dataset

fitted_CLD_rem <- fitted_CLD %>%
  select(-Sample.ID)

# combine the above dataset with the sample name (first letter removed)

fitted_CLD_tidy <- bind_cols(sample_name, fitted_CLD_rem)

# remove the last row (J10)

fitted_CLD_tidy <- fitted_CLD_tidy %>%
  filter(!sample_name == "J10")

# select the useful columns

fitted_CLD_sel <- fitted_CLD_tidy %>%
  select(sample_name, beta.i, gamma.i, beta.ii, gamma.ii, h.ii.i) %>%
  na.omit() %>% # remove the rows that contained NAs 
  rename(Sample = sample_name)

# save the dataset

write_csv(fitted_CLD_sel, "data/tidydata/fitted_CLD_tidy.csv")

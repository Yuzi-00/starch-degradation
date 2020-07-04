
library(tidyverse)

df <- read_csv("analysis/total_new_convert.csv")

df2 <- read_csv("analysis/granular_output_final_tidy.csv") %>%
  rename(Sample = sample) %>%
  mutate(Sample = as.character(Sample))

# combine the above two datasets together

df3 <- full_join(df, df2)

# save the dataset

write_csv(df3, "analysis/total_new_convert_add_granular.csv")

# remove the previously used size parameters

df4 <- df3 %>%
  select(-SSA, -VWM, -SWM, -D0.1, -D0.5, -D0.9, -A_granule, -B_granule, -AB_ratio)

# save the dataset

write_csv(df4, "analysis/total_new_convert_replaced_by_granular.csv")

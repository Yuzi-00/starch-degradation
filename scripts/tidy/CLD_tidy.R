library(tidyverse)

# import the CLD dataset

CLD_raw <- read_csv("C:/Users/WAN333/Documents/Thesis/Thesis infomation/MAGIC population/Data_MAGIC Population/CEresults.csv")

# calculate the mean values

CLD_mean <- CLD_raw %>% 
  group_by(id) %>% 
  summarise_each(funs = mean)

# get the summary for the low DP (DP 6-12)

low_dp <- CLD_mean %>%
  select(id, dp6:dp12) %>% ## select just the id and the dp6 to dp12 columns
  gather(dp, results, -id) %>% ## gather all the dp to one single column
  group_by(id) %>% 
  summarise(low_dp = sum(results)) ## calculate the sum of all the dp6 to dp12

# get the summary for the medium DP (DP 13-24)

medium_dp <- CLD_mean %>% 
  select(id, dp13:dp24) %>% ## select just the id and the dp13 to dp24 columns
  gather(dp, results, -id) %>% ## gather all the dp to one single column
  group_by(id) %>% 
  summarise(medium_dp = sum(results)) ## calculate the sum of all the dp13 to dp24

# get the summary for the medium to high DP (DP 25-36)

medium_high_dp <- CLD_mean %>% 
  select(id, dp25:dp36) %>% ## select just the id and the dp13 to dp24 columns
  gather(dp, results, -id) %>% ## gather all the dp to one single column
  group_by(id) %>% 
  summarise(medium_high_dp = sum(results)) ## calculate the sum of all the dp25 to dp36

# get the summary for the high DP (DP 37-47)

high_dp <- CLD_mean %>% 
  select(id, dp37:dp47) %>% ## select just the id and the dp13 to dp24 columns
  gather(dp, results, -id) %>% ## gather all the dp to one single column
  group_by(id) %>% 
  summarise(high_dp = sum(results)) ## calculate the sum of all the dp37 to dp47

# combine all these dp together

first_join <- full_join(low_dp, medium_dp) ## join the first two tibbles together

second_join <- full_join(first_join, medium_high_dp) ## join the above joined tibble with the third tibble

CLD_tidy <- full_join(second_join, high_dp) ## join the above joined tibble with the fourth tibble

# write out the tidy data of CLD

write_csv(CLD_tidy, "data/tidydata/previous_data/CLD_tidy.csv")

## noted that there are some missing values in this dataset

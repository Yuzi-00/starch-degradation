
library(tidyverse)

df <- read_csv("analysis/total_new_convert_add_granular.csv")

# select the granule proportions

df2 <- df %>%
  select(Sample, AB_ratio, pi_AB_ratio) %>%
  unique()

# plot



# plot

p <- df2 %>%
  filter(!Sample %in% c("C+", "C-")) %>%
  ggplot(aes(x = reorder(Sample, pi_AB_ratio),
             y = pi_AB_ratio)) +
  geom_bar(stat = 'identity') 

p

p2 <- df2 %>%
  filter(!Sample %in% c("C+", "C-")) %>%
  ggplot(aes(x = reorder(Sample, AB_ratio),
             y = AB_ratio)) +
  geom_bar(stat = 'identity') 

p2

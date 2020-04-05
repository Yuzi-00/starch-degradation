
library(tidyverse)

df <- read_csv("analysis/total_new_convert.csv")

# select h<0 

df_h <- df %>%
  filter(h < 0) %>%
  arrange(Amylose_Con)

# select h>0

df_h_02 <- df %>%
  filter(h > 0) %>%
  arrange(Amylose_Con)

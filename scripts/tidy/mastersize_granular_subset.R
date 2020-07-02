
library(tidyverse)

df <- read_csv("data/tidydata/mastersize_granular.csv")

# extract all the samples that were not well fitted during the 1st round of fitting 
# 96 samples were not well fitted in total

df2 <- df %>%
  filter(sample %in% c(4, 12, 14, 17, 18, 20, 21, 22, 25, 27, 28, 29, 30, 32, 35, 36, 37, 41,
                     44, 47, 57, 58, 59, 60, 63, 69, 73, 74, 76, 78, 79, 80, 83, 84, 89, 90, 91,
                     93, 95, 96, 97, 98, 100, 101, 102, 104, 109, 116, 117, 122, 127, 136, 144,
                     146, 151, 153, 154, 155, 156, 157, 158, 161, 163, 164, 165, 166, 167, 168,
                     170, 172, 176, 177, 178, 180, 182, 185, 187, 188, 189, 191, 192, 193, 194,
                     197, 198, 199, 200, 202, 203, 206, 208, 211, 217, 218, 219, 220))

write_csv(df2, "data/tidydata/mastersize_granular_subset01.csv")

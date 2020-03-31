
library(tidyverse)

library(plotly)

# import the dataset

df <- read_csv("analysis/total_new_convert.csv")

# plot

df %>%
  plot_ly(x = ~h, y = ~k, 
          alpha = 0.6,
          color = ~Category, 
          colors = "Dark2")

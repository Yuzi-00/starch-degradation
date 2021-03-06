
library(tidyverse)

library(plotly)

# import the dataset

df <- read_csv("analysis/total_new_convert.csv")

#### plotting ####

# coloring by category

df %>%
  plot_ly(x = ~k, y = ~h, z = ~Xinf,
          alpha = 0.3,
          color = ~Category, colors = c('blue', 'red', 'green', 'orange')
)

# coloring by other properties

marker <- list(color = ~SSA, colorscale = c('#FFE1A1', '#683531'), 
               # colored by SSA or other features
               showscale = TRUE)

df %>%
  plot_ly(x = ~k, y = ~h, z = ~Xinf,
          marker = marker) 

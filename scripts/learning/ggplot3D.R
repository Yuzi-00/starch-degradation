
library(tidyverse)

library(plotly)

#### data preperation ####

df <- mtcars %>%
  rownames_to_column() %>% # change the rownames into column names 
  as_tibble() %>%
  mutate(am = ifelse(am == 0, "Automatic", "Manual")) %>%
  # if am=0, replace by "automatic", otherwise, replace by "manual"
  mutate(am = as.factor(am))

df

#### 3D Scatter Plot ####

p <- plot_ly(
  df, x = ~wt, y = ~hp, z = ~qsec, 
  color = ~am, colors = c('#BF382A', '#0C4B8E')
) %>%
  add_markers() %>% # seems not necessary here
  layout(
    scene = list(xaxis = list(title = 'Weight'),
                 yaxis = list(title = 'Gross horsepower'),
                 zaxis = list(title = '1/4 mile time'))
    # change the names of each axis
  )

p

#### 3D Scatter Plot with Color Scaling ####

# Point colors

marker <- list(color = ~mpg, colorscale = c('#FFE1A1', '#683531'), 
               showscale = TRUE)

# Create the plot
p <- plot_ly(df, x = ~wt, y = ~hp, z = ~qsec, marker = marker) %>%
  add_markers() %>%
  layout(
    scene = list(xaxis = list(title = 'Weight'),
                 yaxis = list(title = 'Gross horsepower'),
                 zaxis = list(title = '1/4 mile time'))
  )

p

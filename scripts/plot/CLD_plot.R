
library(tidyverse)

library(readxl)

# import the dataset

df <- read_xlsx("data/magic_population/CLD_for_plot.xlsx")

# gathering the dataset

dfg <- df %>%
  gather(x, values, -dp) %>%
  na.omit() %>%
  mutate(values = as.numeric(values)) 

# plot

CLD <- ggplot(data = dfg,
             aes(x = dp,
                 y = values,
                 group = x)) +
  geom_line(color = "#4682B4", alpha = 0.5) +
  theme(legend.position = "none") +
  labs(x = "Particle size (μm)", y = "Volume (%)") +
  theme(axis.text.x = element_text(color="black", size=17), 
        axis.text.y = element_text(color="black", size=17)) +
  # change the color and size of the tick label for x and y axis
  theme(plot.margin = unit(c(5.5,12,5.5,5.5), "pt")) +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20)) +
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.ticks=element_line(
          colour="black",
          size=.5)) +
  scale_x_continuous(limits = c(6,47), expand = c(0,0)) 

CLD

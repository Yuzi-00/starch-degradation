
library(tidyverse)

library(readxl)

# import the daraset

df <- read_xlsx("data/201900801_DNS result.xlsx",
          sheet = "comparaison37_45℃",
          range = "B14:F28")

# change the column name

df <- df %>%
  rename(Temperature = temperature)

# gathering the samples

df_new <- df %>%
  gather(Sample, RS, -Temperature, - Time)

# calculate the hydrolysis extent

df_new <- df_new %>%
  mutate(HE = RS/11.1*100,
         Temperature = as.factor(Temperature))
  

# plotting

ggplot(data = df_new, 
                         aes(x = Time, 
                             y = RS,
                             shape = Sample,
                             color = Temperature)) + # colored by the status (distinguish the control from the other samples)
  geom_point(size = 1) + # add the transparency
  geom_line(aes(group = interaction(Sample,Temperature))) +
  scale_y_continuous(limits = c(0,10), expand = c(0, 0)) + ## set the range of the y axis
  scale_x_continuous(limits = c(0, 2000), expand = c(0, 0)) +
  ylab("Hydrolysis extent (%)") + ## change the label for the y axis
  xlab("Time (min)") + ## change the name of the x axis
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.ticks=element_line(
          colour="black",
          size=.5)) +
  labs(x = "Time (min)", y = "Hydrolysis extent (%)") +
  theme(axis.text.x = element_text(color="black", size=10), 
        axis.text.y = element_text(color="black", size=10)) +
  theme(legend.key = element_blank(),
        legend.position = "bottom") +
  theme(plot.margin = unit(c(5.5,12,5.5,5.5), "pt"))

# save the plot

ggsave("figures/scatter-plot_15P.png", 
       plot = HE_scatter_15P, 
       width = 10, 
       height = 8, 
       units = "cm") 

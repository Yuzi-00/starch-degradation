library(tidyverse)

library(janitor)

# read in the raw data

raw_data <- read_csv("data/magic_population/MastersizerMagic_granular.csv")

# calculate the mean value for each sample 

df <- raw_data %>%
  group_by(Sample) %>%
  summarise_all(mean) %>%
  select(-(2:11), -112) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  rename(sample = Sample)

# save the dataset

write_csv(df, "data/tidydata/mastersize_granular.csv")

# transpose the dataset

dft <- df %>%
  gather(diameter, volume_intensity, -sample, ) %>%
  mutate(diameter = as.numeric(diameter))

# plot 

ps <- ggplot(data = dft,
       aes(x = diameter,
           y = volume_intensity,
           group = sample)) +
  geom_line(color = "#4682B4", alpha = 0.5) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(0.1,10000), expand = c(0,0)) +
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
  scale_y_continuous(limits = c(0,18), expand = c(0,0)) 

ps

# save the plot

ggsave("figures/particle_size_distribution.png", 
       plot = ps,
       width = 25, 
       height = 20, 
       units = "cm") 

library(tidyverse)

# import the dataset

data_15P <- read_csv("data/tidydata/data_15P_outlier_deleted.csv")

# select the colmuns needed

data_selected <- data_15P %>% 
  filter(Sample != "C+" & Sample != "C-") %>%  # remove the controls
  select(Plate, Row, Column, Time, HE) # select the column needed

# choose a time point and remove the "outlier"

# 0min

data_0 <- data_selected %>% 
  filter(Time == 0)

### plotting

data_0min <- data_0 %>% 
  ggplot(aes(x = Column, y = Row, fill = HE)) + 
  scale_fill_continuous(name = "Hydrolysis extent", type = "viridis") + # set the legend title and the fill 
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11)) +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.15, "cm"),
        legend.key.width = unit(0.8, "cm")) +
  geom_tile(color = "white", size = 0.1) +
  facet_wrap(~Plate) +
  theme(strip.background = element_blank())

ggsave("figures/heatmap_0min.png", 
       plot = data_0min, 
       width = 12, 
       height = 17, 
       units = "cm")


# 20min

data_20 <- data_selected %>% 
  filter(Time == 20) 
### plotting

data_20min <- data_20 %>% 
  ggplot(aes(x = Column, y = Row, fill = HE)) + 
  scale_fill_continuous(name = "Hydrolysis extent", type = "viridis") + # set the legend title and the fill 
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11)) +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.15, "cm"),
        legend.key.width = unit(0.8, "cm")) +
  geom_tile(color = "white", size = 0.1) +
  facet_wrap(~Plate) +
  theme(strip.background = element_blank())

ggsave("figures/heatmap_20min.png", 
       plot = data_20min, 
       width = 12, 
       height = 17, 
       units = "cm")


# 60min

data_60 <- data_selected %>% 
  filter(Time == 60) 

### plotting

data_60min <- data_60 %>% 
  ggplot(aes(x = Column, y = Row, fill = HE)) + 
  scale_fill_continuous(name = "Hydrolysis extent", type = "viridis") + # set the legend title and the fill 
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11)) +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.15, "cm"),
        legend.key.width = unit(0.8, "cm")) +
  geom_tile(color = "white", size = 0.1) +
  facet_wrap(~Plate) +
  theme(strip.background = element_blank())

ggsave("figures/heatmap_60min.png", 
       plot = data_60min, 
       width = 12, 
       height = 17, 
       units = "cm")


# 120min

data_120 <- data_selected %>% 
  filter(Time == 120) 

### plotting

data_120min <- data_120 %>% 
  ggplot(aes(x = Column, y = Row, fill = HE)) + 
  scale_fill_continuous(name = "Hydrolysis extent", type = "viridis") + # set the legend title and the fill 
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11)) +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.15, "cm"),
        legend.key.width = unit(0.8, "cm")) +
  geom_tile(color = "white", size = 0.1) +
  facet_wrap(~Plate) +
  theme(strip.background = element_blank())

ggsave("figures/heatmap_120min.png", 
       plot = data_120min, 
       width = 12, 
       height = 17, 
       units = "cm")


# 180min

data_180 <- data_selected %>% 
  filter(Time == 180) 
### plotting

data_180min <- data_180 %>% 
  ggplot(aes(x = Column, y = Row, fill = HE)) + 
  scale_fill_continuous(name = "Hydrolysis extent", type = "viridis") + # set the legend title and the fill 
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11)) +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.15, "cm"),
        legend.key.width = unit(0.8, "cm")) +
  geom_tile(color = "white", size = 0.1) +
  facet_wrap(~Plate) +
  theme(strip.background = element_blank())

ggsave("figures/heatmap_180min.png", 
       plot = data_180min, 
       width = 12, 
       height = 17, 
       units = "cm")

# 240min

data_240 <- data_selected %>% 
  filter(Time == 240) 

### plotting

data_240min <- data_240 %>% 
  ggplot(aes(x = Column, y = Row, fill = HE)) + 
  scale_fill_continuous(name = "Hydrolysis extent", type = "viridis") + # set the legend title and the fill 
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11)) +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.15, "cm"),
        legend.key.width = unit(0.8, "cm")) +
  geom_tile(color = "white", size = 0.1) +
  facet_wrap(~Plate) +
  theme(strip.background = element_blank())

ggsave("figures/heatmap_240min.png", 
       plot = data_240min, 
       width = 12, 
       height = 17, 
       units = "cm")

# 360min

data_360 <- data_selected %>% 
  filter(Time == 360)

### plotting

data_360min <- data_360 %>% 
  ggplot(aes(x = Column, y = Row, fill = HE)) + 
  scale_fill_continuous(name = "Hydrolysis extent", type = "viridis") + # set the legend title and the fill 
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11)) +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.15, "cm"),
        legend.key.width = unit(0.8, "cm")) +
  geom_tile(color = "white", size = 0.1) +
  facet_wrap(~Plate) +
  theme(strip.background = element_blank())

ggsave("figures/heatmap_360min.png", 
       plot = data_360min, 
       width = 12, 
       height = 17, 
       units = "cm")

# 1440min

data_1440 <- data_selected %>% 
  filter(Time == 1440) 

### plotting

data_1440min <- data_1440 %>% 
  ggplot(aes(x = Column, y = Row, fill = HE)) + 
  scale_fill_continuous(name = "Hydrolysis extent", type = "viridis") + # set the legend title and the fill 
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11)) +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.15, "cm"),
        legend.key.width = unit(0.8, "cm")) +
  geom_tile(color = "white", size = 0.1) +
  facet_wrap(~Plate) +
  theme(strip.background = element_blank())

ggsave("figures/heatmap_1440min.png", 
       plot = data_1440min, 
       width = 12, 
       height = 17, 
       units = "cm")

# 1800min

data_1800 <- data_selected %>% 
  filter(Time == 1800)

### plotting

data_1800min <- data_1800 %>% 
  ggplot(aes(x = Column, y = Row, fill = HE)) + 
  scale_fill_continuous(name = "Hydrolysis extent", type = "viridis") + # set the legend title and the fill 
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11)) +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.15, "cm"),
        legend.key.width = unit(0.8, "cm")) +
  geom_tile(color = "white", size = 0.1) +
  facet_wrap(~Plate) +
  theme(strip.background = element_blank())

ggsave("figures/heatmap_1800min.png", 
       plot = data_1800min, 
       width = 12, 
       height = 17, 
       units = "cm")

# check the number of the observation

with(data_1800,table(Column, Row, Plate)) # one observation/col/row/per



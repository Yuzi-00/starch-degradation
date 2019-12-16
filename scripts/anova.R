
library(tidyverse)

# import the dataset 

total <- read_csv("data/tidydata/joined_15P_update.csv")

# calculate the mean value

total %>% 
  group_by(Sample, Time) %>%
  summarise(mean_HE = mean(Hydro_extent, na.rm = T))

total %>% 
  filter(Time == 1800) %>% 
  ggplot(aes(x = Sample,
             y = Hydro_extent,
             fill = as.factor(Amylose_content))) +
  geom_boxplot() +
  geom_jitter()

anova <- aov(data = total, Hydro_extent~Sample)
summary(anova)

x <- TukeyHSD(anova)




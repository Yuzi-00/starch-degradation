
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

# subset the potential outliers 

subset <- total %>%
  filter(Time == 1800) %>% 
  filter(Sample %in% c("195", "C-"))

anova <- aov(data = subset, Hydro_extent~Sample)
summary(anova)

TukeyHSD(anova)


t.test(data = subset, Hydro_extent~Sample)

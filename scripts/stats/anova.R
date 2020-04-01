
library(tidyverse)

#### impact of h and k on Xinf ####

df <- read_csv("analysis/total_new_convert.csv")

anova <- aov(data = df, 
             Xinf~h*k)

summary(anova)

TukeyHSD(anova)

#### impact of h and k on HE ####

anova <- aov(data = df, 
             HE_120min~h*k)

summary(anova)

TukeyHSD(anova)

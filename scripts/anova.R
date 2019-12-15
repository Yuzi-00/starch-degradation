total <- read_csv("data/tidydata/joined_15P_update.csv")

selected <- total %>% 
  filter(Sample %in% c("129", "143", "222")) %>% 
  filter(Time == 1800) %>% 
  filter(Well != "13_A_11")

sum_selected <- selected %>% 
  group_by(Sample, Time) %>%
  summarise(mean_HE = mean(Hydro_extent, na.rm = T),
            mean_amy = mean(Amylose_content, na.rm = T),
            sd_HE = sd(Hydro_extent, na.rm = T),
            sd_amy = sd(Amylose_content, na.rm = T))

selected %>% 
  filter(Time == 1800) %>% 
  ggplot(aes(x = Sample,
             y = Hydro_extent,
             fill = as.factor(Amylose_content))) +
  geom_boxplot() +
  geom_jitter()

anova <- aov(data = selected, Hydro_extent~Sample)
summary(anova)

TukeyHSD(anova)


total["Amylose_content"][is.na(total["Amylose_content"])] = 100

total$Amylose_content[is.na(total$Sample == "C+")] = 100

x <- total %>% 
  mutate(Amylose_content=ifelse(Sample=="C+",100,Amylose_content),
         Amylose_content=ifelse(Sample=="C-",72.4,Amylose_content))

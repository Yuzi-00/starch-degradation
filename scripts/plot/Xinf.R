
library(tidyverse)

# import the dataset

total <- read_csv("data/tidydata/joined_15P_update.csv")

# add a column h

total_new <- total %>% 
  mutate(h = 1 - H)



# plot h and k 

para <- total_new %>% 
  ggplot(aes(x = h,
             y = k,
             color = Xinf)) +
  geom_point(alpha = 0.1)

# save the plot

ggsave("figures/estimated parameters.png", 
       plot = para, 
       width = 15, 
       height = 15, 
       units = "cm") 

# plot amylose and the others

total_new %>% 
  filter(!(Sample %in% c("C+", "C-"))) %>%
  filter(Time == 1800) %>% 
  ggplot(aes(x = Hydro_extent,
             y = mean_amylase)) +
  geom_point()

# correlation 

total_new <- total_new %>% 
  filter(Sample != "C+" & Sample != "C-")

amy_HE <- cor.test(total_new$Amylose_content, total_new$Hydro_extent)

amy_Xinf <- cor.test(total_new$Amylose_content, total_new$Xinf)

amy_h <- cor.test(total_new$Amylose_content, total_new$h)

amy_k <- cor.test(total_new$Amylose_content, total_new$k)

pairs(total_new)

total_newnew <- total_new %>% 
  select(-(1:21)) %>% 
  select(1:5) %>% 
  filter(Time == 1800)

pairs(total_newnew)

total_new %>% 
  filter(Time == 1800) %>% 
  plot(Amylose_content, Hydro_extent)

total_new %>% 
  ggplot(aes(x = ,
             y = Hydro_extent,
             color = Xinf)) +
  geom_point(alpha = 0.1)

new <- total_new %>% 
  filter(Time == 1800) %>% 
  select(D1, D5, D9, mean_PastingTemp)

pairs(new)

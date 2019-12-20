
library(tidyverse)

# import the dataset

total <- read_csv("data/tidydata/joined_15P_update.csv") %>% 
  filter(Sample != "C+" & Sample != 'C-') # remove the controls

# add a new column h

total_h <- total %>% 
  mutate(h = 1 - H) 

# plot h and k

total_h %>% 
  ggplot(aes(x = h,
             y = k)) +
  geom_point() 

# h and k are strongly correlated 

# add a new column K (=log(k)) and explore the relationship between h and K

total_h_K <- total_h %>% 
  mutate(K = log(k))

# plot h and K

total_h_K %>% 
  ggplot(aes(x = h, 
             y = K)) +
  geom_point()

# check the correlation between h and K 

cor(total_h_K$h, total_h_K$K, use = "complete.obs") # strong correlation 0.97

# linear regression

lm_h_k <- lm(data = total_h_K, K ~ h)

summary(lm_h_k) # K = 5.31h - 5.93

# add the regression line to the plot

cor_h_K <- total_h_K %>% 
  ggplot(aes(x = h, 
             y = K)) +
  geom_point() +
  geom_abline(aes(slope = 5.31, intercept = -5.93), col = "red")

# save the plot

ggsave("figures/correlation_h_k.png", 
       plot = cor_h_K, 
       width = 15, 
       height = 15, 
       units = "cm") 


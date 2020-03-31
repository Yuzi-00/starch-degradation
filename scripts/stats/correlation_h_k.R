
library(tidyverse)

# import the dataset

df <- read_csv("analysis/total_new_convert.csv")

# add a new column K (=log(k))

df <- df %>% 
  mutate(K = log(k))

# add a column to distinguish test samples from the controls. Go back and conserve the WellGroupType
# column 

df <- df %>%
  mutate(WellGroupType = case_when(
    Sample == "C+" ~ "Control+",
    Sample == "C-" ~ "Control-",
    TRUE ~ "Test sample"
  ))

#### test samples ####

# select the test samples

test_sample <- df %>%
  filter(Sample != "C+" & Sample != "C-")

# plot h and k

test_sample %>% 
  ggplot(aes(x = h,
             y = k)) +
  geom_point() 

# h and k are strongly correlated 

# plot h and K

test_sample %>% 
  ggplot(aes(x = h, 
             y = K)) +
  geom_point()

# check the correlation between h and K 

cor(test_sample$h, test_sample$K, use = "complete.obs") # strong correlation 0.97

# linear regression

lm_h_k <- lm(data = test_sample, K ~ h)

summary(lm_h_k) # K = 5.31h - 5.93

# add the regression line to the plot

cor_h_K <- test_sample %>% 
  ggplot(aes(x = h, 
             y = K)) +
  geom_point() +
  geom_abline(aes(slope = 5.31, intercept = -5.93), col = "red")

cor_h_K # K = 5.31h - 5.93

# save the plot

ggsave("figures/correlation_h_k.png", 
       plot = cor_h_K, 
       width = 15, 
       height = 15, 
       units = "cm") 

#### positive controls ####

# select the positive controls

pos_control <- df %>%
  filter(Sample == "C+")

# plot h and k

pos_control %>% 
  ggplot(aes(x = h,
             y = k)) +
  geom_point() 

# plot h and K

pos_control %>% 
  ggplot(aes(x = h, 
             y = K)) +
  geom_point()

# check the correlation between h and K 

cor(pos_control$h, pos_control$K, use = "complete.obs") # strong correlation 0.97

# linear regression

lm_h_k <- lm(data = pos_control, K ~ h)

summary(lm_h_k) # K = 5.48h - 5.14

# add the regression line to the plot

cor_h_K <- pos_control %>% 
  ggplot(aes(x = h, 
             y = K)) +
  geom_point() +
  geom_abline(aes(slope = 5.48, intercept = -5.14), col = "red")

cor_h_K # K = 5.48h - 5.14

# save the plot

ggsave("figures/hk_PosControl.png", 
       plot = cor_h_K, 
       width = 15, 
       height = 15, 
       units = "cm") 

#### negative controls ####

# select the negative controls

neg_control <- df %>%
  filter(Sample == "C-")

# plot h and k

neg_control %>% 
  ggplot(aes(x = h,
             y = k)) +
  geom_point() 

# plot h and K

neg_control %>% 
  ggplot(aes(x = h, 
             y = K)) +
  geom_point()

# check the correlation between h and K 

cor(neg_control$h, neg_control$K, use = "complete.obs") # strong correlation 0.98

# linear regression

lm_h_k <- lm(data = neg_control, K ~ h)

summary(lm_h_k) # K = 5.7h - 5.64

# add the regression line to the plot

cor_h_K <- neg_control %>% 
  ggplot(aes(x = h, 
             y = K)) +
  geom_point() +
  geom_abline(aes(slope = 5.7, intercept = -5.64), col = "red")

cor_h_K # K = 5.48h - 5.14

# save the plot

ggsave("figures/hk_NegControl.png", 
       plot = cor_h_K, 
       width = 15, 
       height = 15, 
       units = "cm") 

#### all in the same plot ###

# scatter plot

plot01 <- df %>% 
  ggplot(aes(x = h, 
             y = K,
             color = WellGroupType)) +
  scale_color_brewer(palette = "Dark2") +
  geom_point(alpha = 0.5)

plot01

# save the plot

ggsave("figures/hk.png", 
       plot = plot01, 
       width = 17, 
       height = 12, 
       units = "cm") 

# add the ablines

plot02 <- plot01 +
  geom_abline(aes(slope = 5.31, intercept = -5.93), col = "#7570b3") + # for test samples
  geom_abline(aes(slope = 5.48, intercept = -5.14), col = "#d95f02") + # for positive controls
  geom_abline(aes(slope = 5.7, intercept = -5.64), col = "#1b9e77") # for negative controls

plot02

# save the plot

ggsave("figures/hk_abline.png", 
       plot = plot02, 
       width = 17, 
       height = 12, 
       units = "cm") 

# install.packages("magrittr")

library(ggpubr)

plot01 <- test_sample %>% 
  ggplot(aes(x = h, 
             y = K)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point(alpha = 0.5) +
  stat_cor(label.y = -6)+ #this means at 35th unit in the y axis, the r squared and p value will be shown
  stat_regline_equation(label.y = -7) 

colors <- c("Test sample" = "black", "Positive control" = "red", "Negative control" = "blue")

ggplot() +
  geom_point(data = test_sample,
             aes(x = k, 
                 y = h),
             alpha = 0.5) +
  geom_smooth(data = test_sample,
              aes(x = k, 
                  y = h),
              method = "lm", se=FALSE, color="black", 
              formula = y ~ log(x)) +
  stat_cor(data = test_sample, aes(x = k, 
                                   y = h),
           label.x = 0.003, label.y = 0.23) + #this means at 35th unit in the y axis, the r squared and p value will be shown
  stat_regline_equation(data = test_sample, aes(x = k, 
                                                y = h),
                        formula = y ~ log(x),
                        label.x = 0.003, label.y = 0.27) +
  # add positive control
  geom_point(data = pos_control,
             aes(x = k, 
                 y = h),
             alpha = 0.5,
             color = "red") +
  geom_smooth(data = pos_control,
              aes(x = k, 
                  y = h
              ),
              method = "lm", se=FALSE, color="red", 
              formula = y ~ log(x)) +
  stat_cor(data = pos_control, aes(x = k, 
                                   y = h),
           label.x = 0.005, label.y = -0.15,
           color="red") + #this means at 35th unit in the y axis, the r squared and p value will be shown
  stat_regline_equation(data = pos_control, aes(x = k, 
                                                y = h),
                        formula = y ~ log(x),
                        label.x = 0.005, label.y = -0.1, color="red") +
  # add negative control
  geom_point(data = neg_control,
             aes(x = k, 
                 y = h,
                 color = "NC"),
             alpha = 0.5
             ) +
  geom_smooth(data = neg_control,
              aes(x = k, 
                  y = h
              ),
              method = "lm", se=FALSE, color="blue", 
              formula = y ~ log(x)) +
  stat_cor(data = neg_control, aes(x = k, 
                                   y = h),
           label.x = 0.01,
           label.y = 0.1,
           color="blue") + #this means at 35th unit in the y axis, the r squared and p value will be shown
  stat_regline_equation(data = neg_control, aes(x = k, 
                                                y = h),
                        formula = y ~ log(x),
                        label.x = 0.01, label.y = 0.15, color="blue") +
  scale_colour_manual(name="Line Color",
                      values=c(PC="red", TS="black", NC = "blue"))

P

# save the plot

ggsave("figures/hk_abline.png", 
       plot = P, 
       width = 17, 
       height = 12, 
       units = "cm") 

  
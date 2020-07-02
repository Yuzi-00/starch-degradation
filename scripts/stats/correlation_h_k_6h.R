library(tidyverse)

# import the dataset

df <- read_csv("analysis/fitted_weibull_parameters_6h_well_with_T0.csv")

df2 <- read_csv("analysis/total_new_convert.csv") %>%
  select(Well, Sample)

df3 <- left_join(df, df2)

# calculate h

df3 <- df3 %>%
  mutate(h = 1 - H) %>%
  select(-H)

# add a new column K (=log(k))

df4 <- df3 %>% 
  mutate(K = log(k))

# add a column to distinguish test samples from the controls. Go back and conserve the WellGroupType
# column 

df5 <- df4 %>%
  mutate(WellGroupType = case_when(
    Sample == "C+" ~ "Control+",
    Sample == "C-" ~ "Control-",
    TRUE ~ "Test sample"
  ))

# select the test samples

test_sample <- df5 %>%
  filter(Sample != "C+" & Sample != "C-")

# select the pos samples

pos_control <- df5 %>%
  filter(Sample == "C+")

# select the neg samples

neg_control <- df5 %>%
  filter(Sample == "C-")

# install.packages("magrittr")

library(ggpubr)

plot01 <- test_sample %>% 
  ggplot(aes(x = h, 
             y = K)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point(alpha = 0.5) +
  stat_cor(label.y = -6)+ #this means at 35th unit in the y axis, the r squared and p value will be shown
  stat_regline_equation(label.y = -7) 

P <- ggplot() +
  geom_point(data = test_sample,
             aes(x = k, 
                 y = h,
                 color = "Test_sample"),
             alpha = 0.5) +
  geom_smooth(data = test_sample,
              aes(x = k, 
                  y = h),
              method = "lm", se=FALSE, color="black", 
              formula = y ~ log(x, base = exp(1))) +
  stat_cor(data = test_sample, aes(x = k, 
                                   y = h),
           label.x = 0.003, label.y = 0.23) + #this means at 35th unit in the y axis, the r squared and p value will be shown
  stat_regline_equation(data = test_sample, aes(x = k, 
                                                y = h),
                        formula = y ~ log(x, base = exp(1)),
                        label.x = 0.003, label.y = 0.27) +
  # add positive control
  geom_point(data = pos_control,
             aes(x = k, 
                 y = h,
                 color = "Pos_control"),
             alpha = 0.5,
  ) +
  geom_smooth(data = pos_control,
              aes(x = k, 
                  y = h
              ),
              method = "lm", se=FALSE, color="red", 
              formula = y ~ log(x, base = exp(1))) +
  stat_cor(data = pos_control, aes(x = k, 
                                   y = h),
           label.x = 0.005, label.y = -0.15,
           color="red") + #this means at 35th unit in the y axis, the r squared and p value will be shown
  stat_regline_equation(data = pos_control, aes(x = k, 
                                                y = h),
                        formula = y ~ log(x, base = exp(1)),
                        label.x = 0.005, label.y = -0.1, color="red") +
  # add negative control
  geom_point(data = neg_control,
             aes(x = k, 
                 y = h,
                 color = "Neg_control"),
             alpha = 0.5
  ) +
  geom_smooth(data = neg_control,
              aes(x = k, 
                  y = h
              ),
              method = "lm", se=FALSE, color="blue", 
              formula = y ~ log(x, base = exp(1))) +
  stat_cor(data = neg_control, aes(x = k, 
                                   y = h),
           label.x = 0.01,
           label.y = 0.1,
           color="blue") + #this means at 35th unit in the y axis, the r squared and p value will be shown
  stat_regline_equation(data = neg_control, aes(x = k, 
                                                y = h),
                        formula = y ~ log(x, base = exp(1)),
                        label.x = 0.01, label.y = 0.15, color="blue") +
  scale_colour_manual(name="Sample type",
                      values=c(Pos_control="red", Test_sample="black", Neg_control = "blue")) +
  geom_hline(yintercept=0.04, color = "black", linetype = "dashed", size = 0.7) +
  scale_x_continuous(limits = c(0, 0.02), expand = c(0, 0)) 


P

# save the plot

ggsave("figures/hk_abline.png", 
       plot = P, 
       width = 25, 
       height = 20, 
       units = "cm") 

# scatter plot

plot01 <- df5 %>% 
  ggplot(aes(x = k, 
             y = h,
             color = WellGroupType)) +
  scale_color_brewer(palette = "Dark2") +
  geom_point(alpha = 0.5)

plot01


# !!!!!! run script correlation_h_k before this script !!!!!!

# plot the h k model with the control samples

library(ggpubr)

P <- ggplot() +
  geom_point(data = test_sample,
             aes(x = k, 
                 y = h),
             alpha = 0.5) +
  geom_smooth(data = test_sample,
              aes(x = k, 
                  y = h),
              method = "lm", se=FALSE, color="black", 
              formula = y ~ log(x, base = exp(1))) +
  stat_cor(data = test_sample, aes(x = k, 
                                   y = h),
           label.x = 0.008, label.y = 0, size = 5) + #this means at 35th unit in the y axis, the r squared and p value will be shown
  stat_regline_equation(data = test_sample, aes(x = k, 
                                                y = h),
                        formula = y ~ log(x, base = exp(1)),
                        label.x = 0.008, label.y = 0.05, size = 5) +
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
  scale_x_continuous(limits = c(0, 0.02), expand = c(0, 0)) +
  theme(axis.text.x = element_text(color="black", size=15), 
        axis.text.y = element_text(color="black", size=15)) +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))
 

P

# save the plot

ggsave("figures/hk_model.png", 
       plot = P, 
       width = 25, 
       height = 20, 
       units = "cm") 

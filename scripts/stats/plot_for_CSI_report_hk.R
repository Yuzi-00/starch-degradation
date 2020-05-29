
# !!!!!! run script correlation_h_k before this script !!!!!!

# # change the name of the category (in order to have a proper name in the legend)
# 
# neg_control$Category <- gsub('neg_control', 'Negative control', neg_control$Category)
# 
# pos_control$Category <- gsub('pos_control', 'Positive control', pos_control$Category)

# plot the h k model with the control samples

library(ggpubr)

P <- ggplot() +
  geom_point(data = test_sample,
             aes(x = k, 
                 y = h,
                 color = "MAGIC"),
             alpha = 0.5) +
  geom_smooth(data = test_sample,
              aes(x = k, 
                  y = h),
              method = "lm", se=FALSE, color="black", 
              formula = y ~ log(x, base = exp(1))) +
  stat_cor(data = test_sample, aes(x = k, 
                                   y = h),
           label.x = 0.003, label.y = 0.23, size = 5) + #this means at 35th unit in the y axis, the r squared and p value will be shown
  stat_regline_equation(data = test_sample, aes(x = k, 
                                                y = h),
                        formula = y ~ log(x, base = exp(1)),
                        label.x = 0.003, label.y = 0.27, size = 5) +
  # add positive control
  geom_point(data = pos_control,
             aes(x = k, 
                 y = h,
                 color = "Wx"),
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
           color="red", size = 5) + #this means at 35th unit in the y axis, the r squared and p value will be shown
  stat_regline_equation(data = pos_control, aes(x = k, 
                                                y = h),
                        formula = y ~ log(x, base = exp(1)),
                        label.x = 0.005, label.y = -0.1, color="red", size = 5) +
  # add negative control
  geom_point(data = neg_control,
             aes(x = k, 
                 y = h,
                 color = "HAM"),
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
           color="blue",
           size = 5) + #this means at 35th unit in the y axis, the r squared and p value will be shown
  stat_regline_equation(data = neg_control, aes(x = k, 
                                                y = h),
                        formula = y ~ log(x, base = exp(1)),
                        label.x = 0.01, label.y = 0.15, color="blue", size = 5) +
  scale_colour_manual(name="Sample type",
                      values=c("Wx"="red", "MAGIC"="black", "HAM" = "blue")) +
  scale_x_continuous(limits = c(0, 0.02)) +
  theme(axis.text.x = element_text(color="black", size=15), 
        axis.text.y = element_text(color="black", size=15)) +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20)) +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.spacing.x = unit(0.5, 'cm'))
 
P

# save the plot

ggsave("figures/hk_model.png", 
       plot = P, 
       width = 25, 
       height = 20, 
       units = "cm") 

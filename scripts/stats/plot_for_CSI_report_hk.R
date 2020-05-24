
# !!!!!! run script correlation_h_k before this script !!!!!!

# plot the h k model without the control samples

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
           label.x = 0.003, label.y = 0.23, size = 5) + #this means at 35th unit in the y axis, the r squared and p value will be shown
  stat_regline_equation(data = test_sample, aes(x = k, 
                                                y = h),
                        formula = y ~ log(x, base = exp(1)),
                        label.x = 0.003, label.y = 0.27, size = 5) +
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

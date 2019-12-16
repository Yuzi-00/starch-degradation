
library(tidyverse)

# import the dataset

residual_data <- read_csv("analysis/weibull_residuals.csv")

# plotting

residual_plot <- residual_data %>% 
  ggplot(aes(x = .fitted,
             y = .resid)) +
  geom_point(shape = 1,
             alpha = 0.5,
             size = 1) +
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5)，
        panel.background = element_rect(fill = "white"),
        axis.ticks=element_line(
          colour="black",
          size=.5)) +
  labs(x = "Fitted values", y = "Residuals") +
  geom_hline(yintercept=0, color = "blue", linetype = "dashed", size = 0.7) +
  scale_y_continuous(limits = c(-15,15), expand = c(0, 0))
  # geom_smooth(colour="red")

# save the plot

ggsave("figures/Weibull_residuals_replicates.png", 
       plot = residual_plot, 
       width = 10, 
       height = 12, 
       units = "cm") 

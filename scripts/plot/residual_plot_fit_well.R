
library(tidyverse)

# plot the residual with or without the time 0 

removeT0 <- FALSE

# import the dataset

if(removeT0) residual_data <- read_csv("analysis/weibull_residuals_well_without_T0.csv") else
  residual_data <- read_csv("analysis/weibull_residuals_well_with_T0.csv")

# plotting

residual_plot <- residual_data %>% 
  ggplot(aes(x = .fitted,
             y = .resid)) +
  geom_point(shape = 1,
             alpha = 0.5,
             size = 1) +
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.ticks=element_line(
          colour="black",
          size=.5)) +
  labs(x = "Fitted values", y = "Residuals") +
  geom_hline(yintercept=0, color = "blue", linetype = "dashed", size = 0.7) +
  scale_y_continuous(limits = c(-10,10), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-2,100), expand = c(0, 0)) +
  # geom_smooth(colour="red")
  theme(plot.margin = unit(c(5.5,12,5.5,5.5), "pt"))

residual_plot

# save the plot

if(removeT0) ggsave("figures/Weibull_residuals_well_without_T0.png", 
                    plot = residual_plot, 
                    width = 12, 
                    height = 12, 
                    units = "cm") else
                      ggsave("figures/Weibull_residuals_well_with_T0.png", 
                             plot = residual_plot, 
                             width = 12, 
                             height = 12, 
                             units = "cm")

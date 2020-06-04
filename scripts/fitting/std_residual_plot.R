
library(tidyverse)

# import the fitted dataset 

df <- read_csv("analysis/weibull_residuals_well_with_T0.csv") %>%
  select(-.resid) # remove the residual generated automatically by R, we're going to calculate
                  # the residuals by ourself

# import a dataset that contains sample and well columns 

df0 <- read_csv("analysis/total_new_convert.csv") %>%
  select(Sample, Well) # select just the sample and well columns 

# add the sample name into the residual dataset

df1 <- left_join(df, df0)

# calculate the standarised residual for each sample at each time point 

res <- df1 %>%
  filter(Time != 0) %>% # remove time = 0 cuz this will disturb the calculation of std_resid
  mutate(resid = HE - .fitted) %>%
  group_by(Sample, Time) %>%
  mutate(mean_resid = mean(resid), sd_fit = sd(.fitted),
         std_resid = resid / sd_fit) 

# plot

rp <- ggplot(data = res,
       aes(x = .fitted,
           y = std_resid)) +
  geom_point(shape = 1,
             alpha = 0.8,
             size = 1) +
  scale_y_continuous(limits = c(-5,5)) +
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.ticks=element_line(
          colour="black",
          size=.5)) +
  labs(x = "Fitted values", y = "Standardized residuals") +
  geom_hline(yintercept=0, color = "blue", linetype = "dashed", size = 0.7) +
  scale_x_continuous(limits = c(-2,100), expand = c(0, 0)) +
  # geom_smooth(colour="red")
  theme(plot.margin = unit(c(5.5,12,5.5,5.5), "pt"))

rp

# save the plot

ggsave("figures/std_residual_CSI.png", 
       plot = rp, 
       width = 12, 
       height = 12, 
       units = "cm") 

# check the distribution of the residuals 

qqnorm(res$resid, pch = 1, frame = FALSE)
qqline(res$resid, col = "steelblue", lwd = 2)
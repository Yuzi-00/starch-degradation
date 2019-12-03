

#                                         ** the curve fitting for 9_F_5 (sample 186) failed due to unknown reason **
 
#                                                   ** Let's check this Well **


library(tidyverse)

# import the dataset (outliers has been replaced by NAs)

data_15P_cal_HE_outlier_replaced <- read_csv("data/tidydata/data_15P_cal_HE_outlier_replaced.csv")

# create a subset for 9_F_5

problem_well <- data_15P_cal_HE_outlier_replaced %>% 
  filter(Well == "9_F_5")

# plotting

ggplot(data = problem_well, 
       aes(x = Time, 
           y = HE)) + 
  geom_point(size = 1, shape = 1) + # add the transparency
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) + ## set the range of the y axis
  scale_x_continuous(limits = c(0, 2000), expand = c(0, 0)) +
  ylab("Hydrolysis extent (%)") + ## change the label for the y axis
  xlab("Time (min)") + ## change the name of the x axis
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5)，
        panel.background = element_rect(fill = "white"),
        axis.ticks=element_line(
          colour="black",
          size=.5)) +
  labs(x = "Time (min)", y = "Hydrolysis extent (%)") +
  theme(axis.text.x = element_text(color="black", size=10), 
        axis.text.y = element_text(color="black", size=10)) +
  scale_color_discrete(labels = c("Negative control", "Sample", "Positive control")) +
  theme(legend.key = element_blank(),
        legend.position = "bottom")
# the plot looks nice without issues 



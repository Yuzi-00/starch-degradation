
library(tidyverse)


#                       ** plots are generated using the dataset with outliers replaced **


#                                      ** scatter plot of the HE **


# import the dataset

data_15P_cal_HE_outlier_replaced <- read_csv("data/tidydata/data_15P_cal_HE_outlier_replaced.csv")

# remove the unused wells

data_15P_cal_HE_outlier_replaced <- data_15P_cal_HE_outlier_replaced %>% 
  filter(Sample != "X")

# plotting

HE_scatter_15P <- ggplot(data = data_15P_cal_HE_outlier_replaced, 
       aes(x = Time, 
           y = HE,
           color = WellGroupType)) + # colored by the status (distinguish the control from the other samples)
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
        legend.position = "bottom") +
  theme(plot.margin = unit(c(5.5,12,5.5,5.5), "pt"))

# save the plot

ggsave("figures/scatter-plot_15P.png", 
       plot = HE_scatter_15P, 
       width = 10, 
       height = 8, 
       units = "cm") 


#                                    ** line plot of the HE ** 


HE_line_15P <- ggplot(data = data_15P_cal_HE_outlier_replaced, 
                 aes(x = Time, 
                     y = HE,
                     group = Well,
                     color = WellGroupType)) + # colored by the status (distinguish the control from the other samples)
  geom_point(size = 1, shape = 1) + # add the transparency
  geom_line(size = 0.005) +
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
  scale_color_discrete(labels = c("Negtive control", "Sample", "Positive control")) +
  theme(legend.key = element_blank(),
        legend.position = "bottom")+
  theme(plot.margin = unit(c(5.5,12,5.5,5.5), "pt"))

# save the plot

ggsave("figures/line-plot_15P.png", 
       plot = HE_line_15P, 
       width = 10, 
       height = 8, 
       units = "cm") 


#                                   ** individual plot (pdf) ** 


pdf(file = "figures/degradability_individual plot.pdf") # creating a pdf file and senting all the plot below to this file
for(i in unique(data_15P_cal_HE_outlier_replaced$Sample)){ # i stands for each item within this dataset
  # unique() can show all the Sample names here whithin the mean_HE_6P dataset 
  digestibility <- data_15P_cal_HE_outlier_replaced %>% 
    filter(Sample == i) %>% # pipe this to the first argument on the right side 
    # here, the first argument of ggplot is the data, otherwise, we have to type ggplot(data = .)
    # to let it pipe to this argument
    ggplot(aes(x = Time, 
               y = HE,
               group = Well)) + 
    geom_line() +
    geom_point() +
    ggtitle(i) + # set the title for each plot as i 
    scale_y_continuous(limits = c(0,100), expand = c(0, 0)) + ## set the range of the y axis
    scale_x_continuous(limits = c(0, 2000), expand = c(0, 0)) +
    theme( # remove the legend
          panel.grid = element_blank(), # remove the grid 
          axis.line = element_line(colour = "black", size = 0.5), # add the x axis
          panel.background = element_rect(fill = "white", color = "black"),
          #change the backgroud color to white and the frame color to black
          axis.ticks = element_line(colour="black", size=.5)) +
    # change the color of the ticks into black and the size to 0.5
    labs(x = "Time (min)", y = "Hydrolysis extent (%)") +
    # change the title of the x and y axis
    theme(axis.text.x = element_text(color="black", size=10), 
          axis.text.y = element_text(color="black", size=10)) +
  # change the color and size of the tick label for x and y axis
    theme(plot.margin = unit(c(5.5,12,5.5,5.5), "pt"))
  print(digestibility) # print out the plots
} 
dev.off() # stop sending plot to the pdf file


#                           ** individual plot with the sd error **


# import the variation dataset (noted that the outliers had already been removed from this dataset)

data_15P_cal_var <- read_csv("analysis/data_15P_cal_var.csv")

# remove the unused wells

data_15P_cal_var <- data_15P_cal_var %>% 
  filter(Sample != "X")

# creat the loop

pdf(file = "figures/degradability_individual plot_sd_error.pdf") # creating a pdf file and senting all the plot below to this file
for(i in unique(data_15P_cal_var$Sample)){ # i stands for each item within this dataset
  # unique() can show all the Sample names here within the mean_HE_6P dataset 
  digestibility_var <- data_15P_cal_var %>% 
    filter(Sample == i) %>% # pipe this to the first argument on the right side 
    # here, the first argument of ggplot is the data, otherwise, we have to type ggplot(data = .)
    # to let it pipe to this argument
    ggplot() + 
    geom_line(aes(x = Time,
                  y = Mean_HE,
                  group = Sample),
              color = "red",
              size = 0.05) +
    geom_errorbar(aes(x = Time, 
                      ymin = Mean_HE - Se_HE, 
                      ymax = Mean_HE + Se_HE), 
                  width=20,
                  color = "red") +
    geom_point(aes(x = Time, 
                   y = HE),
               shape = 1,
               alpha = 0.2,
               size = 1) +
    geom_line(aes(x = Time,
                  y = HE,
                  group = Well),
              size = 0.005,
              alpha = 0.2) +
    ggtitle(i) + # set the title for each plot as i 
    scale_y_continuous(limits = c(0,100), expand = c(0, 0)) + ## set the range of the y axis
    scale_x_continuous(limits = c(0, 2000), expand = c(0, 0)) +
    theme( # remove the legend
      panel.grid = element_blank(), # remove the grid 
      axis.line = element_line(colour = "black", size = 0.5), # add the x axis
      panel.background = element_rect(fill = "white", color = "black"),
      #change the backgroud color to white and the frame color to black
      axis.ticks = element_line(colour="black", size=.5)) +
    # change the color of the ticks into black and the size to 0.5
    labs(x = "Time (min)", y = "Hydrolysis extent (%)") +
    # change the title of the x and y axis
    theme(axis.text.x = element_text(color="black", size=10), 
          axis.text.y = element_text(color="black", size=10)) +
    # change the color and size of the tick label for x and y axis
    theme(plot.margin = unit(c(5.5,12,5.5,5.5), "pt"))
  print(digestibility_var) # print out the plots
} 
dev.off() # stop sending plot to the pdf file


#                                           ** check if the lower lines come from the same plate **


# create a vector that contains the samples that we want to check

check_items <- c(32, 196, 108, 209, 40, 204, 87, 112, 72, 104, 122, 27, 197, 188, 8, 2, 52, 183, 98, 163, 203, 121, 92)

# select the items above from the total dataset

lower_line <- data_15P_cal_HE_outlier_replaced %>% 
  filter(Sample %in% check_items) %>% # give us the samples that are contained in check_items
  filter(Time == 1800) %>% # choose just 1800min
  group_by(Sample) %>% # group by sample
  filter(HE == min(HE)) # find the minimum value of HE for each sample 
# take a look at the corresponding plate number, they don't come from the same plates  












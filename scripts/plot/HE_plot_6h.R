

library(tidyverse)

# import the dataset

data_15P_cal_HE_outlier_replaced <- read_csv("data/tidydata/data_15P_cal_HE_outlier_replaced.csv") %>% 
  mutate(Plate = as.factor(Plate))

# remove the unused wells and remove time 1440 and 1800min

data_15P_cal_HE_outlier_replaced <- data_15P_cal_HE_outlier_replaced %>% 
  filter(Sample != "X") %>%
  filter(Time != 1440 & Time != 1800)

# change the name of the Wellgrouptype (for a proper legend later)

data_15P_cal_HE_outlier_replaced$WellGroupType <- gsub("Control[+]", "Wx", data_15P_cal_HE_outlier_replaced$WellGroupType)

data_15P_cal_HE_outlier_replaced$WellGroupType <- gsub("Control[-]", "HAM", data_15P_cal_HE_outlier_replaced$WellGroupType)

data_15P_cal_HE_outlier_replaced$WellGroupType <- gsub("Test sample", "MAGIC", data_15P_cal_HE_outlier_replaced$WellGroupType)

#                                   ** individual plot (pdf) ** 


pdf(file = "figures/degradability_individual plot_6h.pdf") # creating a pdf file and senting all the plot below to this file
for(i in unique(data_15P_cal_HE_outlier_replaced$Sample)){ # i stands for each item within this dataset
  # unique() can show all the Sample names here whithin the mean_HE_6P dataset 
  digestibility <- data_15P_cal_HE_outlier_replaced %>% 
    filter(Sample == i) %>% # pipe this to the first argument on the right side 
    # here, the first argument of ggplot is the data, otherwise, we have to type ggplot(data = .)
    # to let it pipe to this argument
    ggplot(aes(x = Time, 
               y = HE,
               group = Well,
               color = Plate)) + 
    geom_line() +
    geom_point() +
    ggtitle(i) + # set the title for each plot as i 
    scale_y_continuous(limits = c(0,100), expand = c(0, 0)) + ## set the range of the y axis
    scale_x_continuous(limits = c(0, 400), expand = c(0, 0)) +
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

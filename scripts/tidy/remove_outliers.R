
# in order the identify the outliers
# we have to firstly draw and take a look at the individual plot for each sample


#                         ** use individual plot (pdf) to check the outliers ** 


# read in the calculated dataset

data_15P_cal_HE <- read_csv("data/tidydata/data_15P_cal_HE.csv")

# creat a loop to generate plots for each sample

pdf(file = "figures/check_outlier_individual plot.pdf") # creating a pdf file and senting all the plot below to this file
for(i in unique(data_15P_cal_HE$Sample)){ # i stands for each item within this dataset
  # unique() can show all the Sample names here whithin the mean_HE_6P dataset 
  digestibility <- data_15P_cal_HE %>% 
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

# now we can identify the outliers


#                              ** change the outliers into NAs **


#       ** noted that the outliers were replaced by NAs after going back and checking the raw data, make 
#                   sure that there's no mistake in the raw spreadsheet **

data_15P_cal_HE_outlier_replaced <- data_15P_cal_HE %>% 
  mutate(HE = if_else(Sample == 208 & HE > 92, NA_real_,HE), # remove one point
         HE = if_else(Time == 20 & Sample == 13 & HE > 29, NA_real_,HE), # remove one point
         HE = if_else(Time == 20 & Sample == 118 & HE > 16, NA_real_,HE), # remove one point
         HE = if_else(Time == 20 & Sample == 127 & HE > 11, NA_real_,HE), # remove one point
         HE = if_else(Time == 120 & Sample == 127 & HE > 29, NA_real_,HE), # remove one point
         HE = if_else(Time == 0 & Sample == 96 & HE > 9, NA_real_,HE), # remove one point
         HE = if_else(Time == 180 & Sample == 103 & HE >40, NA_real_,HE), # remove one point
         HE = if_else(Time == 360 & Sample == 134 & HE < 35, NA_real_,HE), # remove one point
         HE = if_else(Time == 180 & Sample == 165 & HE > 30, NA_real_,HE), # remove one point
         HE = if_else(Time == 360 & Sample == 26 & HE < 37, NA_real_,HE),  # remove one point
         HE = if_else(Time == 180 & Sample == 134 & HE < 23, NA_real_,HE), # remove one point
         HE = if_else(Time == 180 & Sample == 136 & HE < 19, NA_real_,HE), # remove one point
         HE = if_else(Plate == 12 & Sample == "113", NA_real_,HE)) # remove 9 time points
# before, there were 6480 observations.
# now, just 6459 observations. (21 outliers had been deleted)

# save the dataset

write_csv(data_15P_cal_HE_outlier_replaced, "data/tidydata/data_15P_cal_HE_outlier_replaced.csv")

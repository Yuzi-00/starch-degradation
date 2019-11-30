
# read in the dataset

data_15P <- read_csv("data/tidydata/data_15P_outlier_deleted.csv")


# create two subsets for the controls 

# positive control

pos <- data_15P %>% 
  filter(Sample == "C+")

# negative control

neg <- data_15P %>% 
  filter(Sample == 'C-')

# find the lowest value for C+

pos_control <- data_15P %>% 
  filter(Time == 1800) %>% 
  filter(Sample == "C+") %>% 
  arrange(HE) # the minimun value of the C+ at 1800 min is 78.96

# find the highest value for C-

neg_control <- data_15P %>% 
  filter(Time == 1800) %>% 
  filter(Sample == "C-") %>% 
  arrange(desc(HE)) # the highest value of C- at 1800min is 64.10

# find the samples with a HE > 78

high_HE <- data_15P %>% 
  filter(Time == 1800) %>% 
  filter(HE > 78)

# go back to the initial dataset and find out all the samples (with all time points) that are in the high_HE

high_HE <- data_15P[data_15P$Sample %in% high_HE$Sample,] 
# if the sample name in data_15P is also in the potentiel_outliers, print out the results and save it to a new data frame



# plotting the high_HE

data_status <- high_HE %>% 
  mutate(Status = case_when( # number of conditions (left), discription of the results in that condition
    Sample == "C+" ~ "Positive control", 
    Sample == "C-" ~ "Negative control",
    TRUE ~ "Potentiel outliers"
  ))

# add a column that distinguish each sample

final_data <- data_status %>% 
  mutate(Well = paste(Plate, Row, Column, sep = "_")) 

# plot and save the high HE samples

pdf(file = "figures/high_HE.pdf") # creating a pdf file and senting all the plot below to this file
for(i in unique(high_HE$Sample)){ # i stands for each item within this dataset
  # unique() can show all the Sample names here whithin the mean_HE_6P dataset 
  digestibility <- high_HE %>% 
    filter(Sample == i) %>% # pipe this to the first argument on the right side 
    # here, the first argument of ggplot is the data, otherwise, we have to type ggplot(data = .)
    # to let it pipe to this argument
    ggplot(aes(x = Time, 
               y = HE,
               group = Plate,
               color = as.factor(Plate))) + 
    geom_line() +
    geom_point() +
    geom_line(data = pos,
              aes(x = Time,
              y = HE,
              group = Plate),
              color = "black",
              alpha = 0.5) +
  geom_point(data = pos,
             aes(x = Time,
                 y = HE),
             color = "black",
             alpha = 0.5) +
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

# find the samples with a HE < 65

low_HE <- data_15P %>% 
  filter(Time == 1800) %>% 
  filter(HE < 65)

# go back to the initial dataset and find out all the samples (with all time points) that are in the low_HE

low_HE <- data_15P[data_15P$Sample %in% low_HE$Sample,] 
# if the sample name in data_15P is also in the potentiel_outliers, print out the results and save it to a new data frame

# plot and save the low HE samples

pdf(file = "figures/low_HE.pdf") # creating a pdf file and senting all the plot below to this file
for(i in unique(low_HE$Sample)){ # i stands for each item within this dataset
  # unique() can show all the Sample names here whithin the mean_HE_6P dataset 
  digestibility <- low_HE %>% 
    filter(Sample == i) %>% # pipe this to the first argument on the right side 
    # here, the first argument of ggplot is the data, otherwise, we have to type ggplot(data = .)
    # to let it pipe to this argument
    ggplot(aes(x = Time, 
               y = HE,
               group = Plate,
               color = as.factor(Plate))) + 
    geom_line() +
    geom_point() +
    geom_line(data = neg,
              aes(x = Time,
                  y = HE,
                  group = Plate),
              color = "black",
              alpha = 0.5) +
    geom_point(data = neg,
               aes(x = Time,
                   y = HE),
               color = "black",
               alpha = 0.5) +
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

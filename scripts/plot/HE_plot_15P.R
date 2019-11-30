library(tidyverse)

library(naniar)


#                                ** scatter plot of the HE **


# read in the calculated dataset

data_15P <- read_csv("data/tidydata/data_15P_cal.csv")

# add a new column that descripe the status of each samples 

data_status <- data_15P %>% 
  mutate(status = case_when( # number of conditions (left), discription of the results in that condition
    Sample == "C+" ~ "Positive control", 
    Sample == "C-" ~ "Negative control",
    TRUE ~ "Sample"
  ))

# plot with the 3 replicates separately

ggplot(data = data_status, 
       aes(x = Time, 
           y = HE,
           group = Sample,
           color = status)) + # colored by the status (distinguish the control from the other samples)
  geom_point(size = 2, shape = 1) + # add the transparency
  scale_y_continuous(limits = c(0,100)) + ## set the range of the y axis
  scale_x_continuous(limits = c(0, 2000)) +
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
        legend.position = "bottom")

# noted that there's some outliers (very sure that it's contaminated) at 20 min, need to change these values into NA

# select just the time at 20minutes and find the outliers

data_t20min <- data_status %>% 
  filter(Time == 20) %>% 
  select(Plate, Sample, HE) %>% 
  arrange(desc(HE)) # the first four points are the outliers : 208, 13, 118, 127

# select just the time at 0minutes and find the outliers

data_t0min <- data_status %>% 
  filter(Time == 0) %>% 
  select(Plate, Sample, HE) %>% 
  arrange(desc(HE)) # the first point is the outlier : 96

# change the value into NA, HOW ????????????????????????????????????????????????

## data_15P_remove_outlier <- data_15P %>% 
## mutate(HE = na_if(HE, "92") # if the value in column OD is 92.81697, replace them by NA 

# remove the entire row of those four outliers for now

data_15P_outlier_deleted <- data_15P %>% 
  filter(!(Sample == 208 & HE > 92)) %>% # remove each outlier saparatly
  filter(!(Time == 20 & Sample == 13 & HE > 29)) %>% 
  filter(!(Time == 20 & Sample == 118 & HE > 16)) %>% 
  filter(!(Time == 20 & Sample == 127 & HE > 11)) %>% 
  filter(!(Time == 0 & Sample == 96 & HE > 9)) %>% 
  filter(!(Time == 180 & Sample == 103 & HE >40)) %>% # remove sample 103 in plate 11 at 180min
  filter(!(Time == 360 & Sample == 134 & HE < 35)) %>% # remove sample 134 in plate 11 at 360min
  filter(!(Time == 120 & Sample == 127 & HE > 29)) %>% # remove sample 127 in plate 13 at 120min
  filter(!(Time == 180 & Sample == 165 & HE > 30)) %>%  # remove sample 165 in plate 8 at 180min
  filter(!(Time == 360 & Sample == 26 & HE < 37)) %>%  # remove sample 26 in plate 10 at 360min
  filter(!(Time == 180 & Sample == 134 & HE < 23)) %>% # remove sample 134 in plate 11 at 180min
  filter(!(Time == 180 & Sample == 136 & HE < 19)) # remove sample 136 in plate 13 at 180min

# also, the sample 113 in plate 12 at time 1440 and 1800 have a HE > 100, remove this one as well

data_15P_outlier_deleted <- data_15P_outlier_deleted %>% 
  filter(!(Plate == 12 & Sample == "113"))

# save the dataset without the outlier (208 at t20min)

write_csv(data_15P_outlier_deleted, "data/tidydata/data_15P_outlier_deleted.csv")


#                    ** scatter plot of the HE without outliers **


# import the dataset

data_15P_outlier_deleted <- read_csv("data/tidydata/data_15P_outlier_deleted.csv")

data_status <- data_15P_outlier_deleted %>% 
  mutate(status = case_when( # number of conditions (left), discription of the results in that condition
    Sample == "C+" ~ "Positive control", 
    Sample == "C-" ~ "Negative control",
    TRUE ~ "Sample"
  ))

HE_scatter_15P <- ggplot(data = data_status, 
       aes(x = Time, 
           y = HE,
           group = Sample,
           color = status)) + # colored by the status (distinguish the control from the other samples)
  geom_point(size = 1, shape = 1) + # add the transparency
  scale_y_continuous(limits = c(0,100)) + ## set the range of the y axis
  scale_x_continuous(limits = c(0, 2000)) +
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
        legend.position = "bottom")

# save the plot

ggsave("figures/scatter-plot_15P.png", 
       plot = HE_scatter_15P, 
       width = 10, 
       height = 8, 
       units = "cm") 
# need to change the size of the figure later !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#                                    ** line plot of the HE ** 


# read in the calculated dataset

data_15P <- read_csv("data/tidydata/data_15P_outlier_deleted.csv")

# add a new column that descripe the status of each samples 

data_status <- data_15P %>% 
  mutate(status = case_when( # number of conditions (left), discription of the results in that condition
    Sample == "C+" ~ "Positive control", 
    Sample == "C-" ~ "Negative control",
    TRUE ~ "Sample"
  ))

# add a column that distinguish each sample

final_data <- data_status %>% 
  mutate(Well = paste(Plate, Row, Column, sep = "_")) 

# plot

HE_line_15P <- ggplot(data = final_data, 
                 aes(x = Time, 
                     y = HE,
                     group = Well,
                     color = status)) + # colored by the status (distinguish the control from the other samples)
  geom_point(size = 1, shape = 1) + # add the transparency
  geom_line(size = 0.005) +
  scale_y_continuous(limits = c(0,100)) + ## set the range of the y axis
  scale_x_continuous(limits = c(0, 2000)) +
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
        legend.position = "bottom")

# several checks

# find the sample that disappear from 1440min

arrange <- final_data %>% 
  filter(Time == 360) %>% 
  filter(Sample != "C+") %>% 
  arrange(desc(HE)) # highest HE : sample 113

# check Sample 113

S113 <- final_data %>% 
  filter(Sample == 113) %>% 
  select(Plate, Sample, Time, HE)
# the reason of disappearing is that the HE of 113 in plate 13 at 1440 and 1800min are above 100%, need to check it !!!!!!!!!!!!!!!!!!!!!!!

# save the plot

ggsave("figures/line-plot_15P.png", 
       plot = HE_line_15P, 
       width = 10, 
       height = 8, 
       units = "cm") 
# need to change the size of the figure later !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#                                   ** individual plot (pdf) ** 


# import the dataset and add a new colume

data_15P_outlier_deleted <- read_csv("data/tidydata/data_15P_outlier_deleted.csv")

# add a new column to distinguish each replicates 

data_15P_outlier_deleted <-  data_15P_outlier_deleted %>% 
  mutate(Well = paste(Plate, Row, Column, sep = "_"))

#############

pdf(file = "figures/degradability_individual plot.pdf") # creating a pdf file and senting all the plot below to this file
for(i in unique(data_15P_outlier_deleted$Sample)){ # i stands for each item within this dataset
  # unique() can show all the Sample names here whithin the mean_HE_6P dataset 
  digestibility <- data_15P_outlier_deleted %>% 
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


#                           ** individual plot with the error bar **


# import the variation dataset (noted that the outliers had already been removed from this dataset)

total_data <- read_csv("analysis/data_15P_var.csv")

# add a new column to distinguish each replicates 

total_data <-  total_data %>% 
  mutate(Well = paste(Plate, Row, Column, sep = "_"))

# creat the loop

pdf(file = "figures/degradability_individual plot with error bar.pdf") # creating a pdf file and senting all the plot below to this file
for(i in unique(total_data$Sample)){ # i stands for each item within this dataset
  # unique() can show all the Sample names here whithin the mean_HE_6P dataset 
  digestibility_var <- total_data %>% 
    filter(Sample == i) %>% # pipe this to the first argument on the right side 
    # here, the first argument of ggplot is the data, otherwise, we have to type ggplot(data = .)
    # to let it pipe to this argument
    ggplot() + 
    geom_line(aes(x = Time,
                  y = mean_HE,
                  group = Well),
              color = "red",
              size = 0.05) +
    geom_errorbar(aes(x = Time, ymin=mean_HE - sd_HE, ymax=mean_HE + sd_HE), 
                  width=20,
                  color = "red") +
    geom_point(aes(x = Time, 
                   y = HE),
               shape = 1,
               alpha = 0.2,
               size = 1) +
    geom_line(aes(x = Time,
                  y = HE,
                  group = Plate),
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









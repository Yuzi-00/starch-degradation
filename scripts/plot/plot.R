library(tidyverse)

#######################################################################################################

# ploting using the data_6P_cal dataset 

cal_6P <- read_csv("data/tidydata/data_6P_cal.csv") # read in the dataset with all the calculation

mean_HE_6P <- cal_6P %>% 
  group_by(Sample, Time) %>% 
  summarise(mean_HE = mean(HE)) # calculating the mean values for plotting

# below is a quick check for the NA value in this dataset
x <- cal_6P %>% 
  group_by(Sample, Time) %>% 
  summarise(mean_HE = mean(HE, na.rm = TRUE),
            na_count = sum(is.na(HE)), # count the NA and sum them up
            # it'll give you 1 for each NA and 0 for non NA
            count = n()) # count the number of observation for each group (by sample and time)
# finishing the checking step


ggplot(data = mean_HE_6P, # plotting by Time and the mean values of HE
       aes(x = Time, 
           y = mean_HE,
           group = Sample,
           color = Sample %in% c("C+", "C-"))) + # too many legends, how to change it ???
  geom_line() +
  geom_point() +
  theme(legend.position = "none")
  # ??? scale_color_continuous("Price") # need to find the right expression
  # ??? scale_color_manual(values = ) # how to change the color just for one line ????
  # facet_wrap(~Sample) # problem: too many samples

pdf(file = "figures/digestibility.pdf") # creating a pdf file and senting all the plot below to this file
for(i in unique(mean_HE_6P$Sample)){ # i stands for each item within this dataset
  # unique() can show all the Sample names here whithin the mean_HE_6P dataset 
digestibility <- mean_HE_6P %>% 
    filter(Sample == i) %>% # pipe this to the first argument on the right side 
  # here, the first argument of ggplot is the data, otherwise, we have to type ggplot(data = .)
  # to let it pipe to this argument
    ggplot(aes(x = Time, 
           y = mean_HE)) + 
  geom_line() +
  geom_point() +
  theme(legend.position = "none", # remove the legend
        panel.grid = element_blank(), # remove the grid 
        axis.line = element_line(colour = "black", size = 0.5), # add the x axis
        panel.background = element_rect(fill = "white", color = "black"),
        #change the backgroud color to white and the frame color to black
        axis.ticks = element_line(colour="black", size=.5)) +
# change the color of the ticks into black and the size to 0.5
  labs(x = "Time (min)", y = "Hydrolysis extent (%)") +
       # change the title of the x and y axis
  theme(axis.text.x = element_text(color="black", size=10), 
        axis.text.y = element_text(color="black", size=10))
# change the color and size of the tick label for x and y axis
print(digestibility) # print out the plots
} 
dev.off() # stop sending plot to the pdf file

control_6P <- cal_6P %>% 
  filter(WellGroupType == "Control-" | WellGroupType == "Control+") %>% 
  group_by(Plate)
# choose the control group and save them to a new dataset

control_6P <- ggplot(data = control_6P, # plotting for the control samples separately
       aes(x = Time, 
           y = HE,
           group = Plate,
           color = Sample)) + 
  geom_line() +
  geom_point() + # problem: if we don't split it into two graphs, the lines are not correctly conneted
  facet_wrap(~Sample)

ggsave("figures/control_6P.png", 
       plot = control_6P, 
       width = 15, 
       height = 8, 
       units = "cm") # save the plot

var_control_6P <- cal_6P %>% 
  filter(Sample == "C+" | Sample == "C-") %>% 
  group_by(Sample, Time) %>% 
  summarise(mean_HE = mean(HE), sd_HE = sd(HE), cov = sd_HE / mean_HE * 100)
# calculating the variation for the control samples

write_csv(var_control_6P, "analysis/var_control_6P.csv")

var_control_6P <- ggplot(data = var_control_6P, # plotting for the control samples using mean and sd
                     aes(x = Time, 
                         y = mean_HE,
                         color = Sample)) + 
  geom_errorbar(aes(ymin=mean_HE - sd_HE, ymax=mean_HE + sd_HE), width = 50) + # adding the sd
  # ??? question here
  geom_line() +
  geom_point() 

ggsave("figures/var_control_6P.png", 
       plot = var_control_6P, 
       width = 15, 
       height = 8, 
       units = "cm") # save the plot


#######################################################################################################


# ploting using the data_6P_cal_2nd dataset 

cal_6P_2nd <- read_csv("data/tidydata/data_6P_cal_2nd.csv") # read in the dataset with all the calculation

mean_HE_6P_2nd <- cal_6P_2nd %>% 
  group_by(Sample, Time) %>% 
  summarise(mean_HE = mean(HE)) # calculating the mean values for plotting

sample_6P_2nd <- ggplot(data = mean_HE_6P_2nd, # plotting by Time and the mean values of HE
       aes(x = Time, 
           y = mean_HE,
           group = Sample,
           color = Sample)) + # too many legends, how to change it ???
  geom_line() +
  geom_point() +
  theme(legend.position = "none") + # remove the legend because it's too large
  # ??? scale_color_continuous("Price") # need to find the right expression
  # ??? scale_color_manual(values = ) # how to change the color just for one line ????
  facet_wrap(~Sample) # problem: too many samples

ggsave("figures/sample_6P_2nd.png", 
       plot = sample_6P_2nd, 
       width = 10, 
       height = 8, 
       units = "cm") # save the plot

control_6P_2nd <- cal_6P_2nd %>% 
  filter(WellGroupType == "Control-" | WellGroupType == "Control+") %>% 
  group_by(Plate)
# choose the control group and save them to a new dataset

control_6P_2nd <- ggplot(data = control_6P_2nd, # plotting for the control samples separately
                     aes(x = Time, 
                         y = HE,
                         group = Plate,
                         color = Sample)) +
  geom_line() +
  geom_point() + # problem: if we don't split it into two graphs, the lines are not correctly conneted
  facet_wrap(~Sample)

ggsave("figures/control_6P_2nd.png", 
       plot = control_6P_2nd, 
       width = 15, 
       height = 8, 
       units = "cm") # save the plot

var_control_6P_2nd <- cal_6P_2nd %>% 
  filter(Sample == "C+" | Sample == "C-") %>% 
  group_by(Sample, Time) %>% 
  summarise(mean_HE = mean(HE), sd_HE = sd(HE), cov = sd_HE / mean_HE * 100)
# calculating the variation for the control samples

write_csv(var_control_6P_2nd, "analysis/var_control_6P_2nd.csv")

var_control_6P_2nd <- ggplot(data = var_control_6P_2nd, # plotting for the control samples using mean and sd
                         aes(x = Time, 
                             y = mean_HE,
                             color = Sample)) + 
  geom_errorbar(aes(ymin=mean_HE - sd_HE, ymax=mean_HE + sd_HE), width=.1) + # adding the sd
  # ??? question here
  geom_line() +
  geom_point() 

ggsave("figures/var_control_6P_2nd.png", 
       plot = var_control_6P_2nd, 
       width = 15, 
       height = 8, 
       units = "cm") # save the plot


#######################################################################################################


# ploting using the data_6P_cal_3nd dataset 

cal_6P_3nd <- read_csv("data/tidydata/data_6P_cal_3nd.csv") # read in the dataset with all the calculation

df <- cal_6P_3nd %>% 
  group_by(Sample, Time) %>% 
  summarise(mean_HE = mean(HE, na.rm = TRUE), # calculating the mean values
            sd_HE = sd(HE, na.rm = TRUE), # calculating the standard error 
            cov = sd_HE / mean_HE *100) # calculating the coefficient of variance
# NaN means that the sample hasn't been repeated


df %>% 
  filter(Time == 240, cov >= 10) # check the big variance samples at t = 360min
# 15 samples

df %>% 
  filter(Time == 360, cov >= 10) # check the big variance samples at t = 360min
# 18 samples

df %>% 
  filter(Time == 1440, cov >= 10) # check the big variance samples at t = 1440min
# 4 samples : 122, 139, 220, 92


df %>% 
  filter(Time ==1800, cov >= 10) # check the big variance samples at t = 1800min
# 5 samples : 134, 139, 83, 92, 96

# Apparently sample 92 and 139 have a quite big variance both at 1440 and 1800min 


sample_6P_3nd <- ggplot(data = df, # plotting by Time and the mean values of HE
                        aes(x = Time, 
                            y = mean_HE,
                            group = Sample,
                            color = Sample %in% c("C+", "C-"))) + 
  geom_line() +
  geom_point() +
  theme(legend.position = "none")  # remove the legends cuz there are too many
  # ??? scale_color_continuous("Price") # need to find the right expression
  # ??? scale_color_manual(values = ) # how to change the color just for one line ???


ggsave("figures/sample_6P_3nd.png", 
       plot = sample_6P_3nd, 
       width = 10, 
       height = 8, 
       units = "cm") # save the plot 
# need to change the thickness of the line !!!!!!!!!!!!!!!

# plot with just the points (3 reps separately)

status <- cal_6P_3nd %>% 
  mutate(status = case_when( # number of conditions (left), discription of the results in that condition
    Sample == "C+" ~ "pos_control", 
    Sample == "C-" ~ "neg_control",
    TRUE ~ "other"
  ))


ggplot(data = status, # plotting by Time and the mean values of HE
                        aes(x = Time, 
                            y = HE,
                            group = Sample,
                            color = status)) +
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
  




########################################################################################################

# take a look at the "outliers"

high_HE <- df %>% 
  filter(mean_HE > 84) # show the high HE samples ( the HE of waxy is 86.7%)
# the only two high HE samples are : 67 and 8 

high_HE <- cal_6P_3nd %>% 
  filter(Sample == "67"| Sample == "8" | Sample == "C+") %>% # choose the high HE samples
  group_by(Sample, Time) %>% # group by sample and time
  mutate(mean_HE = mean(HE, na.rm = TRUE))
  
high_HE <- ggplot(data = high_HE, # plotting by Time and the mean values of HE
       aes(x = Time, 
           y = HE,
           group = Sample,
           color = Sample)) +
  geom_point() + # need to line them up correctly !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  theme(legend.position = "none")

ggsave("figures/high_HE.png", 
       plot = high_HE, 
       width = 10, 
       height = 8, 
       units = "cm") # save the plot


low_HE <- df %>% 
  filter(Time == 1800) %>% 
  filter(mean_HE < 63) # show the low HE samples ( the HE of HAMY is 56.5%)
# the only two low HE samples are : 121 and 92 

low_HE <- cal_6P_3nd %>% 
  filter(Sample == "121"| Sample == "92" | Sample == "C-") %>% # choose the high HE samples
  group_by(Sample, Time) %>% # group by sample and time
  mutate(mean_HE = mean(HE, na.rm = TRUE))

low_HE <- ggplot(data = low_HE, # plotting by Time and the mean values of HE
                  aes(x = Time, 
                      y = HE,
                      group = Sample,
                      color = Sample)) +
  geom_point() + # need to line them up correctly !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  theme(legend.position = "none")

ggsave("figures/low_HE.png", 
       plot = low_HE, 
       width = 10, 
       height = 8, 
       units = "cm") # save the plot
########################################################################################################

# draft : calculate the variance

var_control_6P_2nd <- cal_6P_2nd %>% 
  filter(Sample == "C+" | Sample == "C-") %>% 
  group_by(Sample, Time) %>% 
  summarise(mean_HE = mean(HE), sd_HE = sd(HE), cov = sd_HE / mean_HE * 100)
# calculating the variation for the control samples

write_csv(var_control_6P_2nd, "analysis/var_control_6P_2nd.csv")

var_control_6P_2nd <- ggplot(data = var_control_6P_2nd, # plotting for the control samples using mean and sd
                             aes(x = Time, 
                                 y = mean_HE,
                                 color = Sample)) + 
  geom_errorbar(aes(ymin=mean_HE - sd_HE, ymax=mean_HE + sd_HE), width=.1) + # adding the sd
  # ??? question here
  geom_line() +
  geom_point() 

ggsave("figures/var_control_6P_2nd.png", 
       plot = var_control_6P_2nd, 
       width = 15, 
       height = 8, 
       units = "cm") # save the plot



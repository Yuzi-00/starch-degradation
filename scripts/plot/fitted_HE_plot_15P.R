
#                                        ** using Weibull funcion **

#             ** using three replicates separately to generate one single h k and Xinf for each sample **


library(tidyverse)

# read in the dataset

data_15P <- read_csv("data/tidydata/data_15P_outlier_deleted.csv")

# creat a new column to distingrish each sample 

data_15P <- data_15P %>% 
  mutate(Well = paste(Plate, Row, Column, sep = "_")) 

# this new column will help us do the plot

data_15P <- data_15P %>% 
  mutate(Status = case_when( ## creat a new column to classify three different conditions
    Sample == "C+" ~ "Positive control", ## if the sample name is C+, give it pos_control
    Sample == "C-" ~ "Negative control", ## if the sample name is C-, give it neg_control
    TRUE ~ "Magic population" ## if it's not the cases above, give it other
  )) 

# read in the parameter dataset

para <- read_csv("analysis/fitted_Weibull_parameters_15P.csv")

# create a Time that contains 100 values from 0 --> 1800

Time <- seq(0, 1800, len = 100) 

# enach line of the para subset should repeat 100 times

expanded_para <- para[rep(seq_len(nrow(para)), each = 100), ] 

# add the Time and calculate the fitted HE

fitted_HE <- expanded_para %>% 
  mutate(Time = rep(Time, times = 220)) %>% 
  mutate(fitted_HE = Xinf*(1-exp(-k*Time**(1-h))))

# this new column will help us do the plot

fitted_HE <- fitted_HE %>% 
  mutate(Status = case_when( ## creat a new column to classify three different conditions
    Sample == "C+" ~ "Positive control", ## if the sample name is C+, give it pos_control
    Sample == "C-" ~ "Negative control", ## if the sample name is C-, give it neg_control
    TRUE ~ "Magic population" ## if it's not the cases above, give it other
  )) 

# plotting

ggplot() +
  geom_point(data = data_15P, 
             aes(x = Time,
                 y = HE,
                 group = Well,
                 color = Status),
             size = 1, shape = 1) + ## use hollow circles by "shape = 1"
  geom_line(data = fitted_HE,
            aes(x = Time,
                y = fitted_HE,
                group = Sample,
                color = Status),
            size = 0.05) +
  # make the line thinner
  #if we want to add transparancy to the Samples (try the lines below)
  #geom_line(data = x,
  #aes(x = Time,
  # y = fitted_HE,
  #group = Well,
  #color = status,
  # alpha = status == "Sample"),
  #size = 0.05) +
  #scale_alpha_manual(values = c(1, 0.1)) + # c(FALSE, TRUE)
  scale_color_manual(values = c("#377eb8", "#e41a1c", "#4daf4a")) +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) + ## set the range of the y axis with no more expand
  scale_x_continuous(breaks = c(500, 1000, 1500, 2000),
                     limits = c(0, 1900), expand = c(0, 0)) +
  ylab("Hydrolysis extent (%)") + ## change the label for the y axis
  xlab("Time (min)") + ## change the name of the x axis
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        panel.background = element_blank(),
        axis.ticks=element_line(colour="black", size=.5)) +
  labs(x = "Time (min)", y = "Hydrolysis extent (%)") +
  theme(axis.text.x = element_text(color="black", size=7), 
        axis.text.y = element_text(color="black", size=7)) +
  
  theme(legend.key = element_blank(),
        legend.position = "bottom") +
  theme(plot.margin = unit(c(5.5,12,5.5,5.5), "pt")) + ## set the margin
  theme(strip.background = element_blank())




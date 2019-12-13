

#                                                          ** using Weibull funcion **

#                         ** using three replicates separately to generate one single h k and Xinf for each replicate **


library(tidyverse)

# read in the dataset

data_15P <- read_csv("data/tidydata/data_15P_cal_HE_outlier_replaced.csv")


# this new column will help us do the plot

data_15P <- data_15P %>% 
  mutate(Status = case_when( ## creat a new column to classify three different conditions
    Sample == "C+" ~ "Positive control", ## if the sample name is C+, give it pos_control
    Sample == "C-" ~ "Negative control", ## if the sample name is C-, give it neg_control
    TRUE ~ "Magic population" ## if it's not the cases above, give it other
  )) 

# read in the parameter dataset

para <- read_csv("analysis/fitted_Weibull_parameters_15P_ver2.csv")

# create a Time that contains 100 values from 0 --> 1800

Time <- seq(0, 1800, len = 100) 

# enach line of the para subset should repeat 100 times

expanded_para <- para[rep(seq_len(nrow(para)), each = 100), ] 

# add the Time and calculate the fitted HE

fitted_HE <- expanded_para %>% 
  mutate(Time = rep(Time, times = 680)) %>% # repeat 680 times cuz 680 groups of parameters
  mutate(fitted_HE = Xinf*(1-exp(-k*Time**(1-h))))


#                                        ** add the sample name to the fitted_HE data frame **


# import the design spreadsheet

df <- read_csv("data/tidydata/data_15P_cal_HE.csv")

# create a subset that contains just the sample names and wells

SW <- df %>% 
  select(Well, Sample) %>% 
  unique() # remove the repeated rows

# add the sample names into the fitted_HE data frame 

fitted_HE <- left_join(fitted_HE, SW)

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
                group = Well,
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


#                                   ** individual plot for fitted curves (pdf) ** 

# import the residual dataset

fit <- read_csv("analysis/weibull_residuals.csv")

# import the estimated parameters

para <- read_csv("analysis/fitted_weibull_parameters_for_replicates.csv")

# combine these two datasets above together

total_data <- left_join(fit, para)

pdf(file = "figures/degradability_individual plot_fitted_ver3.pdf") # creating a pdf file and senting all the plot below to this file
for(i in unique(total_data$Well)){ # i stands for each item within this dataset
  # unique() can show all the Sample names here whithin the mean_HE_6P dataset 
  digestibility <- total_data %>% 
    filter(Well == i) %>% # pipe this to the first argument on the right side 
    # here, the first argument of ggplot is the data, otherwise, we have to type ggplot(data = .)
    # to let it pipe to this argument
    ggplot() + 
    geom_line(aes(x = Time, 
                  y = .fitted),
              color = "red") +
    geom_point(aes(x = Time, 
                   y = HE),
               shape = 1) +
    geom_hline(aes(yintercept = Xinf), 
               linetype = "dashed",
               colour = "blue") +
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

# try using the plot() function instead of ggplot()


data_15P <- data_15P %>%
  filter(!(is.na(HE)))

# import the estimated parameters 

para <- read_csv("analysis/fitted_weibull_parameters_for_replicates.csv")

#

pdf(file = "figures/degradability_individual plot_fit_replicate.pdf") # creating a pdf file and senting all the plot below to this file
for(i in unique(data_15P$Well)){ # i stands for each item within this dataset
  # unique() can show all the Sample names here whithin the mean_HE_6P dataset 
  
  model_x <- data_15P %>% 
    filter(Well == i) %>% 
    nls(formula = HE ~ Xinf*(1-exp(-k*Time**H)), # using the Weibull function 
                 data = . ,
                 algorithm = "port", # add this if setting the constrains 
                 start = list(Xinf = 73,
                              k = 0.003,
                              H = 1-0.0005),
                 lower = list(Xinf = 0, # set the lower values fo each parameter
                              k = 0))
                 # control = list(warnOnly = TRUE)
  
  
  data_15P %>% 
    filter(Well == i) %>% # pipe this to the first argument on the right side 
    # here, the first argument of ggplot is the data, otherwise, we have to type ggplot(data = .)
    # to let it pipe to this argument
    select(Time, HE) %>% 
    plot(ylim = c(0, 100), main = i) # set the range of the y axis up to 100
    
  ## creat a data frame that contains 100 time points
  
  new.data <- data.frame(Time = seq(0, 1800, len = 100))
  
  ## draw the new line (fitted line)
  
  lines(new.data$Time, predict(model_x, newdata = new.data), col='red')  
  
} 

dev.off() # stop sending plot to the pdf file



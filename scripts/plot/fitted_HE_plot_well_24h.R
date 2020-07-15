
library(tidyverse)

# read in the dataset

data_15P <- read_csv("data/tidydata/data_15P_cal_HE_outlier_replaced.csv")

# remove the NAs

total <- data_15P %>%
  filter(!(is.na(HE)))

# creat a loop to store all the plots into a pdf document

pdf(file = "figures/fit_HE_24h_well_with_T0.pdf") # creating a pdf file and senting all the plot below to this file
for(i in unique(total$Well)){ # i stands for each item within this dataset
  # unique() can show all the Sample names here whithin the mean_HE_6P dataset 
  
  model_x <- total %>%
    filter(Time != 1800) %>% # remove the last two time points 
    filter(Well == i) %>%
    nls(formula = HE ~ Xinf*(1-exp(-k*Time**H)), # using the Weibull function 
        data = . ,
        algorithm = "port", # add this if setting the constrains 
        start = list(Xinf = 50, # some guessing values of the parameters
                     k = 0.002,
                     H = 1.5),
        lower = list(Xinf = 0, # set the constrains for some of the parameters
                     k = 0),
        upper = list(Xinf = 100))
  # control = list(warnOnly = TRUE)
  
  model_y <- total %>%
    filter(Time > 0) %>% # remove the Time 0 
    filter(Well == i) %>%
    nls(formula = HE ~ Xinf*(1-exp(-k*Time**H)), # using the Weibull function
        data = . ,
        algorithm = "port", # add this if setting the constrains
        start = list(Xinf = 73,
                     k = 0.003,
                     H = 1-0.0005),
        lower = list(Xinf = 0, # set the lower values fo each parameter
                     k = 0))
  
  # creat a data frame that contains 100 time points
  
  newdata <- data.frame(Time = seq(0, 1800, len = 100))
  
  newdata$fitted_x <- predict(model_x, newdata)
  
  newdata$fitted_y <- predict(model_y, newdata)
  
  ggp <- total %>%
    filter(Well == i) %>% # pipe this to the first argument on the right side
    # here, the first argument of ggplot is the data, otherwise, we have to type ggplot(data = .)
    # to let it pipe to this argument
    ggplot() +
    geom_point(aes(x = Time,
                   y = HE),
               shape = 1,
               size = 2) +
    geom_line(data = newdata,
              aes(x = Time,
                  y = fitted_x),
              color = "red") +
    geom_line(data = newdata,
              aes(x = Time,
                  y = fitted_y),
              color = "blue",
              alpha = 0.5) +
    # geom_hline(aes(yintercept = Xinf),
    #            linetype = "dashed",
    #            colour = "blue") +
    ggtitle(i) + # set the title for each plot as i 
    scale_y_continuous(limits = c(-2,100)) + ## set the range of the y axis
    scale_x_continuous(limits = c(0, 2000)) +
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
  
  print(ggp)
  
} 

dev.off()

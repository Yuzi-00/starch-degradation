
pdf(file = "figures/degradability_individual plot_fit_replicate_y.pdf") # creating a pdf file and senting all the plot below to this file
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

  # creat a data frame that contains 100 time points

  newdata <- data.frame(Time = seq(0, 1800, len = 100))

  newdata$fitted <- predict(model_x, newdata)

  ggp <- data_15P %>%
    filter(Well == i) %>% # pipe this to the first argument on the right side
    # here, the first argument of ggplot is the data, otherwise, we have to type ggplot(data = .)
    # to let it pipe to this argument
    ggplot() +
    geom_point(aes(x = Time,
                   y = HE),
               shape = 1) +
    geom_line(data = newdata,
              aes(x = Time,
                  y = fitted),
              color = "red") +
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
  
  print(ggp)
  
} 

dev.off()
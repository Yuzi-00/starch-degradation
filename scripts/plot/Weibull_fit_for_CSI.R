
# use sample 129 to do the curve fitting 

# with 3 separate fits and one average line of these three fitted line

library(tidyverse)

# read in the dataset

data_15P <- read_csv("data/tidydata/data_15P_cal_HE_outlier_replaced.csv")

# remove the NAs

df <- data_15P %>%
  filter(!(is.na(HE))) 

# select just the sample 129

S129 <- df %>%
  filter(Sample == 129)

# separate sample 129 into different well

W1 <- S129 %>%
  filter(Well == "1_A_1")

W2 <- S129 %>%
  filter(Well == "6_D_11")

W3 <- S129 %>%
  filter(Well == "13_F_5")

# creat a model for each well

model_1 <- W1 %>%
  nls(formula = HE ~ Xinf*(1-exp(-k*Time**H)), # using the Weibull function
      data = . ,
      algorithm = "port", # add this if setting the constrains
      start = list(Xinf = 73,
                   k = 0.003,
                   H = 1-0.0005),
      lower = list(Xinf = 0, # set the lower values fo each parameter
                   k = 0))
# control = list(warnOnly = TRUE)

model_2 <- W2 %>%
  nls(formula = HE ~ Xinf*(1-exp(-k*Time**H)), # using the Weibull function
      data = . ,
      algorithm = "port", # add this if setting the constrains
      start = list(Xinf = 73,
                   k = 0.003,
                   H = 1-0.0005),
      lower = list(Xinf = 0, # set the lower values fo each parameter
                   k = 0))

model_3 <- W3 %>%
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

newdata$fitted_1 <- predict(model_1, newdata)

newdata$fitted_2 <- predict(model_2, newdata)

newdata$fitted_3 <- predict(model_3, newdata)

# calculate the mean value of the above three fitted points (for plotting the average line)

new_mean <- newdata %>%
  mutate(mean = (fitted_1 + fitted_2 + fitted_3)/3)

# plot

ggp <- ggplot() +
  
  # first well
  
  geom_point(data = W1,
    aes(x = Time,
        y = HE),
             shape = 1,
             size = 2) +
  geom_line(data = newdata,
            aes(x = Time,
                y = fitted_1),
            color = "red") +
  
  # second well
  
  geom_point(data = W2,
             aes(x = Time,
                 y = HE),
             shape = 1,
             size = 2) +
  geom_line(data = newdata,
            aes(x = Time,
                y = fitted_2),
            color = "red") +
  
  # third well
  
  geom_point(data = W3,
             aes(x = Time,
                 y = HE),
             shape = 1,
             size = 2) +
  geom_line(data = newdata,
            aes(x = Time,
                y = fitted_3),
            color = "red") +
  
  # add the average line (the average of the above three lines)
  
  geom_line(data = new_mean,
            aes(x = Time,
                y = mean),
            color = "blue",
            linetype = "dashed") +
  scale_y_continuous(limits = c(0,105), expand = c(0, 0)) + ## set the range of the y axis
  scale_x_continuous(limits = c(0, 2050), expand = c(0, 0)) +
  theme( # remove the legend
    panel.grid = element_blank(), # remove the grid 
    axis.line = element_line(colour = "black", size = 0.5), # add the x axis
    panel.background = element_rect(fill = "white", color = "black"),
    #change the backgroud color to white and the frame color to black
    axis.ticks = element_line(colour="black", size=.5)) +
  # change the color of the ticks into black and the size to 0.5
  labs(x = "Time (min)", y = "Hydrolysis extent (%)") +
  # change the title of the x and y axis
  theme(axis.text.x = element_text(color="black", size=20), 
        axis.text.y = element_text(color="black", size=20)) +
  # change the color and size of the tick label for x and y axis
  theme(plot.margin = unit(c(5.5,12,5.5,5.5), "pt")) +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

ggp

# save the plot 

ggsave("figures/weibull_fit_CSI.png", 
       plot = ggp,
       width = 25, 
       height = 20, 
       units = "cm") 

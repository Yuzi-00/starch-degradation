#                                                          ** using Weibull funcion **

#                         ** using three replicates separately to generate one single h k and Xinf for each replicate **


library(tidyverse)

# read in the dataset

data_15P <- read_csv("data/tidydata/data_15P_cal_HE_outlier_replaced.csv")


data_15P <- data_15P %>%
  filter(!(is.na(HE))) 



pdf(file = "figures/degradability_fit_replicates_logistic.pdf") # creating a pdf file and senting all the plot below to this file
for(i in unique(data_15P$Well)){ # i stands for each item within this dataset
  # unique() can show all the Sample names here whithin the mean_HE_6P dataset 
  
  model_x <- data_15P %>% 
    filter(Well == i) %>% 
    nls(formula = HE ~ Xinf/(1+((Xinf/X0)-1)*exp(-k*Time)), # using the Weibull function 
        data = hydro_data,
        algorithm = "port", # add this if setting the constrains 
        start = list(Xinf = 73,
                     X0 = 1,
                     k = 0.003),
        lower = list(Xinf = 0, 
                     k = 0))
  
  
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

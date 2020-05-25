

# please run the curve_fitting_well script before this one !!!!!!

# extract the sigma (RSE) of the model

####

fit_model <- function(dfr){ # a dataframe will be passed through the following things 
  
  
  # built the model
  
  model <- nls(formula = HE ~ Xinf*(1-exp(-k*Time**H)), # using the Weibull function 
               data = dfr,
               algorithm = "port", # add this if setting the constrains 
               start = list(Xinf = 73, # some guessing values of the parameters
                            k = 0.003,
                            H = 1-0.0005),
               lower = list(Xinf = 0, # set the constrains for some of the parameters 
                            k = 0))
  # control = list(warnOnly = TRUE)) ## change the setting, give us just the waining messages instead of error messages
  
  result <- glance(model) # get the overview of the fitting
  result # give back the content of the model
  
}

fit_list <- hydro_data %>% 
  split(.$Well) %>% # split the initial dataset by Well into a list (680 wells)
  map_df(fit_model, .id = "Well")

#### plot RSE (sigma)

ggplot(data = fit_list,
       aes(x = Well,
           y = sigma)) +
  geom_point()

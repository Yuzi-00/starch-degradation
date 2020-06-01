
library(tidyverse)

# import the fitted dataset 

df <- read_csv("analysis/weibull_residuals_well_with_T0.csv") %>%
  select(-.resid) # remove the residual generated automatically by R, we're going to calculate
                  # the residuals by ourself

# import a dataset that contains sample and well columns 

df0 <- read_csv("analysis/total_new_convert.csv") %>%
  select(Sample, Well) # select just the sample and well columns 

# add the sample name into the residual dataset

df1 <- left_join(df, df0)

# calculate the standarised residual for each sample at each time point 

res <- df1 %>%
  filter(Time != 0) %>% # remove time = 0 cuz this will disturb the calculation of std_resid
  mutate(resid = HE - .fitted) %>%
  group_by(Sample, Time) %>%
  mutate(mean_resid = mean(resid), sd_fit = sd(.fitted),
         std_resid = mean_resid / sd_fit)

# plot

ggplot(data = res,
       aes(x = .fitted,
           y = std_resid)) +
  geom_point() +
  scale_y_continuous(limits = c(-5,5))

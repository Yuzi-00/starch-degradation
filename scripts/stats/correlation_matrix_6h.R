# install.packages("PerformanceAnalytics")

library("PerformanceAnalytics")

library(tidyverse)

# import the dataset

df <- read_csv("analysis/fitted_weibull_parameters_6h_well_with_T0.csv")

df2 <- read_csv("analysis/total_new_convert.csv") %>%
  select(Well, Sample)

df3 <- left_join(df, df2)

# calculate h

df3 <- df3 %>%
  mutate(h = 1 - H) %>%
  select(-H)

#### chart.Correlation ####

df_sel <- df3 %>%
  filter(Sample != "C+" & Sample != "C-") %>%
  select(-Well, -Sample)


chart.Correlation(df_sel, histogram=TRUE, pch=19)

# positive control

df_sel <- df3 %>%
  filter(Sample == "C+") %>%
  select(-Well, -Sample)


chart.Correlation(df_sel, histogram=TRUE, pch=19)

# negative control

df_sel <- df3 %>%
  filter(Sample == "C-") %>%
  select(-Well, -Sample)


chart.Correlation(df_sel, histogram=TRUE, pch=19)

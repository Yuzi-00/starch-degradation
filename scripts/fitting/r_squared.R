
# run the curve_fitting_well script before this one !!!!!!
# we are gonna use the residual_data dataset to calculate the r squared 

# add the sample column into the residual_data dataset

df <- read_csv("data/tidydata/joined_15P_update.csv") %>%
  select(Well, Sample) %>%
  unique()

residual_data <- left_join(residual_data, df)

#### R2 for test samples ####

# calculate the r-squared for each well sample (MAGIC population)

rs1 <- residual_data %>%
  filter(Sample != "C+" & Sample != "C-") 

rs1 <- rs1 %>%
  group_by(Well) %>%
  mutate(rss = sum((HE - .fitted) ^ 2),
         # calculate the residual sum of squares (rss)
         tss = sum((HE - mean(HE)) ^ 2),
         # calculate the total sum of squares (tss)
         r_squared = 1 - rss/tss) %>%
  ungroup()

# calculate the average r-squared for all samples (MAGIC population)

rs1 %>%
  summarise(mean_rs = mean(r_squared, na.rm = TRUE),
            sd_rs = sd(r_squared, na.rm = TRUE))
# r-squared -> mean = 0.998, sd = 0.00225


#### R2 for positive controls ####

# calculate the r-squared for each well sample (MAGIC population)

rs2 <- residual_data %>%
  filter(Sample == "C+") 

rs2 <- rs2 %>%
  group_by(Well) %>%
  mutate(rss = sum((HE - .fitted) ^ 2),
         # calculate the residual sum of squares (rss)
         tss = sum((HE - mean(HE)) ^ 2),
         # calculate the total sum of squares (tss)
         r_squared = 1 - rss/tss) %>%
  ungroup()

# calculate the average r-squared for all samples (MAGIC population)

rs2 %>%
  summarise(mean_rs = mean(r_squared, na.rm = TRUE),
            sd = sd(r_squared, na.rm = TRUE)) 
# mean = 0.995, sd = 0.00307

#### R2 for negative controls ####

# calculate the r-squared for each well sample (MAGIC population)

rs3 <- residual_data %>%
  filter(Sample == "C-") %>%
  group_by(Well) %>%
  mutate(rss = sum((HE - .fitted) ^ 2),
         # calculate the residual sum of squares (rss)
         tss = sum((HE - mean(HE)) ^ 2),
         # calculate the total sum of squares (tss)
         r_squared = 1 - rss/tss) %>%
  ungroup()

# calculate the average r-squared for all samples (MAGIC population)

rs3 %>%
  summarise(mean_rs = mean(r_squared, na.rm = TRUE),
            sd = sd(r_squared, na.rm = TRUE))
# mean = 0.995, sd = 0.00313
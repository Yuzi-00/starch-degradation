 

#                                                      ** tidy the dataset **


library(tidyverse)

library(readxl)

filename1 <- "C:/Users/WAN333/Documents/Thesis/Experiments/2020-01/20200107_DNS_Qin/raw_data/20200107_DNS_Qin_t"
filename2 <- 'min.xlsx'
# assigning the unchangeable part of the path

filetime <- c('20','60','120','180') 
# assigning the changeable part of the path

df <- read_excel(paste0(filename1,'0',filename2), range = "A15:E19") 
# read the 1st spreadsheet et and assign it to a new dataframe

df <- mutate(df, time = 0)
# add two columns: plate number and time point for the 1st spreadsheet

for (fltime in filetime) {
  filename_real <- paste0(filename1,fltime,filename2)
  print(filename_real)
  temp_data <- read_excel(filename_real, range = "A15:E19",na = 'NA')
  temp_data <- mutate(temp_data, time = as.numeric(fltime))
  df <- bind_rows(df, temp_data)
}
# creat a loop to import all the spreadsheet
# add two columns: plate number and time point from the 2nd spreadsheet on 
# bind them by raws

df <- rename(df, raw = ...1) 
# modify the default name for the first col & raw


df <- gather(df, col, OD, -raw, -time) 
# transfer the data into two columns 

df <- select(df, raw, col, time, OD) %>% 
  rename(Col = col,
         Row = raw,
         Time = time) %>% 
  arrange(Row)
# ordering the column names and arrange by the raw

write_csv(df, "data/Qin/tidydata/DNS_data.csv")

# check the NAs

filter(df,is.na(OD)) %>% 
  nrow() # 0 NAs


#                                                      ** tidy the mass data **


# import the dataset

mass <- read_xlsx("C:/Users/WAN333/Documents/Thesis/Experiments/2020-01/20200107_DNS_Qin/raw_data/mass_starch.xlsx", range = "A1:E5")

# rename the first cell

mass <- rename(mass, Row = ...1) 

# # transfer the data into two columns

mass <- mass %>%  
  gather(col, mass, -Row)

# arrange by Row

mass <- mass %>% 
  rename(Column = col, Mass = mass) %>% 
  arrange(Row)

# in order to join this dataset with the other, each row has to repete 5 times 

mass_rep  <- mass[rep(seq_len(nrow(mass)), each = 5), ] # 16*5 = 80 rows

# replace all the 0 by NA

mass_rep <- mass_rep %>% 
  mutate(Mass = na_if(Mass, "0")) # if the value in column Mass is 0, replace them by NA 

# check the NA

missing_value <- mass_rep %>% 
  filter(is.na(Mass)) # 20 NAs correspond to 20 controls 

# save the total mass dataset

write_csv(mass_rep, "data/Qin/tidydata/mass_tidy.csv")


#                                                      ** combination **


# read in the tidied mass dataset

mass <- read_csv("data/Qin/tidydata/mass_tidy.csv") 

df <- read_csv("data/Qin/tidydata/DNS_data.csv")

# select just the Mass column to be merged in the next step

mass_selected <- select(mass, Mass)

df_with_mass <-  bind_cols(df, mass_selected) %>% 
  select(Row, Col, Time, Mass, OD) 
# ordering the columns

# import the sample name

sample_name <- read_xlsx("C:/Users/WAN333/Documents/Thesis/Experiments/2020-01/20200107_DNS_Qin/raw_data/sample_name.xlsx", range = "A2:C18")

# add the sample name to the previous dataset

df_with_mass_sample <- left_join(df_with_mass, sample_name) %>% 
  select(Row, Col, Sample, Time, Mass, OD)

# extract the controls

controls_with_enz <- df_with_mass_sample %>% 
  filter(Row == "D") %>% 
  filter(Sample == "pos_with_enz" | Sample == "neg_with_enz")

controls_without_enz <- df_with_mass_sample %>% 
  filter(Row == "D") %>% 
  filter(Sample == "pos_without_enz" | Sample == "neg_without_enz")

# calculate the mean value of for each control

with_enz <- controls_with_enz %>% 
  group_by(Time) %>% 
  summarise(mean_OD = mean(OD))

controls_with_enz <- left_join(controls_with_enz, with_enz)

without_enz <- controls_without_enz %>% 
  group_by(Time) %>% 
  summarise(mean_OD = mean(OD))

controls_without_enz <- left_join(controls_without_enz, without_enz)

# select the useful columns

controls_with_enz <- controls_with_enz %>% 
  select(Sample, Time, mean_OD)

controls_without_enz <- controls_without_enz %>% 
  select(Sample, Time, mean_OD)

# extract the test sample with enzyme

sample_with_enz <- df_with_mass_sample %>% 
  filter(Sample == "pos_with_enz" | Sample == "neg_with_enz") %>% 
  filter(Row != "D") # remove the row for control

sample_without_enz <- df_with_mass_sample %>% 
  filter(Sample == "pos_without_enz" | Sample == "neg_without_enz") %>% 
  filter(Row != "D") # remove the row for control

# combine with the control data

sample_with_enz <- left_join(sample_with_enz, controls_with_enz)

sample_without_enz <- left_join(sample_without_enz, controls_without_enz)

# combine the previous two datasets together

total_df <- bind_rows(sample_with_enz, sample_without_enz)

#                                                      ** calculation **


# import the dataset

df <- read_csv("data/Qin/tidydata/DNS_data.csv")

# calculate the concentration (C) of the reducing sugar for the sample and the blank: C = OD/slope

df_cal <- df %>% 
  mutate(C_sample = OD_sample / Mean_slope, # calculate the concentration using the mean slope by plate
         C_blk = OD_blk / Mean_slope,
         
         # the mass of the sample was weighed between 9.5mg and 10.5 mg, which are all normalized to 10mg while calculating the concentration
         
         C_spl_nor = 10 * C_sample / Mass_sample, 
         C_blk_nor = 10 * C_blk / Mass_blk)

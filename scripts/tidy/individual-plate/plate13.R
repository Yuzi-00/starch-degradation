library(tidyverse)

library(readxl)

# assigning the unchangeable part of the path

filename1 <- "C:/Users/WAN333/Documents/Thesis/Experiments/2019-11/20191108_Hydrolysis Plate 13/raw data/20191108_DNS plate13-t"
filename2 <- 'min.xlsx'

# assigning the changeable part of the path

filetime <- c('20','60','120','180','240', '360', '1440', '1800') 

# read the 1st spreadsheet et and assign it to a new dataframe

data_plate13 <- read_excel(paste0(filename1,'0',filename2), range = "A15:M23") 

# add two columns: plate number and time point for the 1st spreadsheet

data_plate13 <- mutate(data_plate13, plate = 13, time = 0)

# creat a loop to import all the spreadsheet

for (fltime in filetime) {
  filename_real <- paste0(filename1,fltime,filename2)
  print(filename_real)
  temp_data <- read_excel(filename_real, range = "A15:M23", na = 'NA')
  temp_data <- mutate(temp_data,  plate = 13, time = as.numeric(fltime))
  # add two columns: plate number and time point 
  data_plate13 <- bind_rows(data_plate13, temp_data) # bind them by raws
}

# modify the default name for the first col & raw

data_plate13 <- rename(data_plate13, raw = ...1) 

# transfer the data into two columns

data_plate13 <- gather(data_plate13, col, OD, -raw, -plate, -time) 

# ordering the column names and arrange by the raw

data_plate13 <- select(data_plate13, plate, raw, col, time, OD) %>% 
  arrange(raw)

# replace all the row F, column 11 and 12 into NA

data_plate13 <- data_plate13 %>% 
  mutate(OD = na_if(OD, "0")) # if the value in column OD is 0, replace them by NA 

# write out the final dataset

write_csv(data_plate13, "data/tidydata/plate13.csv")

# check the missing values

filter(data_plate13,is.na(OD)) %>% 
  nrow() # 18 NAs, which is correct








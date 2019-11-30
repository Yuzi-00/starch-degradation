library(tidyverse)

library(readxl)

filename1 <- "C:/Users/WAN333/Documents/Thesis/Experiments/2019-09/20190910_Hydrolysis Plate 01/raw data/20190910_DNS plate01-t"
filename2 <- 'min.xlsx'
# assigning the unchangeable part of the path

filetime <- c('20','60','120','180','240', '360', '1440', '1800') 
# assigning the changeable part of the path

data_plate01 <- read_excel(paste0(filename1,'0',filename2), range = "A15:M23") 
# read the 1st spreadsheet et and assign it to a new dataframe

data_plate01 <- mutate(data_plate01, plate = 1, time = 0)
# add two columns: plate number and time point for the 1st spreadsheet

for (fltime in filetime) {
  filename_real <- paste0(filename1,fltime,filename2)
  print(filename_real)
  temp_data <- read_excel(filename_real, range = "A15:M23",na = 'NA')
  temp_data <- mutate(temp_data,  plate = 1, time = as.numeric(fltime))
  data_plate01 <- bind_rows(data_plate01, temp_data)
}
# creat a loop to import all the spreadsheet
# add two columns: plate number and time point from the 2nd spreadsheet on 
# bind them by raws

data_plate01 <- rename(data_plate01, raw = ...1) 
# modify the default name for the first col & raw


data_plate01 <- gather(data_plate01, col, OD, -raw, -plate, -time) 
# transfer the data into two columns 

data_plate01 <- select(data_plate01, plate, raw, col, time, OD) %>% 
  arrange(raw)
# ordering the column names and arrange by the raw

# replace all the row G, column 9 and 10 into NA

data_plate01 <- data_plate01 %>% 
  mutate(OD = na_if(OD, "0")) # if the value in column OD is 0, replace them by NA 

write_csv(data_plate01, "data/tidydata/plate01.csv")

# check the NAs

filter(data_plate01,is.na(OD)) %>% 
  nrow() # 18 NAs, which is right



library(tidyverse)

library(readxl)

filename1 <- "C:/Users/WAN333/Documents/Thesis/Experiments/2019-09/20190930_Hydrolysis Plate 05/raw data/20190930_DNS plate05-t"
filename2 <- 'min.xlsx'
# assigning the unchangeable part of the path

filetime <- c('20','60','120','180','240', '360', '1440', '1800') 
# assigning the changeable part of the path

data_plate05 <- read_excel(paste0(filename1,'0',filename2), range = "A15:M23") 
# read the 1st spreadsheet et and assign it to a new dataframe

data_plate05 <- mutate(data_plate05, plate = 5, time = 0)
# add two columns: plate number and time point for the 1st spreadsheet

for (fltime in filetime) {
  filename_real <- paste0(filename1,fltime,filename2)
  print(filename_real)
  temp_data <- read_excel(filename_real, range = "A15:M23",na = 'NA')
  temp_data <- mutate(temp_data,  plate = 5, time = as.numeric(fltime))
  data_plate05 <- bind_rows(data_plate05, temp_data)
}
# creat a loop to import all the spreadsheet
# add two columns: plate number and time point from the 2nd spreadsheet on 
# bind them by raws


data_plate05 <- rename(data_plate05, raw = ...1) 
# modify the default name for the first col & raw

data_plate05 <- gather(data_plate05, col, OD, -raw, -plate, -time) 
# transfer the data into two columns 

data_plate05 <- select(data_plate05, plate, raw, col, time, OD) %>% 
  arrange(raw)
# ordering the column names and arrange by the raw

# replace all the row A, column 9 and 10 into NA

data_plate05 <- data_plate05 %>% 
  mutate(OD = na_if(OD, "0")) # if the value in column OD is 0, replace them by NA 

write_csv(data_plate05, "data/tidydata/plate05.csv")

filter(data_plate05,is.na(OD)) %>% 
  nrow() # 18 NAs, which is right




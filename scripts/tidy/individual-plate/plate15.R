library(tidyverse)

library(readxl)

filename1 <- "C:/Users/WAN333/Documents/Thesis/Experiments/2019-11/20191118_Hydrolysis Plate 15/raw data/20191118_DNS plate15-t"
filename2 <- 'min.xlsx'
# assigning the unchangeable part of the path

filetime <- c('20','60','120','180','240', '360', '1440', '1800') 
# assigning the changeable part of the path

data_plate15 <- read_excel(paste0(filename1,'0',filename2), range = "A15:M23") 
# read the 1st spreadsheet et and assign it to a new dataframe

data_plate15 <- mutate(data_plate15, plate = 15, time = 0)
# add two columns: plate number and time point for the 1st spreadsheet

for (fltime in filetime) {
  filename_real <- paste0(filename1,fltime,filename2)
  print(filename_real)
  temp_data <- read_excel(filename_real, range = "A15:M23",na = 'NA')
  temp_data <- mutate(temp_data,  plate = 15, time = as.numeric(fltime))
  data_plate15 <- bind_rows(data_plate15, temp_data)
}
# creat a loop to import all the spreadsheet
# add two columns: plate number and time point from the 2nd spreadsheet on 
# bind them by raws

data_plate15 <- rename(data_plate15, raw = ...1) 
# modify the default name for the first col & raw


data_plate15 <- gather(data_plate15, col, OD, -raw, -plate, -time) 
# transfer the data into two columns 

data_plate15 <- select(data_plate15, plate, raw, col, time, OD) %>% 
  arrange(raw)
# ordering the column names and arrange by the raw

# replace the column 7 and 8, row A, B, D
# replace the row H from column 3 to 12

data_plate15 <- data_plate15 %>% 
  mutate(OD = na_if(OD, "0")) # if the value in column OD is 0, replace them by NA 

write_csv(data_plate15, "data/tidydata/plate15.csv")

# check the missing values

filter(data_plate15,is.na(OD)) %>% 
  nrow() # 144 NAs, which is correct




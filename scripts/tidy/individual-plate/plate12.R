library(tidyverse)

library(readxl)

# assigning the unchangeable part of the path

filename1 <- "C:/Users/WAN333/Documents/Thesis/Experiments/2019-11/20191116_Hydrolysis Plate 12/raw data/20191116_DNS plate12_repeat-t"
filename2 <- 'min.xlsx'

# assigning the changeable part of the path

filetime <- c('20','60','120','180','240', '360', '1440', '1800') 

# read the 1st spreadsheet et and assign it to a new dataframe

data_plate12 <- read_excel(paste0(filename1,'0',filename2), range = "A15:M23") 

# add two columns: plate number and time point for the 1st spreadsheet

data_plate12 <- mutate(data_plate12, plate = 12, time = 0)

# creat a loop to import all the spreadsheet

for (fltime in filetime) {
  filename_real <- paste0(filename1,fltime,filename2)
  print(filename_real)
  temp_data <- read_excel(filename_real, range = "A15:M23", na = 'NA')
  temp_data <- mutate(temp_data,  plate = 12, time = as.numeric(fltime))
  # add two columns: plate number and time point 
  data_plate12 <- bind_rows(data_plate12, temp_data) # bind them by raws
}

# modify the default name for the first col & raw

data_plate12 <- rename(data_plate12, raw = ...1) 

# transfer the data into two columns

data_plate12 <- gather(data_plate12, col, OD, -raw, -plate, -time) 

# ordering the column names and arrange by the raw

data_plate12 <- select(data_plate12, plate, raw, col, time, OD) %>% 
  arrange(raw)

# replace the row G, column 7 and 8 into NA
# replace the row H, column 9 and 10 into NA

data_plate12 <- data_plate12 %>% 
  mutate(OD = na_if(OD, "0")) # if the value in column OD is 0, replace them by NA 

# write out the final dataset

write_csv(data_plate12, "data/tidydata/plate12.csv")

# check if there are any missing values

filter(data_plate12,is.na(OD)) %>% 
  nrow() # 36 NAs, which is correct








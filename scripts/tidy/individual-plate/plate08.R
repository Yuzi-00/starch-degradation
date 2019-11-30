library(tidyverse)

library(readxl)

# assigning the unchangeable part of the path

filename1 <- "C:/Users/WAN333/Documents/Thesis/Experiments/2019-10/20191021_Hydrolysis Plate 08/raw data/20191021_DNS plate08-t"
filename2 <- 'min.xlsx'

# assigning the changeable part of the path

filetime <- c('20','60','120','180','240', '360', '1440', '1800') 

# read the 1st spreadsheet et and assign it to a new dataframe

data_plate08 <- read_excel(paste0(filename1,'0',filename2), range = "A15:M23") 

# add two columns: plate number and time point for the 1st spreadsheet

data_plate08 <- mutate(data_plate08, plate = 8, time = 0)

# creat a loop to import all the spreadsheet

for (fltime in filetime) {
  filename_real <- paste0(filename1,fltime,filename2)
  print(filename_real)
  temp_data <- read_excel(filename_real, range = "A15:M23", na = 'NA')
  temp_data <- mutate(temp_data,  plate = 8, time = as.numeric(fltime))
  # add two columns: plate number and time point 
  data_plate08 <- bind_rows(data_plate08, temp_data) # bind them by raws
}

# modify the default name for the first col & raw

data_plate08 <- rename(data_plate08, raw = ...1) 

# transfer the data into two columns

data_plate08 <- gather(data_plate08, col, OD, -raw, -plate, -time) 

# ordering the column names and arrange by the raw

data_plate08 <- select(data_plate08, plate, raw, col, time, OD) %>% 
  arrange(raw)

# write out the final dataset

write_csv(data_plate08, "data/tidydata/plate08.csv")

# check if there are any missing values

filter(data_plate08,is.na(OD)) %>% 
  nrow()








library(tidyverse)

library(readxl)

filename1 <- "C:/Users/WAN333/Documents/Thesis/Experiments/2019-10/20191004_Hydrolysis Plate 06/raw data/20191004_DNS plate06-t"
filename2 <- 'min.xlsx'
# assigning the unchangeable part of the path

filetime <- c('20','60','120','180','240', '360', '1440', '1800') 
# assigning the changeable part of the path

data_plate06 <- read_excel(paste0(filename1,'0',filename2), range = "A15:M23") 
# read the 1st spreadsheet et and assign it to a new dataframe

data_plate06 <- mutate(data_plate06, plate = 6, time = 0)
# add two columns: plate number and time point for the 1st spreadsheet

for (fltime in filetime) {
  filename_real <- paste0(filename1,fltime,filename2)
  print(filename_real)
  temp_data <- read_excel(filename_real, range = "A15:M23", na = 'NA')
  temp_data <- mutate(temp_data,  plate = 6, time = as.numeric(fltime))
  data_plate06 <- bind_rows(data_plate06, temp_data)
}
# creat a loop to import all the spreadsheet
# add two columns: plate number and time point from the 2nd spreadsheet on 
# bind them by raws


data_plate06 <- rename(data_plate06, raw = ...1) 
# modify the default name for the first col & raw

data_plate06 <- gather(data_plate06, col, OD, -raw, -plate, -time) 
# transfer the data into two columns 

data_plate06 <- select(data_plate06, plate, raw, col, time, OD) %>% 
  arrange(raw)
# ordering the column names and arrange by the raw

# replace all the row H, column 11 and 12 into NA

# replace col 5&6, col 9&10

data_plate06 <- data_plate06 %>% 
  mutate(OD = na_if(OD, "0")) # if the value in column OD is 0, replace them by NA 

write_csv(data_plate06, "data/tidydata/plate06.csv")

filter(data_plate06,is.na(OD)) %>% 
  nrow() # 306 NAs, which is right

# can't compare(==) NA, for ex, if you enter in the console NA == NA, it will give you NA
# so you can't actually compare a missing data
# use the function is.na(), put the col name within the ()
# if you want all the data expect for the NA, just put a ! in front of the is.na()
  # other option to filter the NA is: filter(data_plate06, OD %in% NA)
  








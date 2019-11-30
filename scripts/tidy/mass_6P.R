library(tidyverse)

library(readxl)

filename1 <- "C:/Users/WAN333/Documents/Thesis/Experiments/raw_data/starch_mass/starch plate"
filename2 <- '.xlsx'
# assigning the unchangeable part of the path

filenumber <- c('02', '03', '04', '05', '06') 
# assigning the changeable part of the path, other methods ???

data_plate01 <- read_excel(paste0(filename1,'01',filename2), range = "A1:M9") %>% 
  mutate(Plate = "01")
# read the 1st spreadsheet et and assign it to a new dataframe

for (flnum in filenumber) {
  filename_real <- paste0(filename1,flnum,filename2)
  print(filename_real)
  temp_data <- read_excel(filename_real, range = "A1:M9")
  temp_data <- mutate(temp_data, Plate = flnum) # adding plate numbers
  data_plate01 <- bind_rows(data_plate01, temp_data)
}
# creat a loop to import all the spreadsheet
# add column: plate number from the 2nd spreadsheet on 
# bind them by raws

data_6P <- data_plate01 %>% 
  mutate(Plate = sub("0", "", data_plate01$Plate))
# removing the 0 in the plate numbers and renaming the dataset

data_6P <- rename(data_6P, row = ...1) 
# replacing the default name by "row" 

data_6P_gathered <- data_6P %>%  
  gather(col, mass, -row, -Plate) 
# transfer the data into two columns, remaining the row and Plate unchanged

data_6P_ordered <- select(data_6P_gathered, Plate, row, col, mass) %>% 
  arrange(Plate, row)
# ordering the column names and arrange by the raw

mass_6P  <- data_6P_ordered[rep(seq_len(nrow(data_6P_ordered)), each = 9), ] 
# in order to join this dataset with the other two, each row has to repete 9 times 

write_csv(mass_6P, "data/tidydata/mass_6P.csv")





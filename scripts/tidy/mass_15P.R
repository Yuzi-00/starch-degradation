library(tidyverse)

library(readxl)

# assigning the unchangeable part of the path

filename1 <- "C:/Users/WAN333/Documents/Thesis/Experiments/raw_data/starch_mass/starch plate"
filename2 <- '.xlsx'

# assigning the changeable part of the path

filenumber <- c('02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '14', '15') 
# note that the plate 12 used here is the reapeated spreadsheet, even though it's named as plate 12, not plate 12 repeated 

# read the 1st spreadsheet et and assign it to a new dataframe

data_plate01 <- read_excel(paste0(filename1,'01',filename2), range = "A1:M9") %>% 
  mutate(Plate = "01")

# creat a loop to import all the spreadsheet

for (flnum in filenumber) {
  filename_real <- paste0(filename1,flnum,filename2)
  print(filename_real)
  temp_data <- read_excel(filename_real, range = "A1:M9")
  temp_data <- mutate(temp_data, Plate = flnum) # add column: plate number from the 2nd spreadsheet on 
  data_plate01 <- bind_rows(data_plate01, temp_data) # bind them by raws
}

# split into two dataset

data_9P <- data_plate01 %>% 
  filter(Plate == "01" | Plate == "02" | Plate == "03" | Plate == "04" | Plate == "05" | 
           Plate == "06" | Plate == "07" | Plate == "08" | Plate == "09") # 8*9 = 72 rows

rest <- data_plate01 %>% 
  filter(Plate == "10" | Plate == "11" | Plate == "12" | Plate == "13" | Plate == "14" | Plate == "15")

# removing the 0 in the plate numbers for the first 9 plates

data_9P <- data_9P %>% 
  mutate(Plate = sub("0", "", data_9P$Plate))

# recombine these two dataset together

mass <- bind_rows(data_9P, rest)

# replacing the default name by "row" 

mass <- rename(mass, Row = ...1) 

# transfer the data into two columns, remaining the row and Plate column unchanged

mass_gathered <- mass %>%  
  gather(col, mass, -Row, -Plate) # 96*15 = 1440 rows

# ordering the column names and arrange by Plate and Row

mass_15P <- mass_gathered %>% 
  rename(Column = col, Mass = mass) %>% 
  transform(Plate = as.numeric(Plate)) %>% # change the Plate column from chr to dbl (this is for the arrange step later on)
  select(Plate, Row, Column, Mass) %>% 
  arrange(Plate, Row)

# in order to join this dataset with the other, each row has to repete 9 times 

mass_15P  <- mass_15P[rep(seq_len(nrow(mass_15P)), each = 9), ] # 1440*9 = 12960 rows

# replace all the 0 by NA

mass_15P <- mass_15P %>% 
  mutate(Mass = na_if(Mass, "0")) # if the value in column Mass is 0, replace them by NA 

# check the NA

missing_value <- mass_15P %>% 
  filter(is.na(Mass)) # 414 NAs, which is correct (five empty samples, two 82*, ten blanks in plate 15, one 49)

# save the total mass dataset

write_csv(mass_15P, "data/tidydata/mass_15P.csv")





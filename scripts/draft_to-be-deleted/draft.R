library(readxl)

filename1 <- "C:/Users/WAN333/Documents/Thesis/Experiments/2019-10/20191004_Hydrolysis Plate 06/raw data/20191004_DNS plate06-t"
filename2 <- 'min.xlsx'

filetime <- c('20','60','120','180','240', '360', '1440', '1800') 
# read 2nd file to the last
data <- read_excel(paste0(filename1,'0',filename2), range = "A15:M23") 
# read the 1st spreadsheet

for (fltime in filetime) {
  filename_real <- paste0(filename1,fltime,filename2)
  print(filename_real)
  temp_data <- read_excel(filename_real, range = "A15:M23",na = 'NA')
  data <- bind_rows(data,temp_data)
}
#-----------------------------------------------------------------------------------------

library(tidyverse)

library(readxl)


filename1 <- "C:/Users/WAN333/Documents/Thesis/Experiments/2019-09/20190910_Hydrolysis Plate 01/raw data/20190910_DNS plate01-t"
filename2 <- 'min.xlsx'

filetime <- c('20','60','120','180','240', '360', '1440', '1800') 
# read 2nd file to the last 
data_plate01 <- read_excel(paste0(filename1,'0',filename2), range = "A15:M23") 

data_plate01 <- mutate(data_plate01, plate = 1, time = 0)

# data_plate01 <- mutate(data_plate01, filename = paste0('plate1', '_0'))
# read the 1st spreadsheet

for (fltime in filetime) {
  filename_real <- paste0(filename1,fltime,filename2)
  print(filename_real)
  temp_data <- read_excel(filename_real, range = "A15:M23",na = 'NA')
  temp_data <- mutate(temp_data, plate = 1, time = as.numeric(fltime))
                      # filename = paste0('plate1','_',fltime))
  data_plate01 <- bind_rows(data_plate01, temp_data)
}


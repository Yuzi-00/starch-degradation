library(tidyverse)

library(readxl)

mass <- read_xlsx("C:/Users/WAN333/Documents/Thesis/Experiments/raw_data/starch_mass/mass_plate1-6.xlsx")
# read in the mass data which were already partly gathered manually in excel (adding plate numbers)
# need to figure it out how to do it in R 

mass_gather <- mass %>% 
  gather(Col, Mass, -Plate, -Row) %>% # gathering the data by creating a new column called Col
  arrange(Plate, Row) # arrange it by the plate number and the row number

mass_6P  <- mass_gather[rep(seq_len(nrow(mass_gather)), each = 9), ] 
# in order to macth with the 9 time points of the other dataset
# we have to repeat each line here for 9 times

write_csv(mass_6P, "data/tidydata/mass_6P_alternative.csv") # save the mass data 

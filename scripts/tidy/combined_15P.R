
library(tidyverse)

library(readxl)


#                            ** combine the OD with the design ** 


# read in the design

design <- read_csv("data/design/design_with_time.csv")

# add a new column called "Well" to distinguish each replicates 

design <-  design %>% 
  mutate(Well = paste(Plate, Row, Column, sep = "_")) %>% 
  select(Plate, Row, ColPair, Column, WellGroup, WellGroupType, Well, Sample, Time)
  # reorder the column names to make it more logical

filename1 <- "C:/Users/WAN333/Documents/Data school/starch-hydrolysis/data/tidydata/plate"

filename2 <- '.csv' 

plate <- c('02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '14', '15') 
# plate <- c(02:15) this is another alternative but have to deal the problem of not showing the 0

# read in the 1st spreadsheet and assign it to a new dataframe

data_15P <- read_csv(paste0(filename1, '01', filename2)) 


for (plt in plate) {
  filename_real <- paste0(filename1, plt, filename2)
  print(filename_real)
  temp_data <- read_csv(filename_real)
  data_15P <- bind_rows(data_15P, temp_data)
}

# below is an ememple of taking one element in the vector(set a value for the 'counter'), 
# and step through the loop, see what the loop is doing (called DEBUG)

# plt <- plate[1]
# filename_real <- paste0(filename1, plt, filename2)
# print(filename_real)
# temp_data <- read_csv(filename_real)
# data_first_six_plates <- bind_rows(data_plate01, temp_data)

# change the column names to be consistent with the design

data_15P <- data_15P %>% 
  rename(Plate = plate, Row = raw, Column = col, Time = time)

# check the missing values

data_15P %>% 
  filter(is.na(OD)) # 702 NAs in total, which is correct
# recall all the NAs for each plate: 18+18+36+18+306+36+18+18+36+18+36+144 = 702

# save the data of these 15 plates

write_csv(data_15P, "data/tidydata/data_15P.csv")

# join the data with the design
# order the columns

joined_15P <- full_join(data_15P, design) %>% 
  select(Plate, Row, ColPair, Column, WellGroup, WellGroupType, Well, Sample, Time, OD)

# save the joined data 

write_csv(joined_15P, "data/tidydata/joined_15P.csv")

############################################ add the mass into the previous joined dataset ########################################################

# read in the mass dataset (tidy version)

mass <- read_csv("data/tidydata/mass_15P.csv") 

# select just the Mass column to be merged in the next step

mass_selected <- select(mass, Mass)

total <-  bind_cols(joined_15P, mass_selected) %>% 
  select(Plate, Row, ColPair, Column, Sample, WellGroup, WellGroupType, Mass, Time, OD) # ordering the columns

# add a column to distnguish the sample and the blank

add_blk <-  total %>% 
  mutate(blank = Column %% 2 == 0) # %% is to check if there is something left over
# for ex, 5 %% 2 should give us 1 
# add a new col to distinguish the sample and the blank 
# so that we can use it to filter our dataset

# extract the samples

Sample <- add_blk %>% 
  filter(blank == FALSE) %>% # just keep the sample columns
  rename(OD_sample = OD, Mass_sample = Mass) %>% # rename the columns 
  select(-blank) # remove that blank column


Blank <- add_blk %>% 
  filter(blank == TRUE) %>% # just keep the blank columns
  rename(OD_blk = OD, Mass_blk = Mass) %>% # rename the columns
  select(Mass_blk, OD_blk) # remove that blank column

# bind the Sample and the Blank together by column

joined_15P_with_mass <- bind_cols(Sample, Blank)

# save the dataset

write_csv(joined_15P_with_mass, "data/tidydata/joined_15P_with_mass.csv")

########################################### add the slope into the previous dataset ##############################################################

# read in the slope data 

slope <- read_xlsx("C:/Users/WAN333/Documents/Thesis/Experiments/raw_data/slope_15P.xlsx")

# gather the data in order to do the calculation

slope_gathered <- slope %>% 
  gather(Day, Slope, -Plate)

# calculate the mean values of the slope by plate

mean_slope <- slope_gathered %>% 
  group_by(Plate) %>% 
  mutate(mean_slope = mean(Slope)) %>% 
  arrange(Plate) %>% 
  select(Plate, mean_slope) %>% 
  unique() # remove the duplicated rows

# join the slope with the previous dataset

joined_15P_with_mass_slope <- left_join(joined_15P_with_mass, mean_slope)

# save the joined dataset

write_csv(joined_15P_with_mass_slope, "data/tidydata/joined_15P_with_mass_slope.csv")

#################################### add the cav number into the previous dataset ###############################################################

# read in the design for the magic population

design_name <- read_csv("data/tidydata/previous_data/design_magic_pop.csv")

# join the previous dataset with id

joined_15P_with_mass_slope_id <- left_join(joined_15P_with_mass_slope, design_name) 

# reordering the column names

joined_15P_with_mass_slope_id <- select(joined_15P_with_mass_slope_id, Plate, Row, ColPair, Column, Sample, ID, WellGroup, WellGroupType, 
                        Mass_sample, Time, OD_sample, Mass_blk, OD_blk, mean_slope)
# ordering the cols

write_csv(joined_15P_with_mass_slope_id, "data/tidydata/joined_15P_with_mass_slope_id.csv")

#####################################################################################################





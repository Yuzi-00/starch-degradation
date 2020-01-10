#                                                      ** tidy the dataset **


library(tidyverse)

library(readxl)

# assigning the unchangeable part of the path

filename1 <- "C:/Users/WAN333/Documents/Thesis/Experiments/2020-01/20200107_DNS_Qin/raw_data/20200107_DNS_Qin_t"
filename2 <- 'min.xlsx'

# assigning the changeable part of the path

filetime <- c('20','60','120','180', '240', '360', '1440', '1800') 

# read the 1st spreadsheet et and assign it to a new dataframe

df <- read_excel(paste0(filename1,'0',filename2), range = "A15:E19") 

# add two columns: plate number and time point for the 1st spreadsheet

df <- mutate(df, Time = 0)

# creat a loop to import all the spreadsheet
# add two columns: plate number and time point from the 2nd spreadsheet on 
# bind them by raws

for (fltime in filetime) {
  filename_real <- paste0(filename1,fltime,filename2)
  print(filename_real)
  temp_data <- read_excel(filename_real, range = "A15:E19",na = 'NA')
  temp_data <- mutate(temp_data, Time = as.numeric(fltime))
  df <- bind_rows(df, temp_data)
}

# modify the default name for the first col & raw

df <- rename(df, Row = ...1) 

# creat a new column to count the replicates and remove the controls

df <- df %>% 
  mutate(Rep = case_when( 
    Row == "A" ~ "1", 
    Row == "B" ~ "2", 
    Row == "C" ~ "3",
    Row == "D" ~ "control",
  )) %>% 
  arrange(Row)

df

df <- gather(df, Col, OD, -Row, -Time, -Rep) 

df


# read in the tidied mass dataset

mass_rep <- read_csv("data/Qin/tidydata/mass_tidy.csv")

# select just the Mass column to be merged in the next step

mass_selected <- select(mass_rep, Mass)

mass_selected

df02 <- bind_cols(df, mass_selected) %>% 
  select(Row, Col, Rep, Mass, Time, OD) %>% 
  mutate(Col = as.numeric(Col))

df02

# import the layout

layout <- read_xlsx("C:/Users/WAN333/Documents/Thesis/Experiments/2020-01/20200107_DNS_Qin/raw_data/layout.xlsx", range = "A2:C18")

# add the layout to the previous dataset

df03 <- left_join(df02, layout) %>% 
  select(Sample, Row, Col, Rep, Mass, Time, OD)

df03

write_csv(df03, "data/Qin/tidydata/raw_dataset.csv")

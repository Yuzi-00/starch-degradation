library(tidyverse)

library(readxl)

# read in the raw dataset

RVA_design <- read_csv("data/rvaDesignAgNO3.csv") ## this is the design

RVA_results <- read_xlsx("data/RVA analysis.xlsx") ## this is the results

# tidy the design dataset (we need the Day, Seq and ID columns)

design_selected <- RVA_design %>% 
  select(Day, Seq, ID) ## just select these three columns that we need for combining the dataset

# tidy the results dataset (we need the DAYS, Seq, Peak 1, Trough 1, Final Visc and the Pasting Temp)

results <- RVA_results %>% 
  select(DAYS, Seq, 'Peak 1', 'Trough 1', 'Final Visc', 'Pasting Temp') %>% 
  rename(Day = DAYS, Peak = 'Peak 1', Trough = 'Trough 1', Final = 'Final Visc', PastingTemp = 'Pasting Temp') %>% ## change the columne name to be consistant with the other dataset
  filter(Day != "DAYS") %>% ## remove this column (this is the last column in the spreadsheet which is useless)
  mutate(Day = as.numeric(Day), Peak = as.numeric(Peak), Trough = as.numeric(Trough), Final = as.numeric(Final), 
         PastingTemp = as.numeric(PastingTemp)) ## change the chr to dbl


# combine the design and the result together

RVA_data <- left_join(results, design_selected)

# calculate the mean value 

RVA_tidy <- RVA_data %>% 
  group_by(ID) %>% 
  summarise(mean_Peak = mean(Peak), mean_Trough = mean(Trough), mean_Final = mean(Final), 
            mean_PastingTemp = mean(PastingTemp))
## after this step, the row number dropped from 735 to 508

# write out the tidy data frame

write_csv(RVA_tidy, "data/tidydata/previous_data/RVA_tidy.csv")


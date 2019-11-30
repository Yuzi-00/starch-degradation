library(tidyverse)

library(readxl)

design_magic <- read_xlsx("C:/Users/WAN333/Documents/Thesis/Thesis infomation/MAGIC population/Data_MAGIC Population/DEsign name starch_flour.xlsx")
# read in the design spreadsheet for the magic population

duplicated(design_magic) # looking for the repeated item in this dataset 
                        # (when the rows are exactly the same including the row number(/name of the row))

# alternative code:
# unique(design_magic[duplicated(design_magic),])
# (when the values are the same, but the row number(/name of the row) could be different)

rep_item <- design_magic[duplicated(design_magic),] # showing the repeated item

design_remove_rep <- unique(design_magic) # removing the repeated items

design_tidy <- design_remove_rep %>% 
  filter(id != "NA")
# 223 rows in total because 1 : there are no 81 and no 160
                          # 2 : 82 and 82* have different cav numbers

design_tidy_renamed <- design_tidy %>%
  rename(ID = "id", Sample = "Sample Name")

write_csv(design_tidy_renamed, "data/tidydata/previous_data/design_magic_pop.csv")  


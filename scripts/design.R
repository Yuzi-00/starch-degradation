library(tidyverse)

library(readxl)

# attention: some samples had been added into plate 15 in order to repeat, and so complete the design spreadsheet first by adding these repeated samples in

design <- read_xlsx("data/design/Design_Starch-digestability-assay_2009.xlsx")

# creat a vector that contains the nine time points

time <- c(0, 20, 60, 120, 180, 240, 360, 1440, 1800)

# repeats each raw 9 times

design_with_time <- design[rep(seq_len(nrow(design)), each = 9), ] %>% 

  mutate(Time = rep(time, times = 1440)) # add a column called "Time" and repeat 1440 times

write_csv(design_with_time, "data/design/design_with_time.csv")




  


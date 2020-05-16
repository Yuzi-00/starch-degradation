library(tidyverse)

library(readxl)

# read in the raw data

raw_data <- read_xls("data/magic_population/MastersizerMagic_bis.xls",
                     sheet = "Total data",
                     range = "A2:N515")


df <- raw_data[-1,] # remove the first empty row

master_size <- df %>% 
  select("Sample Name", "Specific surface area", "D [4, 3] - Volume weighted mean",
         "D [3, 2] - Surface weighted mean","d (0.1)", "d (0.5)", "d (0.9)", "B-granule",
         "A-granule", "A/B ratio") %>%  # select just these useful columns
  rename(Sample = "Sample Name", SSA = "Specific surface area", VWM = "D [4, 3] - Volume weighted mean", 
         SWM = "D [3, 2] - Surface weighted mean", d1 = "d (0.1)", d5 = "d (0.5)", d9 = "d (0.9)",
         B_granule = "B-granule", A_granule = "A-granule", AB_ratio = "A/B ratio") %>% 
  ## change the column name to be consistant with the other dataset
  group_by(Sample) %>% # group by sample
  summarise(SSA = mean(SSA), VWM = mean(VWM, na.rm = TRUE), SWM = mean(SWM, na.rm = TRUE), D1 = mean(d1, na.rm = TRUE),
            D5 = mean(d5, na.rm = TRUE), D9 = mean(d9, na.rm = TRUE), B_granule = mean(B_granule, na.rm = TRUE),
            A_granule = mean(A_granule, na.rm = TRUE), AB_ratio = mean(AB_ratio, na.rm = TRUE)) 
            ## calculate the average diameters
# 224 samples in total according to the tibble that has just been created, which is consistant with what we have

write_csv(master_size, "data/tidydata/master_size_bis.csv")







library(tidyverse)

library(readxl)

library(FactoMineR)

library(factoextra)

# import the dataset

df <- read_xls("data/MastersizerMagic_forPCA.xls", sheet = "Total data")

# remove the cav numbers

df1 <- df %>%
  select(-id) %>%
  rename(Sample = 'Sample Name') %>%
  rownames_to_column() %>%
  unite("sample", rowname:Sample) %>% # I did this because duplicated rownames are not allowed 
  column_to_rownames(var = 'sample') %>%
  select(-(1:28), -(91:103)) %>% # these colonms are all NAs or 0  
  na.omit() %>% 
  scale()

res.pca <- PCA(df1, scale.unit = TRUE)

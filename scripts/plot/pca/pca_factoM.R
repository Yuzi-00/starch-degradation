library(tidyverse)
library(readr)
library(FactoMineR)
library(factoextra)

# import the dataset

raw_df <- read_csv("data/tidydata/joined_6P_update.csv")

# tidy the dataset in order to do the pca

df <- raw_df %>% 
  filter(!is.na(Hydro_extent), !is.na(Amylose_content), !is.na(low_dp)) %>% # remove the missing value
  filter(Sample != "C+", Sample != "C-") %>% # remove the controls which don't belong to the magic population
  select(ID, Amylose_content, D1, D5, D9, h, k, Xinf, mean_Peak, mean_Trough, mean_Final, mean_PastingTemp,
         low_dp, medium_dp, medium_high_dp, high_dp) %>% 
  unique() # remove the same rows


df_pca <- PCA(df[, 6:8], graph = FALSE)
print(df_pca)

eg <- get_eigenvalue(df_pca)
eg

fviz_eig(df_pca, addlabels = TRUE, ylim = c(0, 50))

fviz_pca_var(df_pca, col.var = "black")

get_pca_ind(df_pca)

fviz_pca_ind(df_pca)

fviz_pca_biplot(df_pca)

# quality of the representation

var <- get_pca_var(df_pca)

head(var$cos2) ## the cos 2 values are used to estimate the quality of the representation

install.packages('corrplot')

library(corrplot)

corrplot(var$cos2, is.corr = FALSE)
##########################################################################################################################


df <- raw_df %>% 
  filter(!is.na(Hydro_extent), !is.na(Amylose_content)) %>% # remove the missing value
  filter(Sample != "C+", Sample != "C-") %>% # remove the controls which don't belong to the magic population
  select(Sample, Amylose_content, D1, D5, D9, h, k, Xinf, mean_Peak, mean_Trough, mean_Final, mean_PastingTemp) %>% 
  unique() %>% # remove the same rows
  column_to_rownames("Sample") # remove the column name

df_pca <- PCA(df[, -(2:7)], graph = FALSE)
print(df_pca)

eg <- get_eigenvalue(df_pca)
eg

fviz_eig(df_pca, addlabels = TRUE, ylim = c(0, 50))

fviz_pca_var(df_pca, col.var = "black")
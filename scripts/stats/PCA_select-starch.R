
library(tidyverse)

library(readxl)

library(FactoMineR)

library(factoextra)

# import the dataset

df <- read_csv("analysis/total_new_convert_replaced_by_granular.csv")

df1 <- df %>%
  select(Sample, pi_A, pi_B, pi_C) %>%
  unique() %>%
  column_to_rownames(var = 'Sample') %>%
  na.omit() %>% 
  scale()

res.pca <- PCA(df1, scale.unit = TRUE)

eig.val <- get_eigenvalue(res.pca)

eig.val

# plot PCA

fviz_pca_var(res.pca, col.var = "black") 

# graph of individuals 

ind <- get_pca_ind(res.pca)

ind

fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE # Avoid text overlapping (slow if many points)
)

# plot of biplot

fviz_pca_biplot(res.pca, repel = FALSE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)


library(tidyverse)

library(FactoMineR)

library(factoextra)

# import the RVA dataset 

df <- read_csv("data/tidydata/previous_data/RVA_tidy.csv")

# scale

df1 <- df  %>%
  remove_rownames() %>%
  column_to_rownames(var = 'ID') %>%
  scale()

# compute PCA

res.pca <- PCA(df1, 
               scale.unit = TRUE,
               graph = FALSE)

summary(res.pca)

# plot PCA

fviz_pca_var(res.pca, col.var = "black") 

# graph of individuals 

ind <- get_pca_ind(res.pca)

ind

fviz_pca_ind(res.pca, col.ind = "coord", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE # Avoid text overlapping (slow if many points)
)

# plot of biplot

fviz_pca_biplot(res.pca, repel = FALSE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "coord"  # Individuals color
)

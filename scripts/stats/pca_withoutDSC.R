
# install.packages("factoextra")

library(tidyverse)

library(FactoMineR)

library(factoextra)

############### pca structure ################

df <- read_csv("analysis/total_new.csv")

df_tidy <- df %>%
  select(Amylose_content, SSA, Surface_weighted_mean, D1, D5, D9, low_dp, medium_dp, medium_high_dp,
         high_dp) %>%
  unique() %>% 
  na.omit()

# compute PCA

res.pca <- prcomp(df_tidy, scale = TRUE)

# visualize eigenvalues (scree plot)

fviz_eig(res.pca)

## it shows the percentage of variances explained by each principal component

fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE     # Avoid text overlapping
)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca, repel = FALSE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

# select all the structural and kinetic data 

df_02 <- df %>%
  select(Amylose_content, SSA, Surface_weighted_mean, D1, D5, D9, low_dp, medium_dp, medium_high_dp,
         high_dp, h, k, Xinf) %>%
  unique() %>% 
  na.omit()

## plot the pca and add the kinetcis as supplimantary variables  

res.pca = PCA(df_02, scale.unit=TRUE, ncp=5, quanti.sup=c(11:13), graph=T) 

############### pca kinetics ################

# select the kinetics data

df_kin <- df %>%
  select(h, k, Xinf) %>%
  unique() %>%
  na.omit()

# compute PCA

res.pca <- prcomp(df_kin, scale = TRUE)

# visualize eigenvalues (scree plot)

fviz_eig(res.pca)

## it shows the percentage of variances explained by each principal component

fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE     # Avoid text overlapping
)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca, repel = FALSE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)






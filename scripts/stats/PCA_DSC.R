
library(tidyverse)

library(FactoMineR)

library(factoextra)

# import the dataset

df <- read_csv("analysis/total_new_convert_DSC.csv") %>%
  na.omit()

# add the residuals of the h k model

res <- read_csv("analysis//hkmodel_residuals.csv")

df <- left_join(df, res) %>%
  rename(residual = .resid)

# calculate the mean value of the kinetics 

df1 <- df %>%
  select(11, 34:36, 47) %>%
  group_by(Sample) %>%
  summarise(k = mean(k), h = mean(h), Xinf = mean(Xinf), residual = mean(residual)) 

# select the structural properties (all, including the parameters that we are going to put as supplimentary variables)

df2 <- df %>%
  select(1:11) %>% 
  unique() 

# combine and scale

df_new <- full_join(df2, df1) %>%
  remove_rownames() %>%
  column_to_rownames(var = 'Sample') %>%
  scale()

# compute PCA

res.pca <- PCA(df_new, 
               scale.unit = TRUE,
               # quanti.sup = 11:14, # the kinetics were not very well represented
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

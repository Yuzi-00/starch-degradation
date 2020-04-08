
# install.packages("factoextra")

library(tidyverse)

library(FactoMineR)

library(factoextra)

# importe the dataset

df <- read_csv("analysis/total_new_convert.csv") %>% 
  filter(Sample != "C+" & Sample != "C-") %>%
  na.omit() 

# calculate the mean value of the kinetics 

df1 <- df %>%
  select(2, 29:31) %>%
  group_by(Sample) %>%
  summarise(k = mean(k), h = mean(h), Xinf = mean(Xinf)) 

# select the structural properties

df2 <- df %>%
  select(2, 5:10, 15:18) %>% # remove the RVA and amylase results as they were not 
                             # represented well in the PCA
  unique()

# combine and scale

df_new <- full_join(df2, df1) %>%
  select(-Sample) %>%
  scale() %>%
  as_tibble()

# compute PCA

res.pca <- prcomp(df_new[1:10]) 

summary(res.pca)

# visualize eigenvalues (scree plot)

fviz_eig(res.pca) # the percentage of variances explained 
                  # by each principal component

fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE     # Avoid text overlapping
)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)

fviz_pca_biplot(res.pca, repel = FALSE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

# add supplimentary variable
  
quanti.coord <- cor(df_new[11:13], res.pca$x)

quanti.cos2 <- quanti.coord^2

p <- fviz_pca_var(res.pca)

fviz_add(p, quanti.coord, color ="blue", geom="arrow")



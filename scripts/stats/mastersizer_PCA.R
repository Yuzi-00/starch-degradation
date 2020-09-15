
library(tidyverse)

library(readxl)

library(FactoMineR)

library(factoextra)

########################## 

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

eig.val <- get_eigenvalue(res.pca)

eig.val

# plot PCA

fviz_pca_var(res.pca, col.var = "black") # we can see that there are three groups

var <- get_pca_var(res.pca)

var

head(var$coord)

# Create a grouping variable using kmeans
# Create 3 groups of variables (centers = 3)

res.km <- kmeans(var$coord, centers = 3, nstart = 25)

grp <- as.factor(res.km$cluster)
# Color variables by groups
fviz_pca_var(res.pca, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")

# graph of individuals 

ind <- get_pca_ind(res.pca)

ind

fviz_pca_ind(res.pca, col.ind = "coord", # color by coordinant
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE # Avoid text overlapping (slow if many points)
)

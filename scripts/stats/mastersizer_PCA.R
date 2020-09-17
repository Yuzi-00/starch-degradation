
library(tidyverse)

library(readxl)

library(FactoMineR)

library(factoextra)

########################## 

# import the dataset and calculate the mean values (the MasterSizer was on replicates)

df <- read_xls("data/MastersizerMagic_forPCA.xls", sheet = "Total data") %>%
  select(-(70:103)) %>% # remove the aggregations (diameter > 100um)
  select(-id, -"0.01") %>% # remove the colomn of 0.01 as its all NAs 
  rename(sample = "Sample Name") %>%
  group_by(sample) %>%
  summarise_all(mean)

# transpose the dataset 

dft <- as.data.frame(t(df))
  
# set the first row as colomn names 

names(dft) <- as.matrix(dft[1, ])
dft <- dft[-1, ]
dft[] <- lapply(dft, function(x) type.convert(as.character(x)))
dft <- dft %>%
  as.data.frame()
  


# df1 <- df %>%
#   select(-id) %>%
#   rename(Sample = 'Sample Name') %>%
#   filter(Sample != 1 & Sample != 3 & Sample != 7) %>%
#   rownames_to_column() %>%
#   unite("sample", rowname:Sample) %>% # I did this because duplicated rownames are not allowed 
#   column_to_rownames(var = 'sample') %>%
#   select(-(1:28)) %>% # these colonms are all NAs or 0  
#   na.omit() %>% 
#   scale()


# PCA

res.pca <- PCA(dft, scale.unit = TRUE)

eig.val <- get_eigenvalue(res.pca)

eig.val

# plot PCA

fviz_pca_var(res.pca, col.var = "black") # we can see that there are three groups

var <- get_pca_var(res.pca)

var

head(var$coord)

# Create a grouping variable using kmeans
# Create 3 groups of variables (centers = 3)

res.km <- kmeans(var$coord, centers = 2)

grp <- as.factor(res.km$cluster)
# Color variables by groups
fviz_pca_var(res.pca, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")

# graph of individuals 

ind <- get_pca_ind(res.pca)

ind

res.km <- kmeans(ind$coord, centers = 4)

grp <- as.factor(res.km$cluster)

fviz_pca_ind(res.pca, col.ind = grp, # color by coordinant
             repel = FALSE # Avoid text overlapping 
)

fviz_pca_ind(res.pca,
             repel = TRUE # Avoid text overlapping
)

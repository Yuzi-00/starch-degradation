
library(tidyverse)

library(readxl)

library(FactoMineR)

library(factoextra)

############################ pca with all the factors except for amylase activity and RVAs ############################

# import the dataset

df <- read_csv("analysis/total_new_convert_replaced_by_granular.csv") 

df0 <- df %>%
  filter(Sample != "C+" & Sample != "C-") %>%
  na.omit() %>%
  select(-(HE_0min:Xinf), -(Peak_Vis:Pasting_Temp), -Amylase_Act, -Well, -ID, -Category) %>%
  unique()

# # add the residuals of the h k model
# 
# res <- read_csv("analysis//hkmodel_residuals.csv")
# 
# df <- left_join(df, res) %>%
#   rename(residual = .resid)

# calculate the mean value of the kinetics 

df1 <- df %>%
  select(2, 24:26) %>%
  group_by(Sample) %>%
  summarise(k = mean(k), h = mean(h), Xinf = mean(Xinf)) %>%
  na.omit() 

# combine and scale

df_new <- left_join(df0, df1) %>%
  na.omit() %>%
  column_to_rownames(var = 'Sample') %>%
  scale()

df1 <- df %>%
  select(Sample, pi_A, pi_B, pi_C, k, h, Xinf) %>%
  unique() %>%
  column_to_rownames(var = 'Sample') %>%
  na.omit() %>% 
  scale()

# PCA

res.pca <- PCA(df_new, 
               scale.unit = TRUE,
               quanti.sup = 16:18, # kinetics (h, k, Xinf) as illustrative factors 
               graph = FALSE) 

eig.val <- get_eigenvalue(res.pca)

eig.val

# plot PCA

fviz_pca_var(res.pca, col.var = "black") 
# we can see that the amylose content is not very well represented

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

############################## remove the amylose content in the PCA ############################

df_new1 <- left_join(df0, df1) %>%
  select(-Amylose_Con) %>%
  na.omit() %>%
  column_to_rownames(var = 'Sample') %>%
  scale()

# redo the PCA using dataset without amylose content 

res.pca <- PCA(df_new1, 
               scale.unit = TRUE,
               quanti.sup = 15:17, # kinetics (h, k, Xinf) as illustrative factors 
               graph = FALSE) 

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


# color by groups (by kinetics)

############################ pca on pi_A, pi_B, pi_C, with kinetics as illustratif variable ##############################


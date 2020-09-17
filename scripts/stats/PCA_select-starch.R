
library(tidyverse)

library(readxl)

library(FactoMineR)

library(factoextra)

############################ pca with all the factors except for amylase activity and RVAs ############################

# import the dataset

df <- read_csv("analysis/total_new_convert_replaced_by_granular.csv") 

# transpose the dataframe

dft <- transpose(df) %>%
  as.data.frame()

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

# scree plot

fviz_eig(res.pca, addlabels = TRUE)

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
               ncp = 7, # choose 7 as about 90% of the variability is represented by 7 dimensions 
               scale.unit = TRUE,
               quanti.sup = 15:17, # kinetics (h, k, Xinf) as illustrative factors 
               graph = FALSE) 

# plot PCA

fviz_pca_var(res.pca, col.var = "black") 

var <- get_pca_var(res.pca)

var

# Create a grouping variable using kmeans
# Create 3 groups of variables (centers = 3)

res.km <- kmeans(var$coord, centers = 4)

grp <- as.factor(res.km$cluster)
# Color variables by groups
fviz_pca_var(res.pca, col.var = grp, 
             legend.title = "Cluster")

# graph of individuals 

ind <- get_pca_ind(res.pca)

ind

res.km <- kmeans(ind$coord, centers = 5)

grp <- as.factor(res.km$cluster)

fviz_pca_ind(res.pca, col.ind = grp, 
             repel = FALSE # Avoid text overlapping (slow if many points)
)

# plot of biplot

fviz_pca_biplot(res.pca, repel = FALSE,
                col.var = "black", # Variables color
                col.ind = grp  # Individuals color
)

# get the 5 clusters

cluster <- res.km$cluster %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  rename(sample = "rowname", cluster = ".")

write.csv(cluster, "analysis/cluster_pca_selection.csv")

######################## find the samples that have the effect of h k Xinf #########################

# import the dataset of samples that have h k Xinf or residual significance

library(readxl)

df <- read_xlsx("data/amidon_avec_effet.xlsx") %>%
  select(-"Table 1")

# set the first row as colomn names 

names(df) <- as.matrix(df[1, ])
df <- df[-1, ]
df[] <- lapply(df, function(x) type.convert(as.character(x)))
df <- df %>%
  as.data.frame()
# we have 252 observations for now

# remove the repeated lines (samples)

dfr <- df %>%
  unique() %>%
  mutate(sample = as.character(sample))
# we now reduced the dataset into 163 observations

# let's find where these samples are positioned in the clusters of our PCA plot

dft <- left_join(dfr, cluster)
# we keep those 163 observations by this "left_join" 

########################## randomly choose 10 samples for each cluster #########################

# extract samples of cluster 1

c1 <- dft %>%
  filter(cluster == 1) %>%
  select(sample) %>%
  as.matrix() 
# we have 44 samples in total

# randomly choose 10 samples 

sample(c1, 10)
# we get "164" "46"  "117" "127" "56"  "123" "15"  "62"  "48"  "128"

# extract samples of cluster 2

c2 <- dft %>%
  filter(cluster == 2) %>%
  select(sample) %>%
  as.matrix() 
# we have 39 samples in total

# randomly choose 10 samples 

sample(c2, 10)
# we get "110" "102" "175" "5"   "18"  "93"  "4"   "221" "141" "59" 

# extract samples of cluster 3

c3 <- dft %>%
  filter(cluster == 3) %>%
  select(sample) %>%
  as.matrix() 
# we have 15 samples in total

# randomly choose 10 samples 

sample(c3, 10)
# we get "38"  "213" "223" "130" "143" "224" "214" "94"  "215" "72" 

# extract samples of cluster 4

c4 <- dft %>%
  filter(cluster == 4) %>%
  select(sample) %>%
  as.matrix() 
# we have 35 samples in total

# randomly choose 10 samples 

sample(c4, 10)
# we get "217" "2"   "77"  "187" "206" "144" "188" "139" "78"  "168"

# extract samples of cluster 5

c5 <- dft %>%
  filter(cluster == 5) %>%
  select(sample) %>%
  as.matrix() 
# we have 17 samples in total

# randomly choose 10 samples 

sample(c5, 10)
# we get "158" "20"  "203" "90"  "104" "32"  "98"  "176" "197" "154" 

library(tidyverse)

#################### using ggbiplot #####################

# install.packages("devtools")

library(devtools)

# install_github("vqv/ggbiplot")

library(ggbiplot)

# import the dataset

df <- read_csv("analysis/total_new.csv")

# select the variables, if there are non-numeric variabls, remove them too as 
# PCA works best with numerical data 

df_tidy <- df %>%
  select(23:44) %>%
  unique() %>% 
  na.omit() %>% # remove rows that contain NAs
  scale

df_tidy_pca <- prcomp(df_tidy[, ], center = TRUE, scale. = TRUE)

# take a look at the PCA object using summary()

summary(df_tidy_pca) 

## there are 22 principal components, which we call PC1-22. Each of these explains 
## a percentage of the total variation in the dataset.

# take a look at the PCA object using str()

str(df_tidy_pca) 

## here, we can see that the PCA object contains the standard deviation(sdev), 
## center point ($center), scaling ($scale) of each principal component; 
## the relationship (correlation or anticorrelation, etc) between the initial 
## variables and the principal components ($rotation); the values of each sample 
## in terms of the principal components ($x)

# plot PC1 and 2

ggbiplot(df_tidy_pca)

## this biplot includes both the position of each sample in terms of PC1 and PC2 
## and also shows us how the initial variables map onto this. Therefore, it allows 
## us to visualize how the samples relate to one another in our PCA (which samples 
## are similar and which are different) and simultaneously reveals how each variable
## contributes to each principal component.

# plot other PCs

ggbiplot(df_tidy_pca, choices=c(2,4)) # taken PC2 and PC4 as an example 

# we can also scale the sample (what for?) by using these two arguments below

ggbiplot(df_tidy_pca, obs.scale = 1, var.scale = 1)

# PCA of h k and Xinf

# select the related columns

df_sel <- df %>% 
  select(Well, h, k, Xinf, Amylose_content, SSA) %>% 
  na.omit() %>%
  unique() %>% # remove the same rows
  column_to_rownames("Well") 

# extract the amylose column for later uses

df_amy <- df_sel %>%
  select(Amylose_content) %>%
  arrange(Amylose_content) %>% 
  mutate(status = case_when( 
    Amylose_content < 28 ~ "low_amy", 
    Amylose_content >= 28 ~ "high_amy"
  )) 

# extract the SSA column for later uses

df_SSA <- df_sel %>%
  select(SSA) %>%
  arrange(SSA) %>% 
  mutate(status = case_when( 
    SSA < 1 ~ "low_SSA", 
    SSA >= 1 ~ "high_SSA"
  )) 

# scale the rest of the data

df_kin <- df_sel %>% 
  select(h, k, Xinf) %>%
  na.omit() %>%
  unique() %>% # remove the same rows
  scale
  
# compute PCA

df_kin_pca <- prcomp(df_kin[, ], center = TRUE, scale. = TRUE)

# add the amylose data into the pca

pca_amy <- df_kin_pca$x %>% 
  as_tibble() %>% 
  mutate(status = df_amy$status) 

# add the SSA data into the pca

pca_SSA <- df_kin_pca$x %>% 
  as_tibble() %>% 
  mutate(status = df_SSA$status) 

# plot

pca_SSA %>% 
  ggplot(aes(x = PC1, y = PC2,
             color = status)) +
  geom_point() +
  theme(legend.position = "none")

## neither amylose content nor SSA could classify the kinatic data into distinct groups  

#################### using factoextra #####################

# install.packages("factoextra")

library(factoextra)

# compute PCA

res.pca <- prcomp(df_tidy, scale = TRUE)

# visualize eigenvalues (scree plot)

fviz_eig(res.pca)

## it shows the percentage of variances explained by each principal component






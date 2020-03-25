
library(tidyverse)

# install.packages("clValid")

# install.packages("cluster")

# install.packages('dendextend', dependencies = TRUE) 

library(dendextend)

library(clValid)

# import the dataset 

total_new <- read_csv("analysis/total_new.csv")

# select the columns 

subdata <- total_new %>% 
  select(-(1:8), -(10:22), -(40:43))

# remove the NAs

subdata_red <- subdata %>% 
  unique() %>% 
  na.omit()

# remove the Sample column

subdata_num <- subdata_red %>%
  select(-ID)

# normalization

subdata_nor <- scale(subdata_num)

## notice the mean of all the columns is 0 and the standard deviation is 1 
## after scaling.

# build the distance matrix  

distance <- dist(subdata_nor, method = 'euclidean')

## here we use the euclidean distance method as all the values are continuous
## numerical values.

# dentrogram

hc <- hclust(distance, method = 'average')

## at this point, we should decide which linkage method we want to use for
## the hierarchical clustering. We can try all kinds of linkage methods and 
## later decide on which one performed better. Here we proceed with average 
## linkage method.

# plot

plot(hc)

## now we can cut the dendrogram in order to create the desired number of 
## clusters. Here probably three groups as can be seen from the dendrogram

# cut the tree and put them into different clusters 

rect.hclust(hc, k = 2, border = 2:6)

abline(h = 10, col = 'red', lty = 'dashed')

# another way to color the dendrogram

hc_dend <- as.dendrogram(hc)

hc_col_dend <- color_branches(hc_dend, h = 10)

plot(hc_col_dend)

# calculate how many observations were assigned in each cluster

cut_hc <- cutree(hc, k = 2)

subdata_cl <- mutate(subdata_num, cluster = cut_hc)

count(subdata_cl,cluster) # 608 vs 6

## is it a good clustering as one of the group is too small ? 

# evaluate the trend between two features based on the clustering

ggplot(subdata_cl, aes(x = SSA, y = k, color = factor(cluster))) + geom_point()

## did not find any trend yet based on the clustering 

# calculte the dunn's index

dunn(distance, cut_hc) # 0.603834

## Dunn's index is the ratio between the minimum inter-cluster distances to the maximum 
## intra-cluster diameter. The diameter of a cluster is the distance between its two furthermost 
## points. We should aim for higher dunn's index in order to have well separated and compact clusters

# double check the clustering results

results <- kmeans(subdata_nor, 2) # choose the number of groups

table(subdata_red$ID, results$cluster)



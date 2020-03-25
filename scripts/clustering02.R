
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

# exchange columns and rows

subdata_transpose = t(subdata_num)

# normalization

subdata_nor <- scale(subdata_transpose)

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


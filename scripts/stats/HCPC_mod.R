library(tidyverse)

library(FactoMineR)

library(factoextra)

# library(plyr)
# 
# library(purrr)

df <- read_csv("analysis/total_new_convert.csv")

# select the variables

df <- df %>%
  filter(Sample != "C+" & Sample != "C-") %>%
  na.omit()

# calculate the mean value of the kinetics 

df1 <- df %>%
  select(2, 29:31) %>%
  group_by(Sample) %>%
  summarise(k = mean(k), h = mean(h), Xinf = mean(Xinf)) 

# select the structural properties

df2 <- df %>%
  select(2, 5:19) %>% 
  unique() 

# combine and scale

df_new <- full_join(df2, df1) %>%
  select(Sample, Amylose_Con, SSA, SWM, D0.1, D0.5, D0.9, DP6_12, DP13_24, DP25_36, DP37_47, Amylase_Act, Peak_Vis, Trough_Vis,
         Final_Vis, Pasting_Temp, k, h, Xinf) %>%
  remove_rownames() %>%
  column_to_rownames(var = 'Sample') %>%
  scale()

#### ncp=6 #### 

# compute PCA

res.pca <- PCA(df_new, 
               ncp = 6,
               scale.unit = TRUE, 
               quanti.sup = 12:18, # add RVA and kinetics as supplementary data (illustrative data)
               graph = FALSE)

summary(res.pca)

# scree plot

fviz_eig(res.pca)

# HCPC 

# Compute hierarchical clustering on principal components

# 2 clusters

res.hcpc2 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 2)

hcpc2 <- res.hcpc2$desc.var$quanti # contribution of factors  

# convert the list into a dataframe
  
hcpc2 <- do.call(rbind, lapply(hcpc2, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-")

# save the results

write_csv(hcpc2, "analysis/ncp6/hcpc2.csv")
    

# 3 clusters

res.hcpc3 <- HCPC(res.pca, graph = TRUE, 
                 consol = TRUE,
                 nb.clust = 3)

hcpc3 <- res.hcpc3$desc.var$quanti# contribution of factors  

# convert the list into a dataframe

hcpc3 <- do.call(rbind, lapply(hcpc3, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-")

# save the results

write_csv(hcpc3, "analysis/ncp6/hcpc3.csv")

# 4 clusters

res.hcpc4 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 4)

hcpc4 <- res.hcpc4$desc.var$quanti # contribution of factors 

# convert the list into a dataframe

hcpc4 <- do.call(rbind, lapply(hcpc4, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-")

hcpc4[is.na(hcpc4)] = "D0.9"

# save the results

write_csv(hcpc4, "analysis/ncp6/hcpc4.csv")

# 5 clusters

res.hcpc5 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 5)

hcpc5 <- res.hcpc5$desc.var$quanti # contribution of factors  

# convert the list into a dataframe

hcpc5 <- do.call(rbind, lapply(hcpc5, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

hcpc5[is.na(hcpc5)] = "D0.9"

# save the results

write_csv(hcpc5, "analysis/ncp6/hcpc5.csv")

# 6 clusters

res.hcpc6 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 6)

hcpc6 <- res.hcpc6$desc.var$quanti # contribution of factors 

# convert the list into a dataframe

hcpc6 <- do.call(rbind, lapply(hcpc6, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

hcpc6[is.na(hcpc6)] = "D0.9"

# save the results

write_csv(hcpc6, "analysis/ncp6/hcpc6.csv")

# 7 clusters

res.hcpc7 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 7)

hcpc7 <- res.hcpc7$desc.var$quanti # contribution of factors 

# convert the list into a dataframe

hcpc7 <- do.call(rbind, lapply(hcpc7, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# replace the NAs 

hcpc7[is.na(hcpc7)] = c("Amylase_Act", "D0.9") 

# save the results

write_csv(hcpc7, "analysis/ncp6/hcpc7.csv")

# 8 clusters

res.hcpc8 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 8)

hcpc8 <- res.hcpc8$desc.var$quanti # contribution of factors 

# convert the list into a dataframe

hcpc8 <- do.call(rbind, lapply(hcpc8, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# replace the NAs 

hcpc8[is.na(hcpc8)] = c("Amylase_Act", "D0.9") 

# save the results

write_csv(hcpc8, "analysis/ncp6/hcpc8.csv")

# 9 clusters

res.hcpc9 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 9)

hcpc9 <- res.hcpc9$desc.var$quanti # contribution of factors 

# convert the list into a dataframe

hcpc9 <- do.call(rbind, lapply(hcpc9, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# replace the NAs 

hcpc9[is.na(hcpc9)] = c("Amylase_Act", "D0.9") 

# save the results

write_csv(hcpc9, "analysis/ncp6/hcpc9.csv")

# 10 clusters

res.hcpc10 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 10)

hcpc10 <- res.hcpc10$desc.var$quanti # contribution of factors 

hcpc10 <- do.call(rbind, lapply(hcpc10, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# replace the NAs 

hcpc10[is.na(hcpc10)] = c("Amylase_Act", "D0.9") 

# save the results

write_csv(hcpc10, "analysis/ncp6/hcpc10.csv")

# 11 clusters

res.hcpc11 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 11)

hcpc11 <- res.hcpc11$desc.var$quanti # contribution of factors 

hcpc11 <- do.call(rbind, lapply(hcpc11, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# replace the NAs 

hcpc11[is.na(hcpc11)] = c("Amylase_Act", "D0.9") 

# save the results

write_csv(hcpc11, "analysis/ncp6/hcpc11.csv")

# 12 clusters

res.hcpc12 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 12)

hcpc12 <- res.hcpc12$desc.var$quanti # contribution of factors 

hcpc12 <- do.call(rbind, lapply(hcpc12, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# replace the NAs 

hcpc12[is.na(hcpc12)] = c("Amylase_Act", "D0.9") 

# save the results

write_csv(hcpc12, "analysis/ncp6/hcpc12.csv")

# 13 clusters

res.hcpc13 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 13)

hcpc13 <- res.hcpc13$desc.var$quanti # contribution of factors 

hcpc13 <- do.call(rbind, lapply(hcpc13, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# replace the NAs 

hcpc13[is.na(hcpc13)] = c("Amylase_Act", "D0.9") 

# save the results

write_csv(hcpc13, "analysis/ncp6/hcpc13.csv")

# wait for modification

# visualize the dendrogram generated by the hierarchical clustering

fviz_dend(res.hcpc3, 
          cex = 0.7, # label size
          palette = "jco",
          rect = TRUE, rect_fill = TRUE,
          rect_border = "jco",         
          labels_track_height = 0.8)

## the dentrogram suggest 3 clusters solution

# visualize individuals on the principal component map 
# and to color individuals according to the cluster they belong to (factorial map)

fviz_cluster(res.hcpc3,
             repel = FALSE, # avoid label overlapping
             show.clust.cent = TRUE, # show cluster centers
             palette = "jco", 
             ggtheme = theme_minimal(),
             main = "Factor map"
)

# display the original data with cluster assignments

head(res.hcpc3$data.clust, 10) # 3 clusters as an example

## the last column contains the cluster assignments

# save the results of the HCPC into a dataframe

r_hcpc2_s <- res.hcpc2$data.clust %>% # 2 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster2 = clust) 

r_hcpc2 <- r_hcpc2_s %>%
  select(Sample, cluster2)

r_hcpc3_s <- res.hcpc3$data.clust %>% # 3 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster3 = clust) 

r_hcpc3 <- r_hcpc3_s %>%
  select(Sample, cluster3)

r_hcpc4_s <- res.hcpc4$data.clust %>% # 4 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster4 = clust)

r_hcpc4 <- r_hcpc4_s %>%
  select(Sample, cluster4)

r_hcpc5_s <- res.hcpc5$data.clust %>% # 4 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster5 = clust)

r_hcpc5 <- r_hcpc5_s %>%
  select(Sample, cluster5)

r_hcpc6_s <- res.hcpc6$data.clust %>% # 4 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster6 = clust)

r_hcpc6 <- r_hcpc6_s %>%
  select(Sample, cluster6)

r_hcpc7_s <- res.hcpc7$data.clust %>% # 4 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster7 = clust) 

r_hcpc7 <- r_hcpc7_s %>%
  select(Sample, cluster7)

r_hcpc8_s <- res.hcpc8$data.clust %>% # 8 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster8 = clust) 

r_hcpc8 <- r_hcpc8_s %>%
  select(Sample, cluster8)

r_hcpc9_s <- res.hcpc9$data.clust %>% # 9 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster9 = clust) 

r_hcpc9 <- r_hcpc9_s %>%
  select(Sample, cluster9)

r_hcpc10_s <- res.hcpc10$data.clust %>% # 4 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster10 = clust) 

r_hcpc10 <- r_hcpc10_s %>%
  select(Sample, cluster10)

r_hcpc11_s <- res.hcpc11$data.clust %>% # 4 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster11 = clust) 

r_hcpc11 <- r_hcpc11_s %>%
  select(Sample, cluster11)

r_hcpc12_s <- res.hcpc12$data.clust %>% # 4 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster12 = clust) 

r_hcpc12 <- r_hcpc12_s %>%
  select(Sample, cluster12)

# combine all the cluster results together to one single dataframe (unscaled values)

df_0 <- full_join(df2, df1) 

r_hcpc_inter <- left_join(df_0, r_hcpc2)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc3) 

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc4) 

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc5)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc6)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc7)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc8) 

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc9) 

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc10)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc11) 

r_hcpc_all <- left_join(r_hcpc_inter, r_hcpc12)

# save the final dataset

write_csv(r_hcpc_all, "analysis/hcpc_ncp6.csv")

# combine all the cluster results together to one single dataframe (scaled values)

r_hcpc_s_inter <- left_join(r_hcpc2_s, r_hcpc3)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc4)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc5)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc6)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc7)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc8)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc9)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc10)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc11)

r_hcpc_s_all <- left_join(r_hcpc_s_inter, r_hcpc12)

# save the final dataset

write_csv(r_hcpc_all, "analysis/hcpc_scaled_ncp6.csv")

# calculate the mean value of each factor #

#### 2 clusters ####

# original values

mean_2c <- r_hcpc_all %>%
  group_by(cluster2) %>% # by 2 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_2c_m <- mean_2c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster2) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_2c_s <- mean_2c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster2) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_2c_full <- full_join(mean_2c_m, mean_2c_s)

# histogram (original values)

ncp6_c2 <- mean_2c_full %>%
  ggplot(aes(x = cluster2, y = mean)) +
  geom_errorbar(aes(x = cluster2,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp6/ncp6_c2_ori.png", 
       plot = ncp6_c2, 
       width = 30, 
       height = 20,  
       units = "cm") 

# scaled values

mean_2c <- r_hcpc_s_all %>%
  group_by(cluster2) %>% # by 2 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_2c_m <- mean_2c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster2) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_2c_s <- mean_2c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster2) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_2c_full <- full_join(mean_2c_m, mean_2c_s)

# histogram (scaled values)

ncp6_c2 <- mean_2c_full %>%
  ggplot(aes(x = cluster2, y = mean)) +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp6/ncp6_c2_scal.png", 
       plot = ncp6_c2, 
       width = 30, 
       height = 20,  
       units = "cm") 

#### 3 clusters ####

# original values

mean_3c <- r_hcpc_all %>%
  group_by(cluster3) %>% # by 3 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_3c_m <- mean_3c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster3) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_3c_s <- mean_3c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster3) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_3c_full <- full_join(mean_3c_m, mean_3c_s)

# histogram (original values)

ncp6_c3 <- mean_3c_full %>%
  ggplot(aes(x = cluster3, y = mean)) +
  geom_errorbar(aes(x = cluster3,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp6/ncp6_c3_ori.png", 
       plot = ncp6_c3, 
       width = 30, 
       height = 20,  
       units = "cm") 

# scaled values

mean_3c <- r_hcpc_s_all %>%
  group_by(cluster3) %>% # by 3 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_3c_m <- mean_3c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster3) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_3c_s <- mean_3c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster3) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_3c_full <- full_join(mean_3c_m, mean_3c_s)

# histogram (scaled values)

ncp6_c3 <- mean_3c_full %>%
  ggplot(aes(x = cluster3, y = mean)) +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp6/ncp6_c3_scal.png", 
       plot = ncp6_c3, 
       width = 30, 
       height = 20,  
       units = "cm") 


#### 4 clusters ####

# original values

mean_4c <- r_hcpc_all %>%
  group_by(cluster4) %>% # by 4 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_4c_m <- mean_4c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster4) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_4c_s <- mean_4c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster4) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_4c_full <- full_join(mean_4c_m, mean_4c_s)

# histogram (original values)

ncp6_c4 <- mean_4c_full %>%
  ggplot(aes(x = cluster4, y = mean)) +
  geom_errorbar(aes(x = cluster4,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp6/ncp6_c4_ori.png", 
       plot = ncp6_c4, 
       width = 30, 
       height = 20, 
       units = "cm") 

# scaled values

mean_4c <- r_hcpc_s_all %>%
  group_by(cluster4) %>% # by 4 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_4c_m <- mean_4c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster4) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_4c_s <- mean_4c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster4) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_4c_full <- full_join(mean_4c_m, mean_4c_s)

# histogram (scaled values)

ncp6_c4 <- mean_4c_full %>%
  ggplot(aes(x = cluster4, y = mean)) +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp6/ncp6_c4_scal.png", 
       plot = ncp6_c4, 
       width = 30, 
       height = 20, 
       units = "cm") 

#### 5 clusters ####

# original values

mean_5c <- r_hcpc_all %>%
  group_by(cluster5) %>% # by 5 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_5c_m <- mean_5c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster5) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_5c_s <- mean_5c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster5) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_5c_full <- full_join(mean_5c_m, mean_5c_s)

# histogram (original values)

ncp6_c5 <- mean_5c_full %>%
  ggplot(aes(x = cluster5, y = mean)) +
  geom_errorbar(aes(x = cluster5,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp6/ncp6_c5_ori.png", 
       plot = ncp6_c5, 
       width = 30, 
       height = 20, 
       units = "cm") 

# scaled values

mean_5c <- r_hcpc_s_all %>%
  group_by(cluster5) %>% # by 5 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_5c_m <- mean_5c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster5) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_5c_s <- mean_5c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster5) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_5c_full <- full_join(mean_5c_m, mean_5c_s)

# histogram (scaled values)

ncp6_c5 <- mean_5c_full %>%
  ggplot(aes(x = cluster5, y = mean)) +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp6/ncp6_c5_scal.png", 
       plot = ncp6_c5, 
       width = 30, 
       height = 20,  
       units = "cm") 

#### 6 clusters ####

# original values

mean_6c <- r_hcpc_all %>%
  group_by(cluster6) %>% # by 6 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_6c_m <- mean_6c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster6) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_6c_s <- mean_6c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster6) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_6c_full <- full_join(mean_6c_m, mean_6c_s)

# histogram (original values)

ncp6_c6 <- mean_6c_full %>%
  ggplot(aes(x = cluster6, y = mean)) +
  geom_errorbar(aes(x = cluster6,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp6/ncp6_c6_ori.png", 
       plot = ncp6_c6, 
       width = 30, 
       height = 20, 
       units = "cm") 

# scaled values

mean_6c <- r_hcpc_s_all %>%
  group_by(cluster6) %>% # by 6 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_6c_m <- mean_6c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster6) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_6c_s <- mean_6c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster6) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_6c_full <- full_join(mean_6c_m, mean_6c_s)

# histogram (scaled values)

ncp6_c6 <- mean_6c_full %>%
  ggplot(aes(x = cluster6, y = mean)) +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp6/ncp6_c6_scal.png", 
       plot = ncp6_c6, 
       width = 30, 
       height = 20, 
       units = "cm") 

#### 7 clusters ####

# original values

mean_7c <- r_hcpc_all %>%
  group_by(cluster7) %>% # by 7 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_7c_m <- mean_7c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster7) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_7c_s <- mean_7c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster7) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_7c_full <- full_join(mean_7c_m, mean_7c_s)

# histogram (original values)

ncp6_c7 <- mean_7c_full %>%
  ggplot(aes(x = cluster7, y = mean)) +
  geom_errorbar(aes(x = cluster7,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp6/ncp6_c7_ori.png", 
       plot = ncp6_c7, 
       width = 30, 
       height = 20, 
       units = "cm") 

# scaled values

mean_7c <- r_hcpc_s_all %>%
  group_by(cluster7) %>% # by 7 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_7c_m <- mean_7c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster7) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_7c_s <- mean_7c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster7) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_7c_full <- full_join(mean_7c_m, mean_7c_s)

# histogram (scaled values)

ncp6_c7 <- mean_7c_full %>%
  ggplot(aes(x = cluster7, y = mean)) +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp6/ncp6_c7_scal.png", 
       plot = ncp6_c7, 
       width = 30, 
       height = 20,  
       units = "cm") 

#### 8 clusters ####

# original values

mean_8c <- r_hcpc_all %>%
  group_by(cluster8) %>% # by 8 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_8c_m <- mean_8c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster8) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_8c_s <- mean_8c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster8) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_8c_full <- full_join(mean_8c_m, mean_8c_s)

# histogram (original values)

ncp6_c8 <- mean_8c_full %>%
  ggplot(aes(x = cluster8, y = mean)) +
  geom_errorbar(aes(x = cluster8,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp6/ncp6_c8_ori.png", 
       plot = ncp6_c8, 
       width = 30, 
       height = 20, 
       units = "cm") 

# scaled values

mean_8c <- r_hcpc_s_all %>%
  group_by(cluster8) %>% # by 8 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_8c_m <- mean_8c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster8) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_8c_s <- mean_8c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster8) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_8c_full <- full_join(mean_8c_m, mean_8c_s)

# histogram (scaled values)

ncp6_c8 <- mean_8c_full %>%
  ggplot(aes(x = cluster8, y = mean)) +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp6/ncp6_c8_scal.png", 
       plot = ncp6_c8, 
       width = 30, 
       height = 20,  
       units = "cm") 

#### 9 clusters ####

# original values

mean_9c <- r_hcpc_all %>%
  group_by(cluster9) %>% # by 9 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_9c_m <- mean_9c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster9) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_9c_s <- mean_9c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster9) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_9c_full <- full_join(mean_9c_m, mean_9c_s)

# histogram (original values)

ncp6_c9 <- mean_9c_full %>%
  ggplot(aes(x = cluster9, y = mean)) +
  geom_errorbar(aes(x = cluster9,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp6/ncp6_c9_ori.png", 
       plot = ncp6_c9, 
       width = 30, 
       height = 20, 
       units = "cm") 

# scaled values

mean_9c <- r_hcpc_s_all %>%
  group_by(cluster9) %>% # by 9 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_9c_m <- mean_9c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster9) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_9c_s <- mean_9c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster9) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_9c_full <- full_join(mean_9c_m, mean_9c_s)

# histogram (scaled values)

ncp6_c9 <- mean_9c_full %>%
  ggplot(aes(x = cluster9, y = mean)) +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp6/ncp6_c9_scal.png", 
       plot = ncp6_c9, 
       width = 30, 
       height = 20, 
       units = "cm") 

#### 10 clusters ####

# original values

mean_10c <- r_hcpc_all %>%
  group_by(cluster10) %>% # by 10 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_10c_m <- mean_10c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster10) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_10c_s <- mean_10c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster10) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_10c_full <- full_join(mean_10c_m, mean_10c_s)

# histogram (original values)

ncp6_c10 <- mean_10c_full %>%
  ggplot(aes(x = cluster10, y = mean)) +
  geom_errorbar(aes(x = cluster10,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp6/ncp6_c10_ori.png", 
       plot = ncp6_c10, 
       width = 30, 
       height = 20, 
       units = "cm") 

# scaled values

mean_10c <- r_hcpc_s_all %>%
  group_by(cluster10) %>% # by 10 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_10c_m <- mean_10c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster10) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_10c_s <- mean_10c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster10) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_10c_full <- full_join(mean_10c_m, mean_10c_s)

# histogram (scaled values)

ncp6_c10 <- mean_10c_full %>%
  ggplot(aes(x = cluster10, y = mean)) +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp6/ncp6_c10_scal.png", 
       plot = ncp6_c10, 
       width = 30, 
       height = 20,  
       units = "cm") 

#### 11 clusters ####

# original values

mean_11c <- r_hcpc_all %>%
  group_by(cluster11) %>% # by 11 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_11c_m <- mean_11c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster11) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_11c_s <- mean_11c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster11) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_11c_full <- full_join(mean_11c_m, mean_11c_s)

# histogram (original values)

ncp6_c11 <- mean_11c_full %>%
  ggplot(aes(x = cluster11, y = mean)) +
  geom_errorbar(aes(x = cluster11,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp6/ncp6_c11_ori.png", 
       plot = ncp6_c11, 
       width = 30, 
       height = 20,  
       units = "cm") 

# scaled values

mean_11c <- r_hcpc_s_all %>%
  group_by(cluster11) %>% # by 11 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_11c_m <- mean_11c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster11) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_11c_s <- mean_11c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster11) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_11c_full <- full_join(mean_11c_m, mean_11c_s)

# histogram (scaled values)

ncp6_c11 <- mean_11c_full %>%
  ggplot(aes(x = cluster11, y = mean)) +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp6/ncp6_c11_scal.png", 
       plot = ncp6_c11, 
       width = 30, 
       height = 20, 
       units = "cm") 

#### 12 clusters ####

# original values

mean_12c <- r_hcpc_all %>%
  group_by(cluster12) %>% # by 12 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_12c_m <- mean_12c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster12) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_12c_s <- mean_12c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster12) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_12c_full <- full_join(mean_12c_m, mean_12c_s)

# histogram (original values)

ncp6_c12 <- mean_12c_full %>%
  ggplot(aes(x = cluster12, y = mean)) +
  geom_errorbar(aes(x = cluster12,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp6/ncp6_c12_ori.png", 
       plot = ncp6_c12, 
       width = 30, 
       height = 20, 
       units = "cm") 

# scaled values

mean_12c <- r_hcpc_s_all %>%
  group_by(cluster12) %>% # by 12 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_12c_m <- mean_12c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster12) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_12c_s <- mean_12c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster12) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_12c_full <- full_join(mean_12c_m, mean_12c_s)

# histogram (scaled values)

ncp6_c12 <- mean_12c_full %>%
  ggplot(aes(x = cluster12, y = mean)) +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp6/ncp6_c12_scal.png", 
       plot = ncp6_c12, 
       width = 30, 
       height = 20, 
       units = "cm") 


# # anova test for each factor 
# 
# AOV_Output <- aov(SSA ~ cluster3, data = r_hcpc_all)
# summary(AOV_Output)
# 
# TukeyHSD(AOV_Output)


#### ncp=9 #### 

# compute PCA

res.pca <- PCA(df_new, 
               ncp = 9,
               scale.unit = TRUE, 
               quanti.sup = 12:18, 
               graph = FALSE)

summary(res.pca)

# scree plot

fviz_eig(res.pca)

# HCPC 

# Compute hierarchical clustering on principal components

# 2 clusters

res.hcpc2 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 2)

res.hcpc2$desc.var$quanti # contribution of factors  

# 3 clusters

res.hcpc3 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 3)

res.hcpc3$desc.var$quanti # contribution of factors  

# 4 clusters

res.hcpc4 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 4)

res.hcpc4$desc.var$quanti # contribution of factors 

# 5 clusters

res.hcpc5 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 5)

res.hcpc5$desc.var$quanti # contribution of factors 

# 6 clusters

res.hcpc6 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 6)

res.hcpc6$desc.var$quanti # contribution of factors 

# 7 clusters

res.hcpc7 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 7)

res.hcpc7$desc.var$quanti # contribution of factors 

# 8 clusters

res.hcpc8 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 8)

res.hcpc8$desc.var$quanti # contribution of factors 

# 9 clusters

res.hcpc9 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 9)
res.hcpc9$desc.var$quanti # contribution of factors 

# 10 clusters

res.hcpc10 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 10)

res.hcpc10$desc.var$quanti # contribution of factors 

# 11 clusters

res.hcpc11 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 11)

res.hcpc11$desc.var$quanti # contribution of factors 

# 12 clusters

res.hcpc12 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 12)

res.hcpc12$desc.var$quanti # contribution of factors 

# 13 clusters

res.hcpc13 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 13)

res.hcpc13$desc.var$quanti # contribution of factors 

# 14 clusters

res.hcpc14 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 14)

res.hcpc14$desc.var$quanti # contribution of factors 

# visualize the dendrogram generated by the hierarchical clustering

fviz_dend(res.hcpc14, 
          cex = 0.7, # label size
          palette = "jco",
          rect = TRUE, rect_fill = TRUE,
          rect_border = "jco",         
          labels_track_height = 0.8)

## the dentrogram suggest 3 clusters solution

# visualize individuals on the principal component map 
# and to color individuals according to the cluster they belong to (factorial map)

fviz_cluster(res.hcpc14,
             repel = FALSE, # avoid label overlapping
             show.clust.cent = TRUE, # show cluster centers
             # palette = "jco", 
             ggtheme = theme_minimal(),
             main = "Factor map"
)

# display the original data with cluster assignments

head(res.hcpc3$data.clust, 10) # 3 clusters as an example

## the last column contains the cluster assignments

# save the results of the HCPC into a dataframe

r_hcpc2_s <- res.hcpc2$data.clust %>% # 2 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster2 = clust)

r_hcpc2 <- r_hcpc2_s %>%
  select(Sample, cluster2)

r_hcpc3_s <- res.hcpc3$data.clust %>% # 3 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster3 = clust)

r_hcpc3 <- r_hcpc3_s %>%
  select(Sample, cluster3)

r_hcpc4_s <- res.hcpc4$data.clust %>% # 4 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster4 = clust)

r_hcpc4 <- r_hcpc4_s %>%
  select(Sample, cluster4)

r_hcpc5_s <- res.hcpc5$data.clust %>% # 4 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster5 = clust)

r_hcpc5 <- r_hcpc5_s %>%
  select(Sample, cluster5)

r_hcpc6_s <- res.hcpc6$data.clust %>% # 4 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster6 = clust)

r_hcpc6 <- r_hcpc6_s %>%
  select(Sample, cluster6)

r_hcpc7_s <- res.hcpc7$data.clust %>% # 7 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster7 = clust)

r_hcpc7 <- r_hcpc7_s %>%
  select(Sample, cluster7)

r_hcpc8_s <- res.hcpc8$data.clust %>% # 4 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster8 = clust) 

r_hcpc8 <- r_hcpc8_s %>%
  select(Sample, cluster8)

r_hcpc9_s <- res.hcpc9$data.clust %>% # 4 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster9 = clust) 

r_hcpc9 <- r_hcpc9_s %>%
  select(Sample, cluster9)

r_hcpc10_s <- res.hcpc10$data.clust %>% # 10 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster10 = clust)

r_hcpc10 <- r_hcpc10_s %>%
  select(Sample, cluster10)

r_hcpc11_s <- res.hcpc11$data.clust %>% # 4 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster11 = clust) 

r_hcpc11 <- r_hcpc11_s %>%
  select(Sample, cluster11)

r_hcpc12_s <- res.hcpc12$data.clust %>% # 12 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster12 = clust)

r_hcpc12 <- r_hcpc12_s %>%
  select(Sample, cluster12)

r_hcpc13_s <- res.hcpc13$data.clust %>% # 13 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster13 = clust)

r_hcpc13 <- r_hcpc13_s %>%
  select(Sample, cluster13)

r_hcpc14_s <- res.hcpc14$data.clust %>% # 4 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster14 = clust) 

r_hcpc14 <- r_hcpc14_s %>%
  select(Sample, cluster14)

# combine all the cluster results together to one single dataframe (unscaled values)

df_0 <- full_join(df2, df1) 

r_hcpc_inter <- left_join(df_0, r_hcpc2)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc3)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc4)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc5)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc6)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc7)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc8)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc9)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc10)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc11)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc12)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc13)

r_hcpc_all <- left_join(r_hcpc_inter, r_hcpc14)

# save the final dataset

write_csv(r_hcpc_all, "analysis/hcpc_ncp9.csv")

# combine all the cluster results together to one single dataframe (scaled values)

r_hcpc_s_inter <- left_join(r_hcpc2_s, r_hcpc3)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc4)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc5)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc6)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc7)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc8)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc9)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc10)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc11)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc12)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc13)

r_hcpc_s_all <- left_join(r_hcpc_s_inter, r_hcpc14)

# save the final dataset

write_csv(r_hcpc_s_all, "analysis/hcpc_scaled_ncp9.csv")

# calculate the mean value of each factor

#### 2 clusters ####

# original values

mean_2c <- r_hcpc_all %>%
  group_by(cluster2) %>% # by 2 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_2c_m <- mean_2c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster2) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_2c_s <- mean_2c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster2) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_2c_full <- full_join(mean_2c_m, mean_2c_s)

# histogram (original values)

ncp9_c2 <- mean_2c_full %>%
  ggplot(aes(x = cluster2, y = mean)) +
  geom_errorbar(aes(x = cluster2,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c2_ori.png", 
       plot = ncp9_c2, 
       width = 30, 
       height = 20, 
       units = "cm") 

# scaled values

mean_2c <- r_hcpc_s_all %>%
  group_by(cluster2) %>% # by 2 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_2c_m <- mean_2c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster2) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_2c_s <- mean_2c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster2) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_2c_full <- full_join(mean_2c_m, mean_2c_s)

# histogram (scaled values)

ncp9_c2 <- mean_2c_full %>%
  ggplot(aes(x = cluster2, y = mean)) +
  geom_errorbar(aes(x = cluster2,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c2_scal.png", 
       plot = ncp9_c2, 
       width = 30, 
       height = 20,  
       units = "cm") 

#### 3 clusters ####

# original values

mean_3c <- r_hcpc_all %>%
  group_by(cluster3) %>% # by 3 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_3c_m <- mean_3c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster3) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_3c_s <- mean_3c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster3) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_3c_full <- full_join(mean_3c_m, mean_3c_s)

# histogram (original values)

ncp9_c3 <- mean_3c_full %>%
  ggplot(aes(x = cluster3, y = mean)) +
  geom_errorbar(aes(x = cluster3,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c3_ori.png", 
       plot = ncp9_c3, 
       width = 30, 
       height = 20, 
       units = "cm") 

# scaled values

mean_3c <- r_hcpc_s_all %>%
  group_by(cluster3) %>% # by 3 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_3c_m <- mean_3c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster3) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_3c_s <- mean_3c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster3) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_3c_full <- full_join(mean_3c_m, mean_3c_s)

# histogram (scaled values)

ncp9_c3 <- mean_3c_full %>%
  ggplot(aes(x = cluster3, y = mean)) +
  geom_errorbar(aes(x = cluster3,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c3_scal.png", 
       plot = ncp9_c3, 
       width = 30, 
       height = 20, 
       units = "cm") 

#### 4 clusters ####

# original values

mean_4c <- r_hcpc_all %>%
  group_by(cluster4) %>% # by 4 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_4c_m <- mean_4c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster4) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_4c_s <- mean_4c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster4) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_4c_full <- full_join(mean_4c_m, mean_4c_s)

# histogram (original values)

ncp9_c4 <- mean_4c_full %>%
  ggplot(aes(x = cluster4, y = mean)) +
  geom_errorbar(aes(x = cluster4,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c4_ori.png", 
       plot = ncp9_c4, 
       width = 30, 
       height = 20, 
       units = "cm") 

# scaled values

mean_4c <- r_hcpc_s_all %>%
  group_by(cluster4) %>% # by 4 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_4c_m <- mean_4c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster4) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_4c_s <- mean_4c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster4) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_4c_full <- full_join(mean_4c_m, mean_4c_s)

# histogram (scaled values)

ncp9_c4 <- mean_4c_full %>%
  ggplot(aes(x = cluster4, y = mean)) +
  geom_errorbar(aes(x = cluster4,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c4_scal.png", 
       plot = ncp9_c4, 
       width = 30, 
       height = 20,  
       units = "cm") 

#### 5 clusters ####

# original values

mean_5c <- r_hcpc_all %>%
  group_by(cluster5) %>% # by 5 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_5c_m <- mean_5c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster5) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_5c_s <- mean_5c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster5) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_5c_full <- full_join(mean_5c_m, mean_5c_s)

# histogram (original values)

ncp9_c5 <- mean_5c_full %>%
  ggplot(aes(x = cluster5, y = mean)) +
  geom_errorbar(aes(x = cluster5,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c5_ori.png", 
       plot = ncp9_c5, 
       width = 30, 
       height = 20, 
       units = "cm") 

# scaled values

mean_5c <- r_hcpc_s_all %>%
  group_by(cluster5) %>% # by 5 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_5c_m <- mean_5c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster5) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_5c_s <- mean_5c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster5) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_5c_full <- full_join(mean_5c_m, mean_5c_s)

# histogram (scaled values)

ncp9_c5 <- mean_5c_full %>%
  ggplot(aes(x = cluster5, y = mean)) +
  geom_errorbar(aes(x = cluster5,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c5_scal.png", 
       plot = ncp9_c5, 
       width = 30, 
       height = 20,  
       units = "cm") 

#### 6 clusters ####

# original values

mean_6c <- r_hcpc_all %>%
  group_by(cluster6) %>% # by 6 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_6c_m <- mean_6c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster6) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_6c_s <- mean_6c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster6) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_6c_full <- full_join(mean_6c_m, mean_6c_s)

# histogram (original values)

ncp9_c6 <- mean_6c_full %>%
  ggplot(aes(x = cluster6, y = mean)) +
  geom_errorbar(aes(x = cluster6,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c6_ori.png", 
       plot = ncp9_c6, 
       width = 30, 
       height = 20, 
       units = "cm") 

# scaled values

mean_6c <- r_hcpc_s_all %>%
  group_by(cluster6) %>% # by 6 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_6c_m <- mean_6c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster6) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_6c_s <- mean_6c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster6) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_6c_full <- full_join(mean_6c_m, mean_6c_s)

# histogram (scaled values)

ncp9_c6 <- mean_6c_full %>%
  ggplot(aes(x = cluster6, y = mean)) +
  geom_errorbar(aes(x = cluster6,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c6_scal.png", 
       plot = ncp9_c6, 
       width = 30, 
       height = 20, 
       units = "cm") 

#### 7 clusters ####

# original values

mean_7c <- r_hcpc_all %>%
  group_by(cluster7) %>% # by 7 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_7c_m <- mean_7c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster7) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_7c_s <- mean_7c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster7) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_7c_full <- full_join(mean_7c_m, mean_7c_s)

# histogram (original values)

ncp9_c7 <- mean_7c_full %>%
  ggplot(aes(x = cluster7, y = mean)) +
  geom_errorbar(aes(x = cluster7,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c7_ori.png", 
       plot = ncp9_c7, 
       width = 30, 
       height = 20, 
       units = "cm") 

# scaled values

mean_7c <- r_hcpc_s_all %>%
  group_by(cluster7) %>% # by 7 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_7c_m <- mean_7c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster7) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_7c_s <- mean_7c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster7) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_7c_full <- full_join(mean_7c_m, mean_7c_s)

# histogram (scaled values)

ncp9_c7 <- mean_7c_full %>%
  ggplot(aes(x = cluster7, y = mean)) +
  geom_errorbar(aes(x = cluster7,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c7_scal.png", 
       plot = ncp9_c7, 
       width = 30, 
       height = 20,  
       units = "cm") 

#### 8 clusters ####

# original values

mean_8c <- r_hcpc_all %>%
  group_by(cluster8) %>% # by 8 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_8c_m <- mean_8c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster8) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_8c_s <- mean_8c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster8) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_8c_full <- full_join(mean_8c_m, mean_8c_s)

# histogram (original values)

ncp9_c8 <- mean_8c_full %>%
  ggplot(aes(x = cluster8, y = mean)) +
  geom_errorbar(aes(x = cluster8,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c8_ori.png", 
       plot = ncp9_c8, 
       width = 30, 
       height = 20, 
       units = "cm") 

# scaled values

mean_8c <- r_hcpc_s_all %>%
  group_by(cluster8) %>% # by 8 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_8c_m <- mean_8c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster8) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_8c_s <- mean_8c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster8) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_8c_full <- full_join(mean_8c_m, mean_8c_s)

# histogram (scaled values)

ncp9_c8 <- mean_8c_full %>%
  ggplot(aes(x = cluster8, y = mean)) +
  geom_errorbar(aes(x = cluster8,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c8_scal.png", 
       plot = ncp9_c8, 
       width = 30, 
       height = 20,  
       units = "cm") 

#### 9 clusters ####

# original values

mean_9c <- r_hcpc_all %>%
  group_by(cluster9) %>% # by 9 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_9c_m <- mean_9c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster9) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_9c_s <- mean_9c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster9) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_9c_full <- full_join(mean_9c_m, mean_9c_s)

# histogram (original values)

ncp9_c9 <- mean_9c_full %>%
  ggplot(aes(x = cluster9, y = mean)) +
  geom_errorbar(aes(x = cluster9,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c9_ori.png", 
       plot = ncp9_c9, 
       width = 30, 
       height = 20, 
       units = "cm") 

# scaled values

mean_9c <- r_hcpc_s_all %>%
  group_by(cluster9) %>% # by 9 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_9c_m <- mean_9c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster9) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_9c_s <- mean_9c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster9) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_9c_full <- full_join(mean_9c_m, mean_9c_s)

# histogram (scaled values)

ncp9_c9 <- mean_9c_full %>%
  ggplot(aes(x = cluster9, y = mean)) +
  geom_errorbar(aes(x = cluster9,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c9_scal.png", 
       plot = ncp9_c9, 
       width = 30, 
       height = 20,  
       units = "cm") 

#### 10 clusters ####

# original values

mean_10c <- r_hcpc_all %>%
  group_by(cluster10) %>% # by 10 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_10c_m <- mean_10c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster10) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_10c_s <- mean_10c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster10) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_10c_full <- full_join(mean_10c_m, mean_10c_s)

# histogram (original values)

ncp9_c10 <- mean_10c_full %>%
  ggplot(aes(x = cluster10, y = mean)) +
  geom_errorbar(aes(x = cluster10,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c10_ori.png", 
       plot = ncp9_c10, 
       width = 30, 
       height = 20, 
       units = "cm") 

# scaled values

mean_10c <- r_hcpc_s_all %>%
  group_by(cluster10) %>% # by 10 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_10c_m <- mean_10c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster10) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_10c_s <- mean_10c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster10) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_10c_full <- full_join(mean_10c_m, mean_10c_s)

# histogram (scaled values)

ncp9_c10 <- mean_10c_full %>%
  ggplot(aes(x = cluster10, y = mean)) +
  geom_errorbar(aes(x = cluster10,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c10_scal.png", 
       plot = ncp9_c10, 
       width = 30, 
       height = 20, 
       units = "cm") 

#### 11 clusters ####

# original values

mean_11c <- r_hcpc_all %>%
  group_by(cluster11) %>% # by 11 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_11c_m <- mean_11c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster11) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_11c_s <- mean_11c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster11) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_11c_full <- full_join(mean_11c_m, mean_11c_s)

# histogram (original values)

ncp9_c11 <- mean_11c_full %>%
  ggplot(aes(x = cluster11, y = mean)) +
  geom_errorbar(aes(x = cluster11,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c11_ori.png", 
       plot = ncp9_c11, 
       width = 30, 
       height = 20,  
       units = "cm") 

# scaled values

mean_11c <- r_hcpc_s_all %>%
  group_by(cluster11) %>% # by 11 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_11c_m <- mean_11c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster11) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_11c_s <- mean_11c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster11) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_11c_full <- full_join(mean_11c_m, mean_11c_s)

# histogram (scaled values)

ncp9_c11 <- mean_11c_full %>%
  ggplot(aes(x = cluster11, y = mean)) +
  geom_errorbar(aes(x = cluster11,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c11_scal.png", 
       plot = ncp9_c11, 
       width = 30, 
       height = 20, 
       units = "cm") 

#### 12 clusters ####

# original values

mean_12c <- r_hcpc_all %>%
  group_by(cluster12) %>% # by 12 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_12c_m <- mean_12c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster12) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_12c_s <- mean_12c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster12) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_12c_full <- full_join(mean_12c_m, mean_12c_s)

# histogram (original values)

ncp9_c12 <- mean_12c_full %>%
  ggplot(aes(x = cluster12, y = mean)) +
  geom_errorbar(aes(x = cluster12,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c12_ori.png", 
       plot = ncp9_c12, 
       width = 30, 
       height = 20, 
       units = "cm") 

# scaled values

mean_12c <- r_hcpc_s_all %>%
  group_by(cluster12) %>% # by 12 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_12c_m <- mean_12c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster12) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_12c_s <- mean_12c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster12) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_12c_full <- full_join(mean_12c_m, mean_12c_s)

# histogram (scaled values)

ncp9_c12 <- mean_12c_full %>%
  ggplot(aes(x = cluster12, y = mean)) +
  geom_errorbar(aes(x = cluster12,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c12_scal.png", 
       plot = ncp9_c12, 
       width = 30, 
       height = 20, 
       units = "cm") 

#### 13 clusters ####

# original values

mean_13c <- r_hcpc_all %>%
  group_by(cluster13) %>% # by 13 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_13c_m <- mean_13c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster13) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_13c_s <- mean_13c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster13) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_13c_full <- full_join(mean_13c_m, mean_13c_s)

# histogram (original values)

ncp9_c13 <- mean_13c_full %>%
  ggplot(aes(x = cluster13, y = mean)) +
  geom_errorbar(aes(x = cluster13,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c13_ori.png", 
       plot = ncp9_c13, 
       width = 30, 
       height = 20, 
       units = "cm") 

# scaled values

mean_13c <- r_hcpc_s_all %>%
  group_by(cluster13) %>% # by 13 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_13c_m <- mean_13c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster13) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_13c_s <- mean_13c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster13) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_13c_full <- full_join(mean_13c_m, mean_13c_s)

# histogram (scaled values)

ncp9_c13 <- mean_13c_full %>%
  ggplot(aes(x = cluster13, y = mean)) +
  geom_errorbar(aes(x = cluster13,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c13_scal.png", 
       plot = ncp9_c13, 
       width = 30, 
       height = 20, 
       units = "cm") 

#### 14 clusters ####

# original values

mean_14c <- r_hcpc_all %>%
  group_by(cluster14) %>% # by 14 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_14c_m <- mean_14c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster14) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_14c_s <- mean_14c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster14) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_14c_full <- full_join(mean_14c_m, mean_14c_s)

# histogram (original values)

ncp9_c14 <- mean_14c_full %>%
  ggplot(aes(x = cluster14, y = mean)) +
  geom_errorbar(aes(x = cluster14,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c14_ori.png", 
       plot = ncp9_c14, 
       width = 30, 
       height = 20, 
       units = "cm") 

# scaled values

mean_14c <- r_hcpc_s_all %>%
  group_by(cluster14) %>% # by 14 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_14c_m <- mean_14c %>%
  select(1:19) %>%
  gather("factor", "mean", -cluster14) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_14c_s <- mean_14c %>%
  select(1, 38:55) %>%
  gather("factor", "se", -cluster14) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_14c_full <- full_join(mean_14c_m, mean_14c_s)

# histogram (scaled values)

ncp6_c14 <- mean_14c_full %>%
  ggplot(aes(x = cluster14, y = mean)) +
  geom_errorbar(aes(x = cluster14,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "red") +
  geom_bar(stat = 'identity', width = 0.3) +
  facet_wrap(~factor, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c14_scal.png", 
       plot = ncp9_c14, 
       width = 30, 
       height = 20, 
       units = "cm") 

#### the following scripts wait for moidifying ####

#### ncp=7 #### 

# compute PCA

res.pca <- PCA(df_new, 
               ncp = 7,
               scale.unit = TRUE, 
               quanti.sup = 12:18,  # add RVA and kinetics as supplementary data (illustrative data)
               graph = FALSE)

summary(res.pca)

# scree plot

fviz_eig(res.pca)

#### HCPC ####

# Compute hierarchical clustering on principal components

# 2 clusters

res.hcpc2 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 2)

res.hcpc2$desc.var$quanti # contribution of factors  

# 3 clusters

res.hcpc3 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 3)

res.hcpc3$desc.var$quanti # contribution of factors  

# 4 clusters

res.hcpc4 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 4)

res.hcpc4$desc.var$quanti # contribution of factors  

# 5 clusters

res.hcpc5 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 5)

res.hcpc5$desc.var$quanti # contribution of factors  

# 6 clusters

res.hcpc6 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 6)

res.hcpc6$desc.var$quanti # contribution of factors  

# 7 clusters

res.hcpc7 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 7)

res.hcpc7$desc.var$quanti # contribution of factors  

# 8 clusters

res.hcpc8 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 8)

res.hcpc8$desc.var$quanti # contribution of factors  

# 9 clusters

res.hcpc9 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 9)

res.hcpc9$desc.var$quanti # contribution of factors  

# 10 clusters

res.hcpc10 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 10)

res.hcpc10$desc.var$quanti # contribution of factors  

# 11 clusters

res.hcpc11 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 11)

res.hcpc11$desc.var$quanti # contribution of factors 

# 12 clusters

res.hcpc12 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 12)

res.hcpc12$desc.var$quanti # contribution of factors 

# visualize the dendrogram generated by the hierarchical clustering

fviz_dend(res.hcpc14, 
          cex = 0.7, # label size
          palette = "jco",
          rect = TRUE, rect_fill = TRUE,
          rect_border = "jco",         
          labels_track_height = 0.8)

## the dentrogram suggest 3 clusters solution

# visualize individuals on the principal component map 
# and to color individuals according to the cluster they belong to (factorial map)

fviz_cluster(res.hcpc4,
             repel = FALSE, # avoid label overlapping
             show.clust.cent = TRUE, # show cluster centers
             # palette = "jco", 
             ggtheme = theme_minimal(),
             main = "Factor map"
)

# display the original data with cluster assignments

head(res.hcpc3$data.clust, 10) # 3 clusters as an example

## the last column contains the cluster assignments

# save the results of the HCPC into a dataframe

r_hcpc4_s <- res.hcpc4$data.clust %>% # 4 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster4 = clust)

r_hcpc4 <- r_hcpc4_s %>%
  select(Sample, cluster4)

r_hcpc5_s <- res.hcpc5$data.clust %>% # 4 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster5 = clust)

r_hcpc5 <- r_hcpc5_s %>%
  select(Sample, cluster5)

r_hcpc6_s <- res.hcpc6$data.clust %>% # 4 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster6 = clust)

r_hcpc6 <- r_hcpc6_s %>%
  select(Sample, cluster6)

r_hcpc7_s <- res.hcpc7$data.clust %>% # 4 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster7 = clust) 

r_hcpc7 <- r_hcpc7_s %>%
  select(Sample, cluster7)

r_hcpc8_s <- res.hcpc8$data.clust %>% # 4 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster8 = clust) 

r_hcpc8 <- r_hcpc8_s %>%
  select(Sample, cluster8)

r_hcpc10_s <- res.hcpc10$data.clust %>% # 4 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster10 = clust) 

r_hcpc10 <- r_hcpc10_s %>%
  select(Sample, cluster10)

r_hcpc12_s <- res.hcpc12$data.clust %>% # 4 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster12 = clust) 

r_hcpc12 <- r_hcpc12_s %>%
  select(Sample, cluster12)

# combine all the cluster results together to one single dataframe (unscaled values)

df_0 <- full_join(df2, df1) 

r_hcpc_inter <- left_join(df_0, r_hcpc4)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc5)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc6)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc7)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc8)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc10)

r_hcpc_all <- left_join(r_hcpc_inter, r_hcpc12)

# save the final dataset

write_csv(r_hcpc_all, "analysis/hcpc_ncp7.csv")

# combine all the cluster results together to one single dataframe (scaled values)

r_hcpc_s_inter <- left_join(r_hcpc4_s, r_hcpc5)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc6)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc7)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc8)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc10)

r_hcpc_s_all <- left_join(r_hcpc_s_inter, r_hcpc12)

# save the final dataset

write_csv(r_hcpc_s_all, "analysis/hcpc_scaled_ncp7.csv")

#### calculate the mean value of each factor ####

mean_4c <- r_hcpc_s_all %>%
  group_by(cluster4) %>% # by 3 clusters
  summarise_if(is.numeric, mean, na.rm = TRUE) 

mean_5c <- r_hcpc_s_all %>%
  group_by(cluster5) %>% # by 4 clusters
  summarise_if(is.numeric, mean, na.rm = TRUE) 

mean_6c <- r_hcpc_s_all %>%
  group_by(cluster6) %>% # by 4 clusters
  summarise_if(is.numeric, mean, na.rm = TRUE) 

mean_7c <- r_hcpc_s_all %>%
  group_by(cluster7) %>% # by 4 clusters
  summarise_if(is.numeric, mean, na.rm = TRUE) 

mean_8c <- r_hcpc_s_all %>%
  group_by(cluster8) %>% # by 7 clusters
  summarise_if(is.numeric, mean, na.rm = TRUE) 

mean_10c <- r_hcpc_s_all %>%
  group_by(cluster10) %>% # by 9 clusters
  summarise_if(is.numeric, mean, na.rm = TRUE) 

mean_12c <- r_hcpc_s_all %>%
  group_by(cluster12) %>% # by 10 clusters
  summarise_if(is.numeric, mean, na.rm = TRUE) 

#### histogram for each factor ####

# 4 clusters

mean_4c_gather <- mean_4c %>%
  gather("factor", "mean", -cluster4)

mean_4c_gather %>%
  ggplot(aes(x = cluster4, y = mean)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~factor)

# 5 clusters

mean_5c_gather <- mean_5c %>%
  gather("factor", "mean", -cluster5)

mean_5c_gather %>%
  ggplot(aes(x = cluster5, y = mean)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~factor)

# 6 clusters

mean_6c_gather <- mean_6c %>%
  gather("factor", "mean", -cluster6)

mean_6c_gather %>%
  ggplot(aes(x = cluster6, y = mean)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~factor)

# 7 clusters

mean_7c_gather <- mean_7c %>%
  gather("factor", "mean", -cluster7)

mean_7c_gather %>%
  ggplot(aes(x = cluster7, y = mean)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~factor)

# 8 clusters

mean_8c_gather <- mean_8c %>%
  gather("factor", "mean", -cluster8)

mean_8c_gather %>%
  ggplot(aes(x = cluster8, y = mean)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~factor)

# 10 clusters

mean_10c_gather <- mean_10c %>%
  gather("factor", "mean", -cluster10)

mean_10c_gather %>%
  ggplot(aes(x = cluster10, y = mean)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~factor)

# 12 clusters

mean_12c_gather <- mean_12c %>%
  gather("factor", "mean", -cluster12)

mean_12c_gather %>%
  ggplot(aes(x = cluster12, y = mean)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~factor)


#### anova test for each factor ####

AOV_Output <- aov(SSA ~ cluster3, data = r_hcpc_all)
summary(AOV_Output)

TukeyHSD(AOV_Output)

# variability

res.hcpc4$desc.var$quanti

res.hcpc5$desc.var$quanti

res.hcpc6$desc.var$quanti

res.hcpc7$desc.var$quanti

res.hcpc8$desc.var$quanti

res.hcpc10$desc.var$quanti

res.hcpc12$desc.var$quanti

#### ncp=8 #### 

# compute PCA

res.pca <- PCA(df_new, 
               ncp = 8,
               scale.unit = TRUE, 
               quanti.sup = 12:18, # add RVA and kinetics as supplementary data (illustrative data)
               graph = FALSE)

summary(res.pca)

# scree plot

fviz_eig(res.pca)

#### HCPC ####

# Compute hierarchical clustering on principal components

# 2 clusters

res.hcpc2 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 2)

res.hcpc2$desc.var$quanti # contribution of factors

# 3 clusters

res.hcpc3 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 3)

res.hcpc3$desc.var$quanti # contribution of factors

# 4 clusters

res.hcpc4 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 4)

res.hcpc4$desc.var$quanti # contribution of factors

# 5 clusters

res.hcpc5 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 5)

res.hcpc5$desc.var$quanti # contribution of factors

# 6 clusters

res.hcpc6 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 6)

res.hcpc6$desc.var$quanti # contribution of factors

# 7 clusters

res.hcpc7 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 7)

res.hcpc7$desc.var$quanti # contribution of factors

# 8 clusters

res.hcpc8 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 8)

res.hcpc8$desc.var$quanti # contribution of factors

# 9 clusters

res.hcpc9 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 9)

res.hcpc9$desc.var$quanti # contribution of factors

# 10 clusters

res.hcpc10 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 10)

res.hcpc10$desc.var$quanti # contribution of factors

# 11 clusters

res.hcpc11 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 11)

res.hcpc11$desc.var$quanti # contribution of factors

# 12 clusters

res.hcpc12 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 12)

res.hcpc12$desc.var$quanti # contribution of factors

# visualize the dendrogram generated by the hierarchical clustering

fviz_dend(res.hcpc14, 
          cex = 0.7, # label size
          palette = "jco",
          rect = TRUE, rect_fill = TRUE,
          rect_border = "jco",         
          labels_track_height = 0.8)

## the dentrogram suggest 3 clusters solution

# visualize individuals on the principal component map 
# and to color individuals according to the cluster they belong to (factorial map)

fviz_cluster(res.hcpc4,
             repel = FALSE, # avoid label overlapping
             show.clust.cent = TRUE, # show cluster centers
             palette = "jco",
             ggtheme = theme_minimal(),
             main = "Factor map"
)

# display the original data with cluster assignments

head(res.hcpc3$data.clust, 10) # 3 clusters as an example

## the last column contains the cluster assignments

# save the results of the HCPC into a dataframe

r_hcpc3_s <- res.hcpc3$data.clust %>% # 4 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster3 = clust)

r_hcpc3 <- r_hcpc3_s %>%
  select(Sample, cluster3)

r_hcpc5_s <- res.hcpc5$data.clust %>% # 4 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster5 = clust)

r_hcpc5 <- r_hcpc5_s %>%
  select(Sample, cluster5)

r_hcpc6_s <- res.hcpc6$data.clust %>% # 4 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster6 = clust)

r_hcpc6 <- r_hcpc6_s %>%
  select(Sample, cluster6)

r_hcpc10_s <- res.hcpc10$data.clust %>% # 4 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster10 = clust) 

r_hcpc10 <- r_hcpc10_s %>%
  select(Sample, cluster10)

r_hcpc12_s <- res.hcpc12$data.clust %>% # 4 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster12 = clust) 

r_hcpc12 <- r_hcpc12_s %>%
  select(Sample, cluster12)

# combine all the cluster results together to one single dataframe (unscaled values)

df_0 <- full_join(df2, df1) 

r_hcpc_inter <- left_join(df_0, r_hcpc3)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc5)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc6)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc10)

r_hcpc_all <- left_join(r_hcpc_inter, r_hcpc12)

# save the final dataset

write_csv(r_hcpc_all, "analysis/hcpc_ncp8.csv")

# combine all the cluster results together to one single dataframe (scaled values)

r_hcpc_s_inter <- left_join(r_hcpc3_s, r_hcpc5)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc6)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc10)

r_hcpc_s_all <- left_join(r_hcpc_s_inter, r_hcpc12)

# save the final dataset

write_csv(r_hcpc_s_all, "analysis/hcpc_scaled_ncp8.csv")

#### calculate the mean value of each factor ####

mean_3c <- r_hcpc_s_all %>%
  group_by(cluster3) %>% # by 3 clusters
  summarise_if(is.numeric, mean, na.rm = TRUE) 

mean_5c <- r_hcpc_s_all %>%
  group_by(cluster5) %>% # by 4 clusters
  summarise_if(is.numeric, mean, na.rm = TRUE) 

mean_6c <- r_hcpc_s_all %>%
  group_by(cluster6) %>% # by 4 clusters
  summarise_if(is.numeric, mean, na.rm = TRUE) 

mean_10c <- r_hcpc_s_all %>%
  group_by(cluster10) %>% # by 9 clusters
  summarise_if(is.numeric, mean, na.rm = TRUE) 

mean_12c <- r_hcpc_s_all %>%
  group_by(cluster12) %>% # by 10 clusters
  summarise_if(is.numeric, mean, na.rm = TRUE) 

#### histogram for each factor ####

# 3 clusters

mean_3c_gather <- mean_3c %>%
  gather("factor", "mean", -cluster3)

mean_3c_gather %>%
  ggplot(aes(x = cluster3, y = mean)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~factor)

# 5 clusters

mean_5c_gather <- mean_5c %>%
  gather("factor", "mean", -cluster5)

mean_5c_gather %>%
  ggplot(aes(x = cluster5, y = mean)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~factor)

# 6 clusters

mean_6c_gather <- mean_6c %>%
  gather("factor", "mean", -cluster6)

mean_6c_gather %>%
  ggplot(aes(x = cluster6, y = mean)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~factor)

# 10 clusters

mean_10c_gather <- mean_10c %>%
  gather("factor", "mean", -cluster10)

mean_10c_gather %>%
  ggplot(aes(x = cluster10, y = mean)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~factor)

# 12 clusters

mean_12c_gather <- mean_12c %>%
  gather("factor", "mean", -cluster12)

mean_12c_gather %>%
  ggplot(aes(x = cluster12, y = mean)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~factor)


#### anova test for each factor ####

AOV_Output <- aov(SSA ~ cluster3, data = r_hcpc_all)
summary(AOV_Output)

TukeyHSD(AOV_Output)

# variability

res.hcpc3$desc.var$quanti

res.hcpc5$desc.var$quanti

res.hcpc6$desc.var$quanti

res.hcpc10$desc.var$quanti

res.hcpc12$desc.var$quanti

#### correlation matrix ####

ncp9 <- read_csv("analysis/hcpc_ncp9.csv")

# select cluster 3 under 11clusters

cluster11_3 <- ncp9 %>%
  filter(cluster11 == 3)

# select cluster 11 under 11clusters

cluster11_11 <- ncp9 %>%
  filter(cluster11 == 11)

# correlations 

library("PerformanceAnalytics")

cluster_11_3_s <- cluster11_3 %>%
  select(2:15)

chart.Correlation(cluster_11_3_s, histogram=TRUE, pch=19)

cluster_11_11_s <- cluster11_11 %>%
  select(2:15)

# nothing interesting found...

# corrplot

library(corrplot)

cor_result <- cor(cluster_11_3_s, use = "complete.obs") # noted that cluster_11_3_s is with similar amylose content

correlation <- corrplot(cor_result, method = "number", type = "lower",
                        tl.cex = 0.5, number.cex = 0.5) 

cor_result <- cor(cluster_11_11_s, use = "complete.obs") # noted that cluster_11_3_s is with similar amylose content

correlation <- corrplot(cor_result, method = "number", type = "lower",
                        tl.cex = 0.5, number.cex = 0.5) 

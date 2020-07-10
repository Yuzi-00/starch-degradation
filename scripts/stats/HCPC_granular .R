# **** this script is for the plots of each cluster **** 

library(tidyverse)

library(FactoMineR)

library(factoextra)

# library(plyr)
# 
# library(purrr)

df <- read_csv("analysis/total_new_convert_replaced_by_granular.csv")

# select the variables

df <- df %>%
  filter(Sample != "C+" & Sample != "C-") %>%
  na.omit()

# add the residuals of the h k model

res <- read_csv("analysis//hkmodel_residuals.csv")

df <- left_join(df, res) %>%
  rename(residual = .resid)

# calculate the mean value of the kinetics 

df1 <- df %>%
  select(2, 24:26, 37) %>%
  group_by(Sample) %>%
  summarise(k = mean(k), h = mean(h), Xinf = mean(Xinf), residual = mean(residual)) 

# select the structural properties

df2 <- df %>%
  select(2, 5:14, 27:36) %>% 
  unique() 

# combine and scale

df_new <- full_join(df2, df1) %>%
  select(Sample, pi_A, pi_B, pi_C, pi_AB_ratio, mu_A, mu_B, mu_C,
         sigma_A, sigma_B, sigma_C, DP6_12, DP13_24, DP25_36, DP37_47, 
         Amylose_Con, Amylase_Act, Peak_Vis, Trough_Vis,
         Final_Vis, Pasting_Temp, k, h, Xinf, residual) %>%
  remove_rownames() %>%
  column_to_rownames(var = 'Sample') %>%
  scale()

#### ncp=9 #### 

# compute PCA

res.pca <- PCA(df_new, 
               ncp = 11, # explain more than 99% of the variability 
               scale.unit = TRUE, 
               quanti.sup = 17:24, # 8 supplementary variables; 16 active variables 
               graph = FALSE)

summary(res.pca)

# Extract eigenvalues/variances

eig <- get_eig(res.pca) %>%
  as_tibble() %>%
  rownames_to_column() %>%
  rename(dimension = rowname) %>%
  mutate_if(is.numeric, round, digits = 1)

# scree plot

fviz_eig(res.pca, choice = "variance" ,addlabels = TRUE)

# cumulative scree plot

sp <- ggplot(data = eig, 
             aes(x = reorder(dimension, cumulative.variance.percent),
                 y = cumulative.variance.percent,
                 group = 1)) +
  geom_point(size = 1.5) +
  geom_line() +
  geom_bar(stat = 'identity', width = 0.7, fill = "#4682B4") +
  scale_x_discrete(limits = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
  geom_text(aes(label = cumulative.variance.percent, hjust = 0.5, vjust = -0.5),size = 5) +
  scale_y_continuous(limits = c(0,110), expand = c(0,0)) +
  labs(x = "Dimensions", y = "Cumulative proportion of variance explained (%)") +
  theme(axis.text.x = element_text(color="black", size=17), 
        axis.text.y = element_text(color="black", size=17)) +
  # change the color and size of the tick label for x and y axis
  theme(plot.margin = unit(c(5.5,12,5.5,5.5), "pt")) +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20)) +
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.ticks=element_line(
          colour="black",
          size=.5)) 

sp

# save the cumulative plot 

ggsave("figures/cumulative_screeplot_granular.png", 
       plot = sp,
       width = 25, 
       height = 20, 
       units = "cm") 

# HCPC 

# globle HCPC

res.hcpc <- HCPC(res.pca, graph = TRUE, 
                 consol = TRUE)

res.hcpc <- HCPC(res.pca, graph = FALSE)

fviz_dend(res.hcpc, k =100)

# Compute hierarchical clustering on principal components

# 2 clusters

res.hcpc2 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 2)

hcpc2 <- res.hcpc2$desc.var$quanti # contribution of factors  

hcpc2 <- do.call(rbind, lapply(hcpc2, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# save the results

write_csv(hcpc2, "analysis/ncp11/hcpc2_granular.csv")

# 3 clusters

res.hcpc3 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 3)

hcpc3 <- res.hcpc3$desc.var$quanti # contribution of factors  

hcpc3 <- do.call(rbind, lapply(hcpc3, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# save the results

write_csv(hcpc3, "analysis/ncp11/hcpc3_granular.csv")

# 4 clusters

res.hcpc4 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 4)

hcpc4 <- res.hcpc4$desc.var$quanti # contribution of factors 

hcpc4 <- do.call(rbind, lapply(hcpc4, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# save the results

write_csv(hcpc4, "analysis/ncp11/hcpc4_granular.csv")

# 5 clusters

res.hcpc5 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 5)

hcpc5 <- res.hcpc5$desc.var$quanti # contribution of factors 

hcpc5 <- do.call(rbind, lapply(hcpc5, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# # replace the NAs 
# 
# hcpc5[is.na(hcpc5)] = "D0.9" 

# save the results

write_csv(hcpc5, "analysis/ncp11/hcpc5_granular.csv")

# 6 clusters

res.hcpc6 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 6)

hcpc6 <- res.hcpc6$desc.var$quanti # contribution of factors 

hcpc6 <- do.call(rbind, lapply(hcpc6, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# # replace the NAs 
# 
# hcpc6[is.na(hcpc6)] = "D0.9"

# save the results

write_csv(hcpc6, "analysis/ncp11/hcpc6_granular.csv")

# 7 clusters

res.hcpc7 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 7)

hcpc7 <- res.hcpc7$desc.var$quanti # contribution of factors 

hcpc7 <- do.call(rbind, lapply(hcpc7, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# # replace the NAs 
# 
# hcpc7[is.na(hcpc7)] = "D0.9"

# save the results

write_csv(hcpc7, "analysis/ncp11/hcpc7_granular.csv")

# 8 clusters

res.hcpc8 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 8)

hcpc8 <- res.hcpc8$desc.var$quanti # contribution of factors 

hcpc8 <- do.call(rbind, lapply(hcpc8, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# # replace the NAs 
# 
# hcpc8[is.na(hcpc8)] = "D0.9"

# save the results

write_csv(hcpc8, "analysis/ncp11/hcpc8_granular.csv")

# 9 clusters

res.hcpc9 <- HCPC(res.pca, graph = TRUE, 
                  consol = TRUE,
                  nb.clust = 9)

hcpc9 <- res.hcpc9$desc.var$quanti # contribution of factors 

hcpc9 <- do.call(rbind, lapply(hcpc9, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# # replace the NAs 
# 
# hcpc9[is.na(hcpc9)] = "D0.9"

# save the results

write_csv(hcpc9, "analysis/ncp11/hcpc9_granular.csv")

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

# # replace the NAs 
# 
# hcpc10[is.na(hcpc10)] = "D0.9"

# save the results

write_csv(hcpc10, "analysis/ncp11/hcpc10_granular.csv")

#### wait to be modified ####

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

# # replace the NAs 
# 
# hcpc11[is.na(hcpc11)] = "D0.9"

# save the results

write_csv(hcpc11, "analysis/ncp9/hcpc11.csv")

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

# # replace the NAs 
# 
# hcpc12[is.na(hcpc12)] = "D0.9"

# save the results

write_csv(hcpc12, "analysis/ncp9/hcpc12.csv")

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

# # replace the NAs 
# 
# hcpc13[is.na(hcpc13)] = c("Amylase_Act", "D0.9")

# save the results

write_csv(hcpc13, "analysis/ncp9/hcpc13.csv")

# 14 clusters

res.hcpc14 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 14)

hcpc14 <- res.hcpc14$desc.var$quanti # contribution of factors 

hcpc14 <- do.call(rbind, lapply(hcpc14, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# # replace the NAs 
# 
# hcpc14[is.na(hcpc14)] = c("Amylase_Act", "D0.9")

# save the results

write_csv(hcpc14, "analysis/ncp9/hcpc14.csv")

# 15 clusters

res.hcpc15 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 15)

hcpc15 <- res.hcpc15$desc.var$quanti # contribution of factors 

hcpc15 <- do.call(rbind, lapply(hcpc15, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# # replace the NAs 
# 
# hcpc15[is.na(hcpc15)] = c("Amylase_Act", "D0.9")

# save the results

write_csv(hcpc15, "analysis/ncp9/hcpc15.csv")

# 16 clusters

res.hcpc16 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 16)

hcpc16 <- res.hcpc16$desc.var$quanti # contribution of factors 

hcpc16 <- do.call(rbind, lapply(hcpc16, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# # replace the NAs 
# 
# hcpc16[is.na(hcpc16)] = c("Amylase_Act", "D0.9")

# save the results

write_csv(hcpc16, "analysis/ncp9/hcpc16.csv")

# 17 clusters

res.hcpc17 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 17)

hcpc17 <- res.hcpc17$desc.var$quanti # contribution of factors 

hcpc17 <- do.call(rbind, lapply(hcpc17, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# replace the NAs 

hcpc17[is.na(hcpc17)] = c("Amylase_Act")

# save the results

write_csv(hcpc17, "analysis/ncp9/hcpc17.csv")

# 18 clusters

res.hcpc18 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 18)

hcpc18 <- res.hcpc18$desc.var$quanti # contribution of factors 

hcpc18 <- do.call(rbind, lapply(hcpc18, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# replace the NAs 

hcpc18[is.na(hcpc18)] = c("Amylase_Act")

# save the results

write_csv(hcpc18, "analysis/ncp9/hcpc18.csv")

# 19 clusters

res.hcpc19 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 19)

hcpc19 <- res.hcpc19$desc.var$quanti # contribution of factors 

hcpc19 <- do.call(rbind, lapply(hcpc19, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# replace the NAs 

hcpc19[is.na(hcpc19)] = c("Amylase_Act")

# save the results

write_csv(hcpc19, "analysis/ncp9/hcpc19.csv")

# 20 clusters

res.hcpc20 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 20)

hcpc20 <- res.hcpc20$desc.var$quanti # contribution of factors 

hcpc20 <- do.call(rbind, lapply(hcpc20, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# replace the NAs 

hcpc20[is.na(hcpc20)] = c("Amylase_Act")

# save the results

write_csv(hcpc20, "analysis/ncp9/hcpc20.csv")

# 21 clusters

res.hcpc21 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 21)

hcpc21 <- res.hcpc21$desc.var$quanti # contribution of factors 

hcpc21 <- do.call(rbind, lapply(hcpc21, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# save the results

write_csv(hcpc21, "analysis/ncp9/hcpc21.csv")

# 22 clusters

res.hcpc22 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 22)

hcpc22 <- res.hcpc22$desc.var$quanti # contribution of factors 

hcpc22 <- do.call(rbind, lapply(hcpc22, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# save the results

write_csv(hcpc22, "analysis/ncp9/hcpc22.csv")

# 23 clusters

res.hcpc23 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 23)

hcpc23 <- res.hcpc23$desc.var$quanti # contribution of factors 

hcpc23 <- do.call(rbind, lapply(hcpc23, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# save the results

write_csv(hcpc23, "analysis/ncp9/hcpc23.csv")

# 24 clusters

res.hcpc24 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 24)

hcpc24 <- res.hcpc24$desc.var$quanti # contribution of factors 

hcpc24 <- do.call(rbind, lapply(hcpc24, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# save the results

write_csv(hcpc24, "analysis/ncp9/hcpc24.csv")

# 25 clusters

res.hcpc25 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 25)

hcpc25 <- res.hcpc25$desc.var$quanti # contribution of factors 

hcpc25 <- do.call(rbind, lapply(hcpc25, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# save the results

write_csv(hcpc25, "analysis/ncp9/hcpc25.csv")

# 26 clusters

res.hcpc26 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 26)

hcpc26 <- res.hcpc26$desc.var$quanti # contribution of factors 

hcpc26 <- do.call(rbind, lapply(hcpc26, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# save the results

write_csv(hcpc26, "analysis/ncp9/hcpc26.csv")

# 27 clusters

res.hcpc27 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 27)

hcpc27 <- res.hcpc27$desc.var$quanti # contribution of factors 

hcpc27 <- do.call(rbind, lapply(hcpc27, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# replace the NAs 

hcpc27[is.na(hcpc27)] = c("Amylase_Act", "DP13_24")

# save the results

write_csv(hcpc27, "analysis/ncp9/hcpc27.csv")

# 28 clusters

res.hcpc28 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 28)

hcpc28 <- res.hcpc28$desc.var$quanti # contribution of factors 

hcpc28 <- do.call(rbind, lapply(hcpc28, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# replace the NAs 

hcpc28[is.na(hcpc28)] = c("Amylase_Act", "DP13_24")

# save the results

write_csv(hcpc28, "analysis/ncp9/hcpc28.csv")

# 29 clusters

res.hcpc29 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 29)

hcpc29 <- res.hcpc29$desc.var$quanti # contribution of factors 

hcpc29 <- do.call(rbind, lapply(hcpc29, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# replace the NAs 

hcpc29[is.na(hcpc29)] = c("Amylase_Act", "DP13-24", "Amylase_Act")

# save the results

write_csv(hcpc29, "analysis/ncp9/hcpc29.csv")

# 30 clusters

res.hcpc30 <- HCPC(res.pca, graph = TRUE, 
                   consol = TRUE,
                   nb.clust = 30)

hcpc30 <- res.hcpc30$desc.var$quanti # contribution of factors 

hcpc30 <- do.call(rbind, lapply(hcpc30, as.data.frame)) %>%
  rownames_to_column() %>%
  mutate(rowname = str_replace(rowname, "\\.", "-")) %>%
  separate(col = rowname,
           into = c("cluster", "factor"),
           sep = "-") 

# replace the NAs 

hcpc30[is.na(hcpc30)] = c("Amylase_Act", "DP13-24", "Amylase_Act", "VWM")

# save the results

write_csv(hcpc30, "analysis/ncp9/hcpc30.csv")

####

# # visualize the dendrogram generated by the hierarchical clustering
# 
# fviz_dend(res.hcpc14, 
#           cex = 0.7, # label size
#           palette = "jco",
#           rect = TRUE, rect_fill = TRUE,
#           rect_border = "jco",         
#           labels_track_height = 0.8)
# 
# ## the dentrogram suggest 3 clusters solution
# 
# # visualize individuals on the principal component map 
# # and to color individuals according to the cluster they belong to (factorial map)
# 
# fviz_cluster(res.hcpc14,
#              repel = FALSE, # avoid label overlapping
#              show.clust.cent = TRUE, # show cluster centers
#              # palette = "jco", 
#              ggtheme = theme_minimal(),
#              main = "Factor map"
# )
# 
# # display the original data with cluster assignments
# 
# head(res.hcpc3$data.clust, 10) # 3 clusters as an example

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

#### wait to be modified02 ####

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

r_hcpc14_s <- res.hcpc14$data.clust %>% # 14 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster14 = clust) 

r_hcpc14 <- r_hcpc14_s %>%
  select(Sample, cluster14)

r_hcpc15_s <- res.hcpc15$data.clust %>% # 15 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster15 = clust) 

r_hcpc15 <- r_hcpc15_s %>%
  select(Sample, cluster15)

r_hcpc16_s <- res.hcpc16$data.clust %>% # 16 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster16 = clust) 

r_hcpc16 <- r_hcpc16_s %>%
  select(Sample, cluster16)

r_hcpc17_s <- res.hcpc17$data.clust %>% # 17 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster17 = clust) 

r_hcpc17 <- r_hcpc17_s %>%
  select(Sample, cluster17)

r_hcpc18_s <- res.hcpc18$data.clust %>% # 18 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster18 = clust) 

r_hcpc18 <- r_hcpc18_s %>%
  select(Sample, cluster18)

r_hcpc19_s <- res.hcpc19$data.clust %>% # 19 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster19 = clust) 

r_hcpc19 <- r_hcpc19_s %>%
  select(Sample, cluster19)

r_hcpc20_s <- res.hcpc20$data.clust %>% # 20 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster20 = clust) 

r_hcpc20 <- r_hcpc20_s %>%
  select(Sample, cluster20)

r_hcpc21_s <- res.hcpc21$data.clust %>% # 21 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster21 = clust) 

r_hcpc21 <- r_hcpc21_s %>%
  select(Sample, cluster21)

r_hcpc22_s <- res.hcpc22$data.clust %>% # 22 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster22 = clust) 

r_hcpc22 <- r_hcpc22_s %>%
  select(Sample, cluster22)

r_hcpc23_s <- res.hcpc23$data.clust %>% # 23 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster23 = clust) 

r_hcpc23 <- r_hcpc23_s %>%
  select(Sample, cluster23)

r_hcpc24_s <- res.hcpc24$data.clust %>% # 24 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster24 = clust) 

r_hcpc24 <- r_hcpc24_s %>%
  select(Sample, cluster24)

r_hcpc25_s <- res.hcpc25$data.clust %>% # 25 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster25 = clust) 

r_hcpc25 <- r_hcpc25_s %>%
  select(Sample, cluster25)

r_hcpc26_s <- res.hcpc26$data.clust %>% # 26 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster26 = clust) 

r_hcpc26 <- r_hcpc26_s %>%
  select(Sample, cluster26)

r_hcpc27_s <- res.hcpc27$data.clust %>% # 27 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster27 = clust) 

r_hcpc27 <- r_hcpc27_s %>%
  select(Sample, cluster27)

r_hcpc28_s <- res.hcpc28$data.clust %>% # 28 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster28 = clust) 

r_hcpc28 <- r_hcpc28_s %>%
  select(Sample, cluster28)

r_hcpc29_s <- res.hcpc29$data.clust %>% # 29 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster29 = clust) 

r_hcpc29 <- r_hcpc29_s %>%
  select(Sample, cluster29)

r_hcpc30_s <- res.hcpc30$data.clust %>% # 30 clusters
  rownames_to_column() %>% # change the row names into column names 
  rename(Sample = rowname, cluster30 = clust) 

r_hcpc30 <- r_hcpc30_s %>%
  select(Sample, cluster30)

####

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

r_hcpc_all <- left_join(r_hcpc_inter, r_hcpc10)

#### wait to be modified03 ####

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc11)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc12)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc13)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc14)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc15)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc16)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc17)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc18)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc19)

r_hcpc_all <- left_join(r_hcpc_inter, r_hcpc20)

#### 

# save the final dataset

write_csv(r_hcpc_all, "analysis/hcpc_ncp11_granular.csv")

# combine all the cluster results together to one single dataframe (scaled values)

r_hcpc_s_inter <- left_join(r_hcpc2_s, r_hcpc3)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc4)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc5)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc6)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc7)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc8)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc9)

r_hcpc_s_all <- left_join(r_hcpc_s_inter, r_hcpc10)

#### wait to be modified04 ####

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc11)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc12)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc13)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc14)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc15)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc16)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc17)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc18)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc19)

r_hcpc_s_all <- left_join(r_hcpc_s_inter, r_hcpc20)

####

# save the final dataset

write_csv(r_hcpc_s_all, "analysis/hcpc_scaled_ncp11_granular.csv")

# calculate the mean value of each factor

#### 2 clusters ####

# # original values
# 
# mean_2c <- r_hcpc_all %>%
#   group_by(cluster2) %>% # by 2 clusters
#   # summarise_if(is.numeric, mean, na.rm = TRUE) 
#   summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# # for the numerical column, calculate the mean, standard deviation and standard error
# 
# # creat two subsets for means and std errors
# 
# mean_2c_m <- mean_2c %>%
#   select(1:19) %>%
#   gather("factor", "mean", -cluster2) %>%
#   mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"
# 
# mean_2c_s <- mean_2c %>%
#   select(1, 38:55) %>%
#   gather("factor", "se", -cluster2) %>%
#   mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"
# 
# # combine the above two datasets together
# 
# mean_2c_full <- full_join(mean_2c_m, mean_2c_s)
# 
# # histogram (original values)
# 
# ncp9_c2 <- mean_2c_full %>%
#   ggplot(aes(x = cluster2, y = mean)) +
#   geom_errorbar(aes(x = cluster2,
#                     ymin = mean - se,
#                     ymax = mean + se),
#                 width=0.2,
#                 color = "red") +
#   geom_bar(stat = 'identity', width = 0.3) +
#   facet_wrap(~factor, scales = "free")
# 
# ggsave("figures/HCPC/ncp9/ncp9_c2_ori.png", 
#        plot = ncp9_c2, 
#        width = 30, 
#        height = 20, 
#        units = "cm") 


# scaled values

mean_2c <- r_hcpc_s_all %>%
  group_by(cluster2) %>% # by 2 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_2c_m <- mean_2c %>%
  select(1:24) %>%
  gather("factor", "mean", -cluster2) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_2c_s <- mean_2c %>%
  select(1, 50:73) %>%
  gather("factor", "se", -cluster2) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_2c_full <- full_join(mean_2c_m, mean_2c_s)

# add a new column to distinguish the explicative and illustratif factors 

mean_2c_full <- mean_2c_full %>%
  mutate(type = case_when(factor %in% c("h", "k", "Xinf", "Peak_Vis",
                                        "Trough_Vis", "Final_Vis", "Pasting_Temp",
                                        "residual") ~ "supplementary_variable",
                          TRUE ~ "active_variable"))

# histogram (scaled values)

ncp11_c2 <- mean_2c_full %>%
  ggplot(aes(x = factor, y = mean, fill = type)) +
  # scale_fill_manual(values = c("8888", "4444")) +
  geom_errorbar(aes(x = factor,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "dark grey") +
  geom_bar(stat = 'identity', width = 0.3) +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none") +
  facet_wrap(~cluster2, scales = "free")

ncp11_c2

ggsave("figures/HCPC/ncp9/ncp11_c2_scal_granular.png", 
       plot = ncp11_c2, 
       width = 30, 
       height = 20,  
       units = "cm") 

#### 3 clusters ####

# # original values
# 
# mean_3c <- r_hcpc_all %>%
#   group_by(cluster3) %>% # by 3 clusters
#   # summarise_if(is.numeric, mean, na.rm = TRUE) 
#   summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# # for the numerical column, calculate the mean, standard deviation and standard error
# 
# # creat two subsets for means and std errors
# 
# mean_3c_m <- mean_3c %>%
#   select(1:20) %>%
#   gather("factor", "mean", -cluster3) %>%
#   mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"
# 
# mean_3c_s <- mean_3c %>%
#   select(1, 40:58) %>%
#   gather("factor", "se", -cluster3) %>%
#   mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"
# 
# # combine the above two datasets together
# 
# mean_3c_full <- full_join(mean_3c_m, mean_3c_s)
# 
# # histogram (original values)
# 
# ncp9_c3 <- mean_3c_full %>%
#   ggplot(aes(x = cluster3, y = mean)) +
#   geom_errorbar(aes(x = cluster3,
#                     ymin = mean - se,
#                     ymax = mean + se),
#                 width=0.2,
#                 color = "red") +
#   geom_bar(stat = 'identity', width = 0.3) +
#   facet_wrap(~factor, scales = "free")
# 
# ggsave("figures/HCPC/ncp9/ncp9_c3_ori.png", 
#        plot = ncp9_c3, 
#        width = 30, 
#        height = 20, 
#        units = "cm") 

# scaled values

mean_3c <- r_hcpc_s_all %>%
  group_by(cluster3) %>% # by 3 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_3c_m <- mean_3c %>%
  select(1:24) %>%
  gather("factor", "mean", -cluster3) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_3c_s <- mean_3c %>%
  select(1, 50:73) %>%
  gather("factor", "se", -cluster3) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_3c_full <- full_join(mean_3c_m, mean_3c_s)

# add a new column to distinguish the explicative and illustratif factors 

mean_3c_full <- mean_3c_full %>%
  mutate(type = case_when(factor %in% c("h", "k", "Xinf", "Peak_Vis",
                                        "Trough_Vis", "Final_Vis", "Pasting_Temp",
                                        "residual") ~ "supplementary_variable",
                          TRUE ~ "active_variable"))

# histogram (scaled values)

ncp11_c3 <- mean_3c_full %>%
  ggplot(aes(x = factor, y = mean, fill = type)) +
  # scale_fill_manual(values = c("8888", "4444")) +
  geom_errorbar(aes(x = factor,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "dark grey") +
  geom_bar(stat = 'identity', width = 0.3) +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none") +
  facet_wrap(~cluster3, scales = "free")

ncp11_c3

ggsave("figures/HCPC/ncp9/ncp11_c3_scal_granular.png", 
       plot = ncp11_c3, 
       width = 30, 
       height = 20, 
       units = "cm") 

#### 4 clusters ####

# # original values
# 
# mean_4c <- r_hcpc_all %>%
#   group_by(cluster4) %>% # by 4 clusters
#   # summarise_if(is.numeric, mean, na.rm = TRUE) 
#   summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# # for the numerical column, calculate the mean, standard deviation and standard error
# 
# # creat two subsets for means and std errors
# 
# mean_4c_m <- mean_4c %>%
#   select(1:20) %>%
#   gather("factor", "mean", -cluster4) %>%
#   mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"
# 
# mean_4c_s <- mean_4c %>%
#   select(1, 40:58) %>%
#   gather("factor", "se", -cluster4) %>%
#   mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"
# 
# # combine the above two datasets together
# 
# mean_4c_full <- full_join(mean_4c_m, mean_4c_s)
# 
# # histogram (original values)
# 
# ncp9_c4 <- mean_4c_full %>%
#   ggplot(aes(x = cluster4, y = mean)) +
#   geom_errorbar(aes(x = cluster4,
#                     ymin = mean - se,
#                     ymax = mean + se),
#                 width=0.2,
#                 color = "red") +
#   geom_bar(stat = 'identity', width = 0.3) +
#   facet_wrap(~factor, scales = "free")
# 
# ggsave("figures/HCPC/ncp9/ncp9_c4_ori.png", 
#        plot = ncp9_c4, 
#        width = 30, 
#        height = 20, 
#        units = "cm") 

# scaled values

#### wait to be modified05 ####

mean_4c <- r_hcpc_s_all %>%
  group_by(cluster4) %>% # by 4 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_4c_m <- mean_4c %>%
  select(1:24) %>%
  gather("factor", "mean", -cluster4) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_4c_s <- mean_4c %>%
  select(1, 50:73) %>%
  gather("factor", "se", -cluster4) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_4c_full <- full_join(mean_4c_m, mean_4c_s)

# add a new column to distinguish the explicative and illustratif factors 

mean_4c_full <- mean_4c_full %>%
  mutate(type = case_when(factor %in% c("h", "k", "Xinf", "Peak_Vis",
                                        "Trough_Vis", "Final_Vis", "Pasting_Temp",
                                        "residual") ~ "supplementary_variable",
                          TRUE ~ "active_variable"))

# histogram (scaled values)

ncp9_c4 <- mean_4c_full %>%
  ggplot(aes(x = factor, y = mean, fill = type)) +
  # scale_fill_manual(values = c("8888", "4444")) +
  geom_errorbar(aes(x = factor,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "dark grey") +
  geom_bar(stat = 'identity', width = 0.3) +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none") +
  facet_wrap(~cluster4, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c4_scal.png", 
       plot = ncp9_c4, 
       width = 30, 
       height = 20,  
       units = "cm") 

#### 5 clusters ####

# # original values
# 
# mean_5c <- r_hcpc_all %>%
#   group_by(cluster5) %>% # by 5 clusters
#   # summarise_if(is.numeric, mean, na.rm = TRUE) 
#   summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# # for the numerical column, calculate the mean, standard deviation and standard error
# 
# # creat two subsets for means and std errors
# 
# mean_5c_m <- mean_5c %>%
#   select(1:24) %>%
#   gather("factor", "mean", -cluster5) %>%
#   mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"
# 
# mean_5c_s <- mean_5c %>%
#   select(1, 50:73) %>%
#   gather("factor", "se", -cluster5) %>%
#   mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"
# 
# # combine the above two datasets together
# 
# mean_5c_full <- full_join(mean_5c_m, mean_5c_s)
# 
# # histogram (original values)
# 
# ncp9_c5 <- mean_5c_full %>%
#   ggplot(aes(x = cluster5, y = mean)) +
#   geom_errorbar(aes(x = cluster5,
#                     ymin = mean - se,
#                     ymax = mean + se),
#                 width=0.2,
#                 color = "red") +
#   geom_bar(stat = 'identity', width = 0.3) +
#   facet_wrap(~factor, scales = "free")
# 
# ggsave("figures/HCPC/ncp9/ncp9_c5_ori.png", 
#        plot = ncp9_c5, 
#        width = 30, 
#        height = 20, 
#        units = "cm") 

# scaled values

mean_5c <- r_hcpc_s_all %>%
  group_by(cluster5) %>% # by 5 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_5c_m <- mean_5c %>%
  select(1:24) %>%
  gather("factor", "mean", -cluster5) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_5c_s <- mean_5c %>%
  select(1, 50:73) %>%
  gather("factor", "se", -cluster5) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_5c_full <- full_join(mean_5c_m, mean_5c_s)

# add a new column to distinguish the explicative and illustratif factors 

mean_5c_full <- mean_5c_full %>%
  mutate(type = case_when(factor %in% c("h", "k", "Xinf", "Peak_Vis",
                                        "Trough_Vis", "Final_Vis", "Pasting_Temp",
                                        "residual") ~ "supplementary_variable",
                          TRUE ~ "active_variable"))

# histogram (scaled values)

ncp9_c5 <- mean_5c_full %>%
  ggplot(aes(x = factor, y = mean, fill = type)) +
  # scale_fill_manual(values = c("8888", "4444")) +
  geom_errorbar(aes(x = factor,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "dark grey") +
  geom_bar(stat = 'identity', width = 0.3) +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none") +
  facet_wrap(~cluster5, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c5_scal.png", 
       plot = ncp9_c5, 
       width = 30, 
       height = 20,  
       units = "cm") 

#### 6 clusters ####

# # original values
# 
# mean_6c <- r_hcpc_all %>%
#   group_by(cluster6) %>% # by 6 clusters
#   # summarise_if(is.numeric, mean, na.rm = TRUE) 
#   summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# # for the numerical column, calculate the mean, standard deviation and standard error
# 
# # creat two subsets for means and std errors
# 
# mean_6c_m <- mean_6c %>%
#   select(1:19) %>%
#   gather("factor", "mean", -cluster6) %>%
#   mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"
# 
# mean_6c_s <- mean_6c %>%
#   select(1, 38:55) %>%
#   gather("factor", "se", -cluster6) %>%
#   mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"
# 
# # combine the above two datasets together
# 
# mean_6c_full <- full_join(mean_6c_m, mean_6c_s)
# 
# # histogram (original values)
# 
# ncp9_c6 <- mean_6c_full %>%
#   ggplot(aes(x = cluster6, y = mean)) +
#   geom_errorbar(aes(x = cluster6,
#                     ymin = mean - se,
#                     ymax = mean + se),
#                 width=0.2,
#                 color = "red") +
#   geom_bar(stat = 'identity', width = 0.3) +
#   facet_wrap(~factor, scales = "free")
# 
# ggsave("figures/HCPC/ncp9/ncp9_c6_ori.png", 
#        plot = ncp9_c6, 
#        width = 30, 
#        height = 20, 
#        units = "cm") 

# scaled values

mean_6c <- r_hcpc_s_all %>%
  group_by(cluster6) %>% # by 6 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_6c_m <- mean_6c %>%
  select(1:24) %>%
  gather("factor", "mean", -cluster6) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_6c_s <- mean_6c %>%
  select(1, 50:73) %>%
  gather("factor", "se", -cluster6) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_6c_full <- full_join(mean_6c_m, mean_6c_s)

# add a new column to distinguish the explicative and illustratif factors 

mean_6c_full <- mean_6c_full %>%
  mutate(type = case_when(factor %in% c("h", "k", "Xinf", "Peak_Vis",
                                        "Trough_Vis", "Final_Vis", "Pasting_Temp",
                                        "residual") ~ "supplementary_variable",
                          TRUE ~ "active_variable"))

# histogram (scaled values)

ncp9_c6 <- mean_6c_full %>%
  ggplot(aes(x = factor, y = mean, fill = type)) +
  # scale_fill_manual(values = c("8888", "4444")) +
  geom_errorbar(aes(x = factor,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "dark grey") +
  geom_bar(stat = 'identity', width = 0.3) +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none") +
  facet_wrap(~cluster6, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c6_scal.png", 
       plot = ncp9_c6, 
       width = 30, 
       height = 20, 
       units = "cm") 

#### 7 clusters ####

# # original values
# 
# mean_7c <- r_hcpc_all %>%
#   group_by(cluster7) %>% # by 7 clusters
#   # summarise_if(is.numeric, mean, na.rm = TRUE) 
#   summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# # for the numerical column, calculate the mean, standard deviation and standard error
# 
# # creat two subsets for means and std errors
# 
# mean_7c_m <- mean_7c %>%
#   select(1:19) %>%
#   gather("factor", "mean", -cluster7) %>%
#   mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"
# 
# mean_7c_s <- mean_7c %>%
#   select(1, 38:55) %>%
#   gather("factor", "se", -cluster7) %>%
#   mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"
# 
# # combine the above two datasets together
# 
# mean_7c_full <- full_join(mean_7c_m, mean_7c_s)
# 
# # histogram (original values)
# 
# ncp9_c7 <- mean_7c_full %>%
#   ggplot(aes(x = cluster7, y = mean)) +
#   geom_errorbar(aes(x = cluster7,
#                     ymin = mean - se,
#                     ymax = mean + se),
#                 width=0.2,
#                 color = "red") +
#   geom_bar(stat = 'identity', width = 0.3) +
#   facet_wrap(~factor, scales = "free")
# 
# ggsave("figures/HCPC/ncp9/ncp9_c7_ori.png", 
#        plot = ncp9_c7, 
#        width = 30, 
#        height = 20, 
#        units = "cm") 

# scaled values

mean_7c <- r_hcpc_s_all %>%
  group_by(cluster7) %>% # by 7 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_7c_m <- mean_7c %>%
  select(1:24) %>%
  gather("factor", "mean", -cluster7) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_7c_s <- mean_7c %>%
  select(1, 50:73) %>%
  gather("factor", "se", -cluster7) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_7c_full <- full_join(mean_7c_m, mean_7c_s)

# add a new column to distinguish the explicative and illustratif factors 

mean_7c_full <- mean_7c_full %>%
  mutate(type = case_when(factor %in% c("h", "k", "Xinf", "Peak_Vis",
                                        "Trough_Vis", "Final_Vis", "Pasting_Temp",
                                        "residual") ~ "supplementary_variable",
                          TRUE ~ "active_variable"))

# histogram (scaled values)

ncp9_c7 <- mean_7c_full %>%
  ggplot(aes(x = factor, y = mean, fill = type)) +
  # scale_fill_manual(values = c("8888", "4444")) +
  geom_errorbar(aes(x = factor,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "dark grey") +
  geom_bar(stat = 'identity', width = 0.3) +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none") +
  facet_wrap(~cluster7, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c7_scal.png", 
       plot = ncp9_c7, 
       width = 30, 
       height = 20,  
       units = "cm") 

#### 8 clusters ####

# # original values
# 
# mean_8c <- r_hcpc_all %>%
#   group_by(cluster8) %>% # by 8 clusters
#   # summarise_if(is.numeric, mean, na.rm = TRUE) 
#   summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# # for the numerical column, calculate the mean, standard deviation and standard error
# 
# # creat two subsets for means and std errors
# 
# mean_8c_m <- mean_8c %>%
#   select(1:19) %>%
#   gather("factor", "mean", -cluster8) %>%
#   mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"
# 
# mean_8c_s <- mean_8c %>%
#   select(1, 38:55) %>%
#   gather("factor", "se", -cluster8) %>%
#   mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"
# 
# # combine the above two datasets together
# 
# mean_8c_full <- full_join(mean_8c_m, mean_8c_s)
# 
# # histogram (original values)
# 
# ncp9_c8 <- mean_8c_full %>%
#   ggplot(aes(x = cluster8, y = mean)) +
#   geom_errorbar(aes(x = cluster8,
#                     ymin = mean - se,
#                     ymax = mean + se),
#                 width=0.2,
#                 color = "red") +
#   geom_bar(stat = 'identity', width = 0.3) +
#   facet_wrap(~factor, scales = "free")
# 
# ggsave("figures/HCPC/ncp9/ncp9_c8_ori.png", 
#        plot = ncp9_c8, 
#        width = 30, 
#        height = 20, 
#        units = "cm") 

# scaled values

mean_8c <- r_hcpc_s_all %>%
  group_by(cluster8) %>% # by 8 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_8c_m <- mean_8c %>%
  select(1:24) %>%
  gather("factor", "mean", -cluster8) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_8c_s <- mean_8c %>%
  select(1, 50:73) %>%
  gather("factor", "se", -cluster8) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_8c_full <- full_join(mean_8c_m, mean_8c_s)

# add a new column to distinguish the explicative and illustratif factors 

mean_8c_full <- mean_8c_full %>%
  mutate(type = case_when(factor %in% c("h", "k", "Xinf", "Peak_Vis",
                                        "Trough_Vis", "Final_Vis", "Pasting_Temp",
                                        "residual") ~ "supplementary_variable",
                          TRUE ~ "active_variable"))

# histogram (scaled values)

ncp9_c8 <- mean_8c_full %>%
  ggplot(aes(x = factor, y = mean, fill = type)) +
  # scale_fill_manual(values = c("8888", "4444")) +
  geom_errorbar(aes(x = factor,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "dark grey") +
  geom_bar(stat = 'identity', width = 0.3) +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none") +
  facet_wrap(~cluster8, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c8_scal.png", 
       plot = ncp9_c8, 
       width = 30, 
       height = 20,  
       units = "cm") 

#### 9 clusters ####

# original values

# mean_9c <- r_hcpc_all %>%
#   group_by(cluster9) %>% # by 9 clusters
#   # summarise_if(is.numeric, mean, na.rm = TRUE) 
#   summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# # for the numerical column, calculate the mean, standard deviation and standard error
# 
# # creat two subsets for means and std errors
# 
# mean_9c_m <- mean_9c %>%
#   select(1:19) %>%
#   gather("factor", "mean", -cluster9) %>%
#   mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"
# 
# mean_9c_s <- mean_9c %>%
#   select(1, 38:55) %>%
#   gather("factor", "se", -cluster9) %>%
#   mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"
# 
# # combine the above two datasets together
# 
# mean_9c_full <- full_join(mean_9c_m, mean_9c_s)
# 
# # histogram (original values)
# 
# ncp9_c9 <- mean_9c_full %>%
#   ggplot(aes(x = cluster9, y = mean)) +
#   geom_errorbar(aes(x = cluster9,
#                     ymin = mean - se,
#                     ymax = mean + se),
#                 width=0.2,
#                 color = "red") +
#   geom_bar(stat = 'identity', width = 0.3) +
#   facet_wrap(~factor, scales = "free")
# 
# ggsave("figures/HCPC/ncp9/ncp9_c9_ori.png", 
#        plot = ncp9_c9, 
#        width = 30, 
#        height = 20, 
#        units = "cm") 

# scaled values

mean_9c <- r_hcpc_s_all %>%
  group_by(cluster9) %>% # by 9 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_9c_m <- mean_9c %>%
  select(1:24) %>%
  gather("factor", "mean", -cluster9) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_9c_s <- mean_9c %>%
  select(1, 50:73) %>%
  gather("factor", "se", -cluster9) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_9c_full <- full_join(mean_9c_m, mean_9c_s)

# add a new column to distinguish the explicative and illustratif factors 

mean_9c_full <- mean_9c_full %>%
  mutate(type = case_when(factor %in% c("h", "k", "Xinf", "Peak_Vis",
                                        "Trough_Vis", "Final_Vis", "Pasting_Temp",
                                        "residual") ~ "supplementary_variable",
                          TRUE ~ "active_variable"))

# histogram (scaled values)

ncp9_c9 <- mean_9c_full %>%
  ggplot(aes(x = factor, y = mean, fill = type)) +
  # scale_fill_manual(values = c("8888", "4444")) +
  geom_errorbar(aes(x = factor,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "dark grey") +
  geom_bar(stat = 'identity', width = 0.3) +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none") +
  facet_wrap(~cluster9, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c9_scal.png", 
       plot = ncp9_c9, 
       width = 30, 
       height = 20,  
       units = "cm") 

#### 10 clusters ####

# # original values
# 
# mean_10c <- r_hcpc_all %>%
#   group_by(cluster10) %>% # by 10 clusters
#   # summarise_if(is.numeric, mean, na.rm = TRUE) 
#   summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# # for the numerical column, calculate the mean, standard deviation and standard error
# 
# # creat two subsets for means and std errors
# 
# mean_10c_m <- mean_10c %>%
#   select(1:19) %>%
#   gather("factor", "mean", -cluster10) %>%
#   mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"
# 
# mean_10c_s <- mean_10c %>%
#   select(1, 38:55) %>%
#   gather("factor", "se", -cluster10) %>%
#   mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"
# 
# # combine the above two datasets together
# 
# mean_10c_full <- full_join(mean_10c_m, mean_10c_s)
# 
# # histogram (original values)
# 
# ncp9_c10 <- mean_10c_full %>%
#   ggplot(aes(x = cluster10, y = mean)) +
#   geom_errorbar(aes(x = cluster10,
#                     ymin = mean - se,
#                     ymax = mean + se),
#                 width=0.2,
#                 color = "red") +
#   geom_bar(stat = 'identity', width = 0.3) +
#   facet_wrap(~factor, scales = "free")
# 
# ggsave("figures/HCPC/ncp9/ncp9_c10_ori.png", 
#        plot = ncp9_c10, 
#        width = 30, 
#        height = 20, 
#        units = "cm") 

# scaled values

mean_10c <- r_hcpc_s_all %>%
  group_by(cluster10) %>% # by 10 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_10c_m <- mean_10c %>%
  select(1:24) %>%
  gather("factor", "mean", -cluster10) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_10c_s <- mean_10c %>%
  select(1, 50:73) %>%
  gather("factor", "se", -cluster10) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_10c_full <- full_join(mean_10c_m, mean_10c_s)

# add a new column to distinguish the explicative and illustratif factors 

mean_10c_full <- mean_10c_full %>%
  mutate(type = case_when(factor %in% c("h", "k", "Xinf", "Peak_Vis",
                                        "Trough_Vis", "Final_Vis", "Pasting_Temp",
                                        "residual") ~ "supplementary_variable",
                          TRUE ~ "active_variable"))

# histogram (scaled values)

ncp9_c10 <- mean_10c_full %>%
  ggplot(aes(x = factor, y = mean, fill = type)) +
  # scale_fill_manual(values = c("8888", "4444")) +
  geom_errorbar(aes(x = factor,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "dark grey") +
  geom_bar(stat = 'identity', width = 0.3) +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none") +
  facet_wrap(~cluster10, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c10_scal.png", 
       plot = ncp9_c10, 
       width = 30, 
       height = 20, 
       units = "cm") 

#### 11 clusters ####

# # original values
# 
# mean_11c <- r_hcpc_all %>%
#   group_by(cluster11) %>% # by 11 clusters
#   # summarise_if(is.numeric, mean, na.rm = TRUE) 
#   summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# # for the numerical column, calculate the mean, standard deviation and standard error
# 
# # creat two subsets for means and std errors
# 
# mean_11c_m <- mean_11c %>%
#   select(1:19) %>%
#   gather("factor", "mean", -cluster11) %>%
#   mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"
# 
# mean_11c_s <- mean_11c %>%
#   select(1, 38:55) %>%
#   gather("factor", "se", -cluster11) %>%
#   mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"
# 
# # combine the above two datasets together
# 
# mean_11c_full <- full_join(mean_11c_m, mean_11c_s)
# 
# # histogram (original values)
# 
# ncp9_c11 <- mean_11c_full %>%
#   ggplot(aes(x = cluster11, y = mean)) +
#   geom_errorbar(aes(x = cluster11,
#                     ymin = mean - se,
#                     ymax = mean + se),
#                 width=0.2,
#                 color = "red") +
#   geom_bar(stat = 'identity', width = 0.3) +
#   facet_wrap(~factor, scales = "free")
# 
# ggsave("figures/HCPC/ncp9/ncp9_c11_ori.png", 
#        plot = ncp9_c11, 
#        width = 30, 
#        height = 20,  
#        units = "cm") 

# scaled values

mean_11c <- r_hcpc_s_all %>%
  group_by(cluster11) %>% # by 11 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_11c_m <- mean_11c %>%
  select(1:24) %>%
  gather("factor", "mean", -cluster11) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_11c_s <- mean_11c %>%
  select(1, 50:73) %>%
  gather("factor", "se", -cluster11) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_11c_full <- full_join(mean_11c_m, mean_11c_s)

# add a new column to distinguish the explicative and illustratif factors 

mean_11c_full <- mean_11c_full %>%
  mutate(type = case_when(factor %in% c("h", "k", "Xinf", "Peak_Vis",
                                        "Trough_Vis", "Final_Vis", "Pasting_Temp",
                                        "residual") ~ "supplementary_variable",
                          TRUE ~ "active_variable"))

# histogram (scaled values)

ncp9_c11 <- mean_11c_full %>%
  ggplot(aes(x = factor, y = mean, fill = type)) +
  # scale_fill_manual(values = c("8888", "4444")) +
  geom_errorbar(aes(x = factor,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "dark grey") +
  geom_bar(stat = 'identity', width = 0.3) +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none") +
  facet_wrap(~cluster11, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c11_scal.png", 
       plot = ncp9_c11, 
       width = 30, 
       height = 20, 
       units = "cm") 

#### 12 clusters ####

# # original values
# 
# mean_12c <- r_hcpc_all %>%
#   group_by(cluster12) %>% # by 12 clusters
#   # summarise_if(is.numeric, mean, na.rm = TRUE) 
#   summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# # for the numerical column, calculate the mean, standard deviation and standard error
# 
# # creat two subsets for means and std errors
# 
# mean_12c_m <- mean_12c %>%
#   select(1:19) %>%
#   gather("factor", "mean", -cluster12) %>%
#   mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"
# 
# mean_12c_s <- mean_12c %>%
#   select(1, 38:55) %>%
#   gather("factor", "se", -cluster12) %>%
#   mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"
# 
# # combine the above two datasets together
# 
# mean_12c_full <- full_join(mean_12c_m, mean_12c_s)
# 
# # histogram (original values)
# 
# ncp9_c12 <- mean_12c_full %>%
#   ggplot(aes(x = cluster12, y = mean)) +
#   geom_errorbar(aes(x = cluster12,
#                     ymin = mean - se,
#                     ymax = mean + se),
#                 width=0.2,
#                 color = "red") +
#   geom_bar(stat = 'identity', width = 0.3) +
#   facet_wrap(~factor, scales = "free")
# 
# ggsave("figures/HCPC/ncp9/ncp9_c12_ori.png", 
#        plot = ncp9_c12, 
#        width = 30, 
#        height = 20, 
#        units = "cm") 

# scaled values

mean_12c <- r_hcpc_s_all %>%
  group_by(cluster12) %>% # by 12 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_12c_m <- mean_12c %>%
  select(1:24) %>%
  gather("factor", "mean", -cluster12) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_12c_s <- mean_12c %>%
  select(1, 50:73) %>%
  gather("factor", "se", -cluster12) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_12c_full <- full_join(mean_12c_m, mean_12c_s)

# add a new column to distinguish the explicative and illustratif factors 

mean_12c_full <- mean_12c_full %>%
  mutate(type = case_when(factor %in% c("h", "k", "Xinf", "Peak_Vis",
                                        "Trough_Vis", "Final_Vis", "Pasting_Temp",
                                        "residual") ~ "supplementary_variable",
                          TRUE ~ "active_variable"))

# histogram (scaled values)

ncp9_c12 <- mean_12c_full %>%
  ggplot(aes(x = factor, y = mean, fill = type)) +
  # scale_fill_manual(values = c("8888", "4444")) +
  geom_errorbar(aes(x = factor,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "dark grey") +
  geom_bar(stat = 'identity', width = 0.3) +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none") +
  facet_wrap(~cluster12, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c12_scal.png", 
       plot = ncp9_c12, 
       width = 30, 
       height = 20, 
       units = "cm") 

#### 13 clusters ####

# # original values
# 
# mean_13c <- r_hcpc_all %>%
#   group_by(cluster13) %>% # by 13 clusters
#   # summarise_if(is.numeric, mean, na.rm = TRUE) 
#   summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# # for the numerical column, calculate the mean, standard deviation and standard error
# 
# # creat two subsets for means and std errors
# 
# mean_13c_m <- mean_13c %>%
#   select(1:19) %>%
#   gather("factor", "mean", -cluster13) %>%
#   mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"
# 
# mean_13c_s <- mean_13c %>%
#   select(1, 38:55) %>%
#   gather("factor", "se", -cluster13) %>%
#   mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"
# 
# # combine the above two datasets together
# 
# mean_13c_full <- full_join(mean_13c_m, mean_13c_s)
# 
# # histogram (original values)
# 
# ncp9_c13 <- mean_13c_full %>%
#   ggplot(aes(x = cluster13, y = mean)) +
#   geom_errorbar(aes(x = cluster13,
#                     ymin = mean - se,
#                     ymax = mean + se),
#                 width=0.2,
#                 color = "red") +
#   geom_bar(stat = 'identity', width = 0.3) +
#   facet_wrap(~factor, scales = "free")
# 
# ggsave("figures/HCPC/ncp9/ncp9_c13_ori.png", 
#        plot = ncp9_c13, 
#        width = 30, 
#        height = 20, 
#        units = "cm") 

# scaled values

mean_13c <- r_hcpc_s_all %>%
  group_by(cluster13) %>% # by 13 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_13c_m <- mean_13c %>%
  select(1:24) %>%
  gather("factor", "mean", -cluster13) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_13c_s <- mean_13c %>%
  select(1, 50:73) %>%
  gather("factor", "se", -cluster13) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_13c_full <- full_join(mean_13c_m, mean_13c_s)

# add a new column to distinguish the explicative and illustratif factors 

mean_13c_full <- mean_13c_full %>%
  mutate(type = case_when(factor %in% c("h", "k", "Xinf", "Peak_Vis",
                                        "Trough_Vis", "Final_Vis", "Pasting_Temp",
                                        "residual") ~ "supplementary_variable",
                          TRUE ~ "active_variable"))

# histogram (scaled values)

ncp9_c13 <- mean_13c_full %>%
  ggplot(aes(x = factor, y = mean, fill = type)) +
  # scale_fill_manual(values = c("8888", "4444")) +
  geom_errorbar(aes(x = factor,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "dark grey") +
  geom_bar(stat = 'identity', width = 0.3) +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none") +
  facet_wrap(~cluster13, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c13_scal.png", 
       plot = ncp9_c13, 
       width = 30, 
       height = 20, 
       units = "cm") 

#### 14 clusters ####

# # original values
# 
# mean_14c <- r_hcpc_all %>%
#   group_by(cluster14) %>% # by 14 clusters
#   # summarise_if(is.numeric, mean, na.rm = TRUE) 
#   summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# # for the numerical column, calculate the mean, standard deviation and standard error
# 
# # creat two subsets for means and std errors
# 
# mean_14c_m <- mean_14c %>%
#   select(1:19) %>%
#   gather("factor", "mean", -cluster14) %>%
#   mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"
# 
# mean_14c_s <- mean_14c %>%
#   select(1, 38:55) %>%
#   gather("factor", "se", -cluster14) %>%
#   mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"
# 
# # combine the above two datasets together
# 
# mean_14c_full <- full_join(mean_14c_m, mean_14c_s)
# 
# # histogram (original values)
# 
# ncp9_c14 <- mean_14c_full %>%
#   ggplot(aes(x = cluster14, y = mean)) +
#   geom_errorbar(aes(x = cluster14,
#                     ymin = mean - se,
#                     ymax = mean + se),
#                 width=0.2,
#                 color = "red") +
#   geom_bar(stat = 'identity', width = 0.3) +
#   facet_wrap(~factor, scales = "free")
# 
# ggsave("figures/HCPC/ncp9/ncp9_c14_ori.png", 
#        plot = ncp9_c14, 
#        width = 30, 
#        height = 20, 
#        units = "cm") 

# scaled values

mean_14c <- r_hcpc_s_all %>%
  group_by(cluster14) %>% # by 14 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_14c_m <- mean_14c %>%
  select(1:24) %>%
  gather("factor", "mean", -cluster14) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_14c_s <- mean_14c %>%
  select(1, 50:73) %>%
  gather("factor", "se", -cluster14) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_14c_full <- full_join(mean_14c_m, mean_14c_s)

# add a new column to distinguish the explicative and illustratif factors 

mean_14c_full <- mean_14c_full %>%
  mutate(type = case_when(factor %in% c("h", "k", "Xinf", "Peak_Vis",
                                        "Trough_Vis", "Final_Vis", "Pasting_Temp",
                                        "residual") ~ "supplementary_variable",
                          TRUE ~ "active_variable"))

# histogram (scaled values)

ncp9_c14 <- mean_14c_full %>%
  ggplot(aes(x = factor, y = mean, fill = type)) +
  # scale_fill_manual(values = c("8888", "4444")) +
  geom_errorbar(aes(x = factor,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "dark grey") +
  geom_bar(stat = 'identity', width = 0.3) +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none") +
  facet_wrap(~cluster14, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c14_scal.png", 
       plot = ncp9_c14, 
       width = 30, 
       height = 20, 
       units = "cm") 

#### 15 clusters ####

# scaled values

mean_15c <- r_hcpc_s_all %>%
  group_by(cluster15) %>% # by 15 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_15c_m <- mean_15c %>%
  select(1:24) %>%
  gather("factor", "mean", -cluster15) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_15c_s <- mean_15c %>%
  select(1, 50:73) %>%
  gather("factor", "se", -cluster15) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_15c_full <- full_join(mean_15c_m, mean_15c_s)

# add a new column to distinguish the explicative and illustratif factors 

mean_15c_full <- mean_15c_full %>%
  mutate(type = case_when(factor %in% c("h", "k", "Xinf", "Peak_Vis",
                                        "Trough_Vis", "Final_Vis", "Pasting_Temp",
                                        "residual") ~ "supplementary_variable",
                          TRUE ~ "active_variable"))

# histogram (scaled values)

ncp9_c15 <- mean_15c_full %>%
  ggplot(aes(x = factor, y = mean, fill = type)) +
  # scale_fill_manual(values = c("8888", "4444")) +
  geom_errorbar(aes(x = factor,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "dark grey") +
  geom_bar(stat = 'identity', width = 0.3) +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none") +
  facet_wrap(~cluster15, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c15_scal.png", 
       plot = ncp9_c15, 
       width = 30, 
       height = 20, 
       units = "cm") 

#### 16 clusters ####

# scaled values

mean_16c <- r_hcpc_s_all %>%
  group_by(cluster16) %>% # by 16 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_16c_m <- mean_16c %>%
  select(1:24) %>%
  gather("factor", "mean", -cluster16) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_16c_s <- mean_16c %>%
  select(1, 50:73) %>%
  gather("factor", "se", -cluster16) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_16c_full <- full_join(mean_16c_m, mean_16c_s)

# add a new column to distinguish the explicative and illustratif factors 

mean_16c_full <- mean_16c_full %>%
  mutate(type = case_when(factor %in% c("h", "k", "Xinf", "Peak_Vis",
                                        "Trough_Vis", "Final_Vis", "Pasting_Temp",
                                        "residual") ~ "supplementary_variable",
                          TRUE ~ "active_variable"))

# histogram (scaled values)

ncp9_c16 <- mean_16c_full %>%
  ggplot(aes(x = factor, y = mean, fill = type)) +
  # scale_fill_manual(values = c("8888", "4444")) +
  geom_errorbar(aes(x = factor,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "dark grey") +
  geom_bar(stat = 'identity', width = 0.3) +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none") +
  facet_wrap(~cluster16, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c16_scal.png", 
       plot = ncp9_c16, 
       width = 30, 
       height = 20, 
       units = "cm") 

#### 17 clusters ####

# scaled values

mean_17c <- r_hcpc_s_all %>%
  group_by(cluster17) %>% # by 17 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_17c_m <- mean_17c %>%
  select(1:24) %>%
  gather("factor", "mean", -cluster17) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_17c_s <- mean_17c %>%
  select(1, 50:73) %>%
  gather("factor", "se", -cluster17) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_17c_full <- full_join(mean_17c_m, mean_17c_s)

# add a new column to distinguish the explicative and illustratif factors 

mean_17c_full <- mean_17c_full %>%
  mutate(type = case_when(factor %in% c("h", "k", "Xinf", "Peak_Vis",
                                        "Trough_Vis", "Final_Vis", "Pasting_Temp",
                                        "residual") ~ "supplementary_variable",
                          TRUE ~ "active_variable"))

# histogram (scaled values)

ncp9_c17 <- mean_17c_full %>%
  ggplot(aes(x = factor, y = mean, fill = type)) +
  # scale_fill_manual(values = c("8888", "4444")) +
  geom_errorbar(aes(x = factor,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "dark grey") +
  geom_bar(stat = 'identity', width = 0.3) +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none") +
  facet_wrap(~cluster17, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c17_scal.png", 
       plot = ncp9_c17, 
       width = 30, 
       height = 20, 
       units = "cm") 

#### 18 clusters ####

# scaled values

mean_18c <- r_hcpc_s_all %>%
  group_by(cluster18) %>% # by 18 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_18c_m <- mean_18c %>%
  select(1:24) %>%
  gather("factor", "mean", -cluster18) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_18c_s <- mean_18c %>%
  select(1, 50:73) %>%
  gather("factor", "se", -cluster18) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_18c_full <- full_join(mean_18c_m, mean_18c_s)

# add a new column to distinguish the explicative and illustratif factors 

mean_18c_full <- mean_18c_full %>%
  mutate(type = case_when(factor %in% c("h", "k", "Xinf", "Peak_Vis",
                                        "Trough_Vis", "Final_Vis", "Pasting_Temp",
                                        "residual") ~ "supplementary_variable",
                          TRUE ~ "active_variable"))

# histogram (scaled values)

ncp9_c18 <- mean_18c_full %>%
  ggplot(aes(x = factor, y = mean, fill = type)) +
  # scale_fill_manual(values = c("8888", "4444")) +
  geom_errorbar(aes(x = factor,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "dark grey") +
  geom_bar(stat = 'identity', width = 0.3) +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none") +
  facet_wrap(~cluster18, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c18_scal.png", 
       plot = ncp9_c18, 
       width = 30, 
       height = 20, 
       units = "cm") 

#### 19 clusters ####

# scaled values

mean_19c <- r_hcpc_s_all %>%
  group_by(cluster19) %>% # by 19 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_19c_m <- mean_19c %>%
  select(1:24) %>%
  gather("factor", "mean", -cluster19) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_19c_s <- mean_19c %>%
  select(1, 50:73) %>%
  gather("factor", "se", -cluster19) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_19c_full <- full_join(mean_19c_m, mean_19c_s)

# add a new column to distinguish the explicative and illustratif factors 

mean_19c_full <- mean_19c_full %>%
  mutate(type = case_when(factor %in% c("h", "k", "Xinf", "Peak_Vis",
                                        "Trough_Vis", "Final_Vis", "Pasting_Temp",
                                        "residual") ~ "supplementary_variable",
                          TRUE ~ "active_variable"))

# histogram (scaled values)

ncp9_c19 <- mean_19c_full %>%
  ggplot(aes(x = factor, y = mean, fill = type)) +
  # scale_fill_manual(values = c("8888", "4444")) +
  geom_errorbar(aes(x = factor,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "dark grey") +
  geom_bar(stat = 'identity', width = 0.3) +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none") +
  facet_wrap(~cluster19, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c19_scal.png", 
       plot = ncp9_c19, 
       width = 30, 
       height = 20, 
       units = "cm") 

#### 20 clusters ####

# scaled values

mean_20c <- r_hcpc_s_all %>%
  group_by(cluster20) %>% # by 20 clusters
  # summarise_if(is.numeric, mean, na.rm = TRUE) 
  summarise_if(is.numeric, funs(mean, sd, se=sd(.)/sqrt(n()))) 
# for the numerical column, calculate the mean, standard deviation and standard error

# creat two subsets for means and std errors

mean_20c_m <- mean_20c %>%
  select(1:24) %>%
  gather("factor", "mean", -cluster20) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_20c_s <- mean_20c %>%
  select(1, 50:73) %>%
  gather("factor", "se", -cluster20) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_20c_full <- full_join(mean_20c_m, mean_20c_s)

# add a new column to distinguish the explicative and illustratif factors 

mean_20c_full <- mean_20c_full %>%
  mutate(type = case_when(factor %in% c("h", "k", "Xinf", "Peak_Vis",
                                        "Trough_Vis", "Final_Vis", "Pasting_Temp",
                                        "residual") ~ "supplementary_variable",
                          TRUE ~ "active_variable"))

# histogram (scaled values)

ncp9_c20 <- mean_20c_full %>%
  ggplot(aes(x = factor, y = mean, fill = type)) +
  # scale_fill_manual(values = c("8888", "4444")) +
  geom_errorbar(aes(x = factor,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "dark grey") +
  geom_bar(stat = 'identity', width = 0.3) +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none") +
  facet_wrap(~cluster20, scales = "free")

ggsave("figures/HCPC/ncp9/ncp9_c20_scal.png", 
       plot = ncp9_c20, 
       width = 30, 
       height = 20, 
       units = "cm") 

####

#### count for the number of samples in each cluster ####

count <- r_hcpc_s_all %>% 
  select(cluster2:cluster10) %>%
  gather(nbr_cluster, values) %>%
  group_by(nbr_cluster, values) %>%
  count()

write_csv(count, "analysis/ncp11/count_individual_granular.csv")

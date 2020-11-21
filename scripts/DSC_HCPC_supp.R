
library(tidyverse)

library(FactoMineR)

library(factoextra)

# import the dataset

df <- read_csv("analysis/total_new_convert_DSC.csv") %>%
  na.omit()

# add the residuals of the h k model

res <- read_csv("analysis//hkmodel_residuals.csv")

df <- left_join(df, res) %>%
  rename(residual = .resid)

# calculate the mean value of the kinetics 

df1 <- df %>%
  select(11, 34:36, 47) %>%
  group_by(Sample) %>%
  summarise(k = mean(k), h = mean(h), Xinf = mean(Xinf), residual = mean(residual)) 

# select the structural properties (all, including the parameters that we are going to put as supplimentary variables)

df2 <- df %>%
  select(1:11, 15:24, 37:46) %>% 
  unique() 

# combine and scale

df_new <- full_join(df2, df1) %>%
  select(Sample, pi_A, pi_B, pi_C, pi_AB_ratio, mu_A, mu_B, mu_C,
         sigma_A, sigma_B, sigma_C, DP6_12, DP13_24, DP25_36, DP37_47, 
         Amylose_Con, Amylase_Act, To1, Tp1, Tc1, Range1, H1, To2, Tp2, Tc2, Range2, H2,
         Peak_Vis, Trough_Vis, Final_Vis, Pasting_Temp, k, h, Xinf, residual) %>%
  remove_rownames() %>%
  column_to_rownames(var = 'Sample') %>%
  scale()

#### ncp=11 #### 

# compute PCA

res.pca <- PCA(df_new, 
               ncp = 11, # explain more than 99% of the variability 
               scale.unit = TRUE, 
               quanti.sup = 17:34, # 18 supplementary variables (DSC, RVAs and kinetics); 16 active variables 
               graph = FALSE)

summary(res.pca)

# Contributions (variable) to the PCs 

res.var <- get_pca_var(res.pca)

df3 <- res.var$contrib %>%
  as.data.frame()

df3

# save the contributions

write_csv(df3, "analysis/DSC_ncp11_supp/contributions_variable_supp.csv")

# Contributions (individual) to the PCs 

res.ind <- get_pca_ind(res.pca)

df4 <- res.ind$contrib %>%
  as.data.frame()

df4

# save the contributions

write_csv(df4, "analysis/DSC_ncp11_supp/contributions_ind_supp.csv")

# Extract eigenvalues/variances

eig <- get_eig(res.pca) %>%
  as_tibble() %>%
  rownames_to_column() %>%
  rename(dimension = rowname) %>%
  mutate_if(is.numeric, round, digits = 1)

eig


# scree plot

fviz_eig(res.pca, choice = "variance", addlabels = TRUE)

# cumulative scree plot

sp <- ggplot(data = eig, 
             aes(x = reorder(dimension, cumulative.variance.percent),
                 y = cumulative.variance.percent,
                 group = 1)) +
  geom_point(size = 1.5) +
  geom_line() +
  geom_bar(stat = 'identity', width = 0.7, fill = "#4682B4") +
  scale_x_discrete(limits = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)) +
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

ggsave("figures/cumulative_screeplot_DSC_supp.png", 
       plot = sp,
       width = 25, 
       height = 20, 
       units = "cm") 

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

# HCPC 

# globle HCPC

res.hcpc <- HCPC(res.pca, graph = TRUE, 
                 consol = TRUE)

# res.hcpc <- HCPC(res.pca, graph = FALSE)
# 
# fviz_dend(res.hcpc, k =100)

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

write_csv(hcpc2, "analysis/DSC_ncp11_supp/hcpc2_DSC.csv")

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

write_csv(hcpc3, "analysis/DSC_ncp11_supp/hcpc3_DSC.csv")

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

write_csv(hcpc4, "analysis/DSC_ncp11_supp/hcpc4_DSC.csv")

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

# save the results

write_csv(hcpc5, "analysis/DSC_ncp11_supp/hcpc5_DSC.csv")

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

# save the results

write_csv(hcpc6, "analysis/DSC_ncp11_supp/hcpc6_DSC.csv")

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

# save the results

write_csv(hcpc7, "analysis/DSC_ncp11_supp/hcpc7_DSC.csv")

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

# save the results

write_csv(hcpc8, "analysis/DSC_ncp11_supp/hcpc8_DSC.csv")

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

# save the results

write_csv(hcpc9, "analysis/DSC_ncp11_supp/hcpc9_DSC.csv")

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

# save the results

write_csv(hcpc10, "analysis/DSC_ncp11_supp/hcpc10_DSC.csv")

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

# save the results

write_csv(hcpc11, "analysis/DSC_ncp11_supp/hcpc11_DSC.csv")

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

# save the results

write_csv(hcpc12, "analysis/DSC_ncp11_supp/hcpc12_DSC.csv")

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

# save the results

write_csv(hcpc13, "analysis/DSC_ncp11_supp/hcpc13_DSC.csv")

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

# save the results

write_csv(hcpc14, "analysis/DSC_ncp11_supp/hcpc14_DSC.csv")

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

# save the results

write_csv(hcpc15, "analysis/DSC_ncp11_supp/hcpc15_DSC.csv")

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

# save the results

write_csv(hcpc16, "analysis/DSC_ncp11_supp/hcpc16_DSC.csv")

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

# save the results

write_csv(hcpc17, "analysis/DSC_ncp11_supp/hcpc17_DSC.csv")

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

# save the results

write_csv(hcpc18, "analysis/DSC_ncp11_supp/hcpc18_DSC.csv")

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

# save the results

write_csv(hcpc19, "analysis/DSC_ncp11_supp/hcpc19_DSC.csv")

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

# save the results

write_csv(hcpc20, "analysis/DSC_ncp11_supp/hcpc20_DSC.csv")

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

hcpc21[is.na(hcpc21)] = c("DP37_47", "DP13_24")

# save the results

write_csv(hcpc21, "analysis/DSC_ncp11_supp/hcpc21_DSC.csv")

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

hcpc22[is.na(hcpc22)] = c("DP37_47", "DP13_24")

# save the results

write_csv(hcpc22, "analysis/DSC_ncp11_supp/hcpc22_DSC.csv")

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

hcpc23[is.na(hcpc23)] = c("DP37_47", "DP13_24")

# save the results

write_csv(hcpc23, "analysis/DSC_ncp11_supp/hcpc23_DSC.csv")

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

# replace the NAs 

hcpc24[is.na(hcpc24)] = c("DP37_47", "DP13_24")

# save the results

write_csv(hcpc24, "analysis/DSC_ncp11_supp/hcpc24_DSC.csv")

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

# replace the NAs 

hcpc25[is.na(hcpc25)] = c("DP37_47", "DP13_24")

# save the results

write_csv(hcpc25, "analysis/DSC_ncp11_supp/hcpc25_DSC.csv")

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

# replace the NAs 

hcpc26[is.na(hcpc26)] = c("DP37_47", "DP13_24")

# save the results

write_csv(hcpc26, "analysis/DSC_ncp11_supp/hcpc26_DSC.csv")

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

hcpc27[is.na(hcpc27)] = c("DP37_47", "DP13_24")

# save the results

write_csv(hcpc27, "analysis/DSC_ncp11_supp/hcpc27_DSC.csv")

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

hcpc28[is.na(hcpc28)] = c("DP37_47", "DP13_24")

# save the results

write_csv(hcpc28, "analysis/DSC_ncp11_supp/hcpc28_DSC.csv")

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

hcpc29[is.na(hcpc29)] = c("DP37_47", "DP13_24")

# save the results

write_csv(hcpc29, "analysis/DSC_ncp11_supp/hcpc29_DSC.csv")

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

hcpc30[is.na(hcpc30)] = c("DP37_47", "DP13_24")

# save the results

write_csv(hcpc30, "analysis/DSC_ncp11_supp/hcpc30_DSC.csv")

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

# combine all the cluster results together to one single dataframe (unscaled values)

df_0 <- full_join(df2, df1) %>%
  mutate(Sample = as.character(Sample))

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

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc14)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc15)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc16)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc17)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc18)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc19)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc20)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc21)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc22)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc23)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc24)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc25)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc26)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc27)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc28)

r_hcpc_inter <- left_join(r_hcpc_inter, r_hcpc29)

r_hcpc_all <- left_join(r_hcpc_inter, r_hcpc30)

# save the final dataset

write_csv(r_hcpc_all, "analysis/DSC_ncp17/hcpc_ncp17_DSC.csv")

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

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc14)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc15)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc16)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc17)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc18)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc19)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc20)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc21)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc22)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc23)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc24)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc25)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc26)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc27)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc28)

r_hcpc_s_inter <- left_join(r_hcpc_s_inter, r_hcpc29)

r_hcpc_s_all <- left_join(r_hcpc_s_inter, r_hcpc30)

# save the final dataset

write_csv(r_hcpc_s_all, "analysis/DSC_ncp11_supp/hcpc_scaled_ncp11_DSC.csv")

#### count for the number of samples in each cluster ####

count <- r_hcpc_s_all %>% 
  select(cluster2:cluster30) %>%
  gather(nbr_cluster, values) %>%
  group_by(nbr_cluster, values) %>%
  count()

write_csv(count, "analysis/DSC_ncp11_supp/count_individual_DSC.csv")

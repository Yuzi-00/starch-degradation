
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

# select the structural properties

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
               ncp = 17, # explain more than 99% of the variability 
               scale.unit = TRUE, 
               quanti.sup = 27:34, # 8 supplementary variables (RVAs and kinetics); 26 active variables 
               graph = FALSE)

summary(res.pca)

# Extract eigenvalues/variances

eig <- get_eig(res.pca) %>%
  as_tibble() %>%
  rownames_to_column() %>%
  rename(dimension = rowname) %>%
  mutate_if(is.numeric, round, digits = 1)

eig


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

ggsave("figures/cumulative_screeplot_DSC.png", 
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

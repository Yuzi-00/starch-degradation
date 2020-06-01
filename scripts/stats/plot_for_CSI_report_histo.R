
# !!!!!! run script HCPC_mod_ncp9 before this script !!!!!!

#### for replicates ####

#### plot for cluster8-6 ####

# this cluster contains significant Xinf

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
  select(1, 48:70) %>%
  gather("factor", "se", -cluster8) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_8c_full <- full_join(mean_8c_m, mean_8c_s) %>%
  filter(cluster8 ==6)

# add a new column to distinguish the explicative and illustratif factors 

mean_8c_full <- mean_8c_full %>%
  mutate(type = case_when(factor %in% c("h", "k", "Xinf", "Peak_Vis",
                                        "Trough_Vis", "Final_Vis", "Pasting_Temp",
                                        "residual") ~ "Supplementary variable",
                          TRUE ~ "Active variable"))

# change the label for x axis

mean_6c_full[which(mean_6c_full == "Amylose_Con")] <- "Amylose content"

x[which(x==0|x==1|x==2)] <-3

# histogram (scaled values)

ncp9_c8_6 <- mean_8c_full %>%
  ggplot(aes(x = factor, y = mean, fill = type)) +
  # scale_fill_manual(values = c("8888", "4444")) +
  geom_errorbar(aes(x = factor,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "dark grey") +
  geom_bar(stat = 'identity', width = 0.3) +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.spacing.x = unit(0.5, 'cm')) +
  theme(axis.text.x = element_text(color="black", size=15), 
        axis.text.y = element_text(color="black", size=15)) +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) +
  scale_x_discrete(labels = c("A granule", "AB ratio", "Amylase activity", "Amylose content",
                              "B granule", "Dv10", "Dv50", "Dv90", "DP13-24", "DP25-36",
                              "DP37-47", "DP6-12", "FV", "h", "k", "PT",
                              "PV", "Residual", "SSA", "SWM", "TV", "VWM", 
                              "Xinf")) +
  xlab("Factors") +
  ylab("Variability around mean")

ncp9_c8_6

ggsave("figures/HCPC/ncp9/ncp9_c8-6_scal_rep.png", 
       plot = ncp9_c8_6, 
       width = 30, 
       height = 20, 
       units = "cm") 


#### plot for cluster8-8 ####

# this cluster contains significant h and k 

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
  select(1, 48:70) %>%
  gather("factor", "se", -cluster8) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_8c_full <- full_join(mean_8c_m, mean_8c_s) %>%
  filter(cluster8 ==8)

# add a new column to distinguish the explicative and illustratif factors 

mean_8c_full <- mean_8c_full %>%
  mutate(type = case_when(factor %in% c("h", "k", "Xinf", "Peak_Vis",
                                        "Trough_Vis", "Final_Vis", "Pasting_Temp",
                                        "residual") ~ "Supplementary variable",
                          TRUE ~ "Active variable"))

# change the label for x axis

mean_6c_full[which(mean_6c_full == "Amylose_Con")] <- "Amylose content"

x[which(x==0|x==1|x==2)] <-3

# histogram (scaled values)

ncp9_c8_8 <- mean_8c_full %>%
  ggplot(aes(x = factor, y = mean, fill = type)) +
  # scale_fill_manual(values = c("8888", "4444")) +
  geom_errorbar(aes(x = factor,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "dark grey") +
  geom_bar(stat = 'identity', width = 0.3) +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.spacing.x = unit(0.5, 'cm')) +
  theme(axis.text.x = element_text(color="black", size=15), 
        axis.text.y = element_text(color="black", size=15)) +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) +
  scale_x_discrete(labels = c("A granule", "AB ratio", "Amylase activity", "Amylose content",
                              "B granule", "Dv10", "Dv50", "Dv90", "DP13-24", "DP25-36",
                              "DP37-47", "DP6-12", "FV", "h", "k", "PT",
                              "PV", "Residual", "SSA", "SWM", "TV", "VWM", 
                              "Xinf")) +
  xlab("Factors") +
  ylab("Variability around mean")

ncp9_c8_8

ggsave("figures/HCPC/ncp9/ncp9_c8-8_scal_rep.png", 
       plot = ncp9_c8_8, 
       width = 30, 
       height = 20, 
       units = "cm") 

#### for samples ####

#### plot for cluster7-5 ####

# this cluster contains significant Xinf 

# creat two subsets for means and std errors

mean_7c_m <- mean_7c %>%
  select(1:24) %>%
  gather("factor", "mean", -cluster7) %>%
  mutate(factor = str_replace(factor, "_mean", "")) # remove the string "_mean"

mean_7c_s <- mean_7c %>%
  select(1, 48:70) %>%
  gather("factor", "se", -cluster7) %>%
  mutate(factor = str_replace(factor, "_se", "")) # remove the string "_se"

# combine the above two datasets together

mean_7c_full <- full_join(mean_7c_m, mean_7c_s) %>%
  filter(cluster7 == 5)

# add a new column to distinguish the explicative and illustratif factors 

mean_7c_full <- mean_7c_full %>%
  mutate(type = case_when(factor %in% c("h", "k", "Xinf", "Peak_Vis",
                                        "Trough_Vis", "Final_Vis", "Pasting_Temp",
                                        "residual") ~ "Supplementary variable",
                          TRUE ~ "Active variable"))

# histogram (scaled values)

ncp9_c7_5 <- mean_7c_full %>%
  ggplot(aes(x = factor, y = mean, fill = type)) +
  # scale_fill_manual(values = c("8888", "4444")) +
  geom_errorbar(aes(x = factor,
                    ymin = mean - se,
                    ymax = mean + se),
                width=0.2,
                color = "dark grey") +
  geom_bar(stat = 'identity', width = 0.3) +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.spacing.x = unit(0.5, 'cm')) +
  theme(axis.text.x = element_text(color="black", size=15), 
        axis.text.y = element_text(color="black", size=15)) +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) +
  xlab("Factors") +
  ylab("Variability around mean")

ggsave("figures/HCPC/ncp9/ncp9_c7-5_scal.png", 
       plot = ncp9_c7_5, 
       width = 30, 
       height = 20,  
       units = "cm") 


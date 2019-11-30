library(tidyverse)


############################################# pca -> 2 categories of amylose to h k Xinf #########################################


# read in the dataset

data_6P <- read_csv("C:/Users/WAN333/Documents/Data school/starch-hydrolysis/data/tidydata/joined_6P.csv")

# tidy the data

data_selected <- data_6P %>% 
  filter(Time == 1800) %>% 
  filter(!is.na(Hydro_extent), !is.na(Amylose_content)) %>% # remove the missing value
  filter(Sample != "C+", Sample != "C-") %>% # remove the controls which don't belong to the magic population
  select(Sample, h, k, Xinf) %>% 
  unique() %>% # remove the same rows
  column_to_rownames("Sample") %>% # remove the column name
  scale 


pca <- prcomp(data_selected)

summary(pca)

HE <-  data_6P %>% 
  filter(Time ==1800) %>% 
  filter(!is.na(Hydro_extent), !is.na(Amylose_content)) %>% # remove the missing value
  filter(Sample != "C+", Sample != "C-") %>% # remove the controls which don't belong to the magic population
  select(Sample, Amylose_content) %>% 
  arrange(Amylose_content) %>% 
  mutate(status = case_when( 
    Amylose_content < 30 ~ "low_amy", 
    Amylose_content >= 30 ~ "high_amy"
  )) %>% 
  unique()


pca_inter <- pca$x %>% 
  as_tibble() %>% 
  mutate(status = HE$status, Sample = HE$Sample) 

# import the design dataset

design <- read_csv("data/tidydata/previous_data/design_magic_pop.csv")

# merge the corresponding ID into the dataset by left_join

pca_final <- left_join(pca_inter, design)

pca_final %>% 
  ggplot(aes(x = PC1, y = PC2,
             color = ID)) +
  geom_point() +
  theme(legend.position = "none")

biplot(pca)

#############################################################################################################################


############################################# pca -> 2 categories of h to structural properties #############################


# read in the dataset

data_6P <- read_csv("C:/Users/WAN333/Documents/Data school/starch-hydrolysis/data/tidydata/joined_6P.csv")

# tidy the data

data_selected <- data_6P %>% 
  filter(Time == 1800) %>% 
  filter(!is.na(Hydro_extent), !is.na(Amylose_content)) %>% # remove the missing value
  filter(Sample != "C+", Sample != "C-") %>% # remove the controls which don't belong to the magic population
  select(Sample, Amylose_content, D1, D5, D9, mean_Peak, mean_Trough, mean_Final, mean_PastingTemp) %>% 
  unique() %>% # remove the same rows
  column_to_rownames("Sample") %>% # remove the column name
  scale 


pca <- prcomp(data_selected)

summary(pca)

HE <-  data_6P %>% 
  filter(Time ==1800) %>% 
  filter(!is.na(Hydro_extent), !is.na(Amylose_content)) %>% # remove the missing value
  filter(Sample != "C+", Sample != "C-") %>% # remove the controls which don't belong to the magic population
  select(Sample, h) %>% 
  mutate(status = case_when( 
    h < 0 ~ "neg_h", 
    h > 0 ~ "pos_h"
  )) %>% 
  unique()


pca$x %>% 
  as_tibble() %>% 
  mutate(status = HE$status) %>% 
  ggplot(aes(x = PC1, y = PC2,
             color = status)) +
  geom_point()


############################################# pca -> h k Xinf to a more complete structural properties #########################


# read in the dataset

data_6P <- read_csv("data/tidydata/joined_6P_update.csv")

# tidy the data

data_selected <- data_6P %>% 
  filter(!is.na(Hydro_extent), !is.na(Amylose_content), !is.na(low_dp)) %>% # remove the missing value
  filter(Sample != "C+", Sample != "C-") %>% # remove the controls which don't belong to the magic population
  select(Sample, Amylose_content, D1, D5, D9, mean_Peak, mean_Trough, mean_Final, mean_PastingTemp, low_dp, medium_dp, 
         medium_high_dp, high_dp) %>% 
  unique() %>% # remove the same rows
  column_to_rownames("Sample") %>% # remove the column name
  scale 


pca <- prcomp(data_selected)

summary(pca)


HE_new <- data_6P %>% 
  filter(!is.na(Hydro_extent), !is.na(Amylose_content), !is.na(low_dp)) %>% # remove the missing value
  filter(Sample != "C+", Sample != "C-") %>% # remove the controls which don't belong to the magic population
  select(Sample, h, k, Xinf) %>% 
  unique() # remove the same rows


##HE <-  data_6P %>% 
  ##filter(Time ==1800) %>% 
  ##filter(!is.na(Hydro_extent), !is.na(Amylose_content)) %>% # remove the missing value
  ##filter(Sample != "C+", Sample != "C-") %>% # remove the controls which don't belong to the magic population
  ##select(Sample, h) %>% 
  ##mutate(status = case_when( 
    ##h < 0 ~ "neg_h", 
    ##h > 0 ~ "pos_h"
  ##)) %>% 
  ##unique()
##

pca_inter <- pca$x %>% 
  as_tibble() %>% 
  mutate(Sample = HE_new$Sample, h = HE_new$h, k = HE_new$k, Xinf = HE_new$Xinf)

# import the design dataset

design <- read_csv("data/tidydata/previous_data/design_magic_pop.csv")

# add the corresponding ID to each sample

pca_before_final <- left_join(pca_inter, design)

# extract the HE at 1800min

HE_1800 <- data_6P %>% 
  filter(Time == 1800) %>% 
  filter(!is.na(Hydro_extent)) %>% # remove the missing value
  filter(Sample != "C+", Sample != "C-") %>% 
  select(ID, Sample, Hydro_extent) %>% 
  group_by(ID) %>% 
  summarise(mean_HE = mean(Hydro_extent))

# add HE at 1800min to the pca_before_final dataset

pca_final <- left_join(pca_before_final, HE_1800)


pca_final%>% 
  ggplot(aes(x = PC1, y = PC2, color = Xinf)) +
  geom_point() +
  theme(legend.position = "none") 
  


############################################# pca -> h k Xinf to a less complete structural properties #########################


# read in the dataset

data_6P <- read_csv("data/tidydata/joined_6P_update.csv")

# tidy the data

data_selected <- data_6P %>% 
  filter(!is.na(Hydro_extent), !is.na(Amylose_content)) %>% # remove the missing value
  filter(Sample != "C+", Sample != "C-") %>% # remove the controls which don't belong to the magic population
  select(Sample, Amylose_content, D1, D5, D9, mean_Peak, mean_Trough, mean_Final, mean_PastingTemp) %>% 
  unique() %>% # remove the same rows
  column_to_rownames("Sample") %>% # remove the column name
  scale 


pca <- prcomp(data_selected)

summary(pca)


##HE_new <- data_6P %>% 
##  filter(!is.na(Hydro_extent), !is.na(Amylose_content), !is.na(low_dp)) %>% # remove the missing value
##  filter(Sample != "C+", Sample != "C-") %>% # remove the controls which don't belong to the magic population
##  select(Sample) %>% 
##  unique() # remove the same rows


HE <-  data_6P %>% 
  filter(!is.na(HE)) %>% # remove the missing value
  filter(Sample != "C+", Sample != "C-") %>% # remove the controls which don't belong to the magic population
  select(Sample, h) %>% 
  mutate(status = case_when( 
    h < 0 ~ "neg_h", 
    h > 0 ~ "pos_h"
  )) %>% 
  unique()
##

pca$x %>% 
  as_tibble() %>% 
  mutate(Sample = HE$Sample) %>% 
  ggplot(aes(x = PC1, y = PC2,
             color = Sample)) +
  geom_point() +
  theme(legend.position = "none")



############################################# pca -> a more complete structural properties to h k Xinf #########################


# read in the dataset

data_6P <- read_csv("data/tidydata/joined_6P_update.csv")

# tidy the data

data_selected <- data_6P %>% 
  filter(!is.na(Hydro_extent), !is.na(Amylose_content), !is.na(low_dp)) %>% # remove the missing value
  filter(Sample != "C+", Sample != "C-") %>% # remove the controls which don't belong to the magic population
  select(Sample, h, k, Xinf) %>% 
  unique() %>% # remove the same rows
  column_to_rownames("Sample") %>% # remove the column name
  scale 

# Amylose_content, D1, D5, D9, mean_Peak, mean_Trough, mean_Final, mean_PastingTemp, low_dp, medium_dp, 
# medium_high_dp, high_dp

pca <- prcomp(data_selected)

summary(pca)


HE_new <- data_6P %>% 
  filter(!is.na(Hydro_extent), !is.na(Amylose_content), !is.na(low_dp)) %>% # remove the missing value
  filter(Sample != "C+", Sample != "C-") %>% # remove the controls which don't belong to the magic population
  select(Sample, Amylose_content, D1, D5, D9, mean_Peak, mean_Trough, mean_Final, mean_PastingTemp, low_dp, medium_dp, 
         medium_high_dp, high_dp) %>% 
  unique() # remove the same rows


##HE <-  data_6P %>% 
##filter(Time ==1800) %>% 
##filter(!is.na(Hydro_extent), !is.na(Amylose_content)) %>% # remove the missing value
##filter(Sample != "C+", Sample != "C-") %>% # remove the controls which don't belong to the magic population
##select(Sample, h) %>% 
##mutate(status = case_when( 
##h < 0 ~ "neg_h", 
##h > 0 ~ "pos_h"
##)) %>% 
##unique()
##

pca_inter <- pca$x %>% 
  as_tibble() %>% 
  mutate(Sample = HE_new$Sample, Amy = HE_new$Amylose_content, D1 = HE_new$D1, D5 = HE_new$D5,
         D9 = HE_new$D9, Peak = HE_new$mean_Peak, Trough = HE_new$mean_Trough, Final = HE_new$mean_Final,
         PastT = HE_new$mean_PastingTemp, Lowdp = HE_new$low_dp, meddp = HE_new$medium_dp, medhidp = HE_new$medium_high_dp,
         highdp = HE_new$high_dp)

# import the design dataset

design <- read_csv("data/tidydata/previous_data/design_magic_pop.csv")

# add the corresponding ID to each sample

pca_before_final <- left_join(pca_inter, design)

# extract the HE at 1800min

HE_1800 <- data_6P %>% 
  filter(Time == 1800) %>% 
  filter(!is.na(Hydro_extent)) %>% # remove the missing value
  filter(Sample != "C+", Sample != "C-") %>% 
  select(ID, Sample, Hydro_extent) %>% 
  group_by(ID) %>% 
  summarise(mean_HE = mean(Hydro_extent))

# add HE at 1800min to the pca_before_final dataset

pca_final <- left_join(pca_before_final, HE_1800)


pca_final%>% 
  ggplot(aes(x = PC1, y = PC2, color = Amy)) +
  geom_point() +
  theme(legend.position = "none") 

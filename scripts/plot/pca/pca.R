library(tidyverse)

data_6P <- read_csv("data/HE_amy_6P.csv")

# check the distribution 

## filter by time point first

time1800 <- data_6P %>% 
  filter(Time == 1800)

## plot the histogram

hist(time1800$Hydro_extent)

## normality tests

shapiro.test(time1800$Hydro_extent)

ad.test(time1800$Hydro_extent)

cvm.test()

##########################################

library(nortest)

hydro <- data_6P %>% 
  select(Hydro_extent) %>% scale

shapiro.test(hydro)
hist(hydro)


shapiro.test(data_6P$Amylose_content)


######################################## pca #######################################################

library(tidyverse)

# read in the dataset

data_6P <- read_csv("resources/data/joined_6P.csv")

# tidy the data

data_selected <- data_6P %>% 
  filter(!is.na(Hydro_extent), !is.na(Amylose_content)) %>% # remove the missing value
  filter(Sample != "C+", Sample != "C-") %>% # remove the controls which don't belong to the magic population
  select(Sample, mean_Peak, mean_Trough, mean_Final, mean_PastingTemp) %>% 
  unique() %>% # remove the same rows
  column_to_rownames("Sample") %>% # remove the column name
  scale 

pca <- prcomp(data_selected)

summary(pca)

data_filted <-  data_6P %>% 
  filter(!is.na(Hydro_extent), !is.na(Amylose_content)) %>% # remove the missing value
  filter(Sample != "C+", Sample != "C-") %>% # remove the controls which don't belong to the magic population
  select(Sample, Amylose_content, D1, D5, D9, h, k, Xinf, mean_Peak, mean_Trough, mean_Final, mean_PastingTemp) %>% 
  unique() 

HE <-  data_6P %>% 
  filter(!is.na(Hydro_extent), !is.na(Amylose_content)) %>% # remove the missing value
  filter(Sample != "C+", Sample != "C-") %>% # remove the controls which don't belong to the magic population
  select(Hydro_extent) %>% 
  unique() 

# pca plot colored by sample

pca$x %>% 
  as_tibble() %>% 
  rownames_to_column("Sample")%>% 
  mutate(h = data_filted$h, k = data_filted$k, Xinf = data_filted$Xinf,
         amy = data_filted$Amylose_content, D1 = data_filted$D1, D5 = data_filted$D5, D9 = data_filted$D9) %>% 
  ggplot(aes(x = PC1, y = PC2,
           color = amy)) +
  geom_point(alpha = 0.6) +
  theme(legend.position = "none")

# pca plot colored by time

pca$x %>% 
  as_tibble() %>% 
  mutate(Sample = data_filted$Sample, Time = data_filted$Time, HE = data_filted$Hydro_extent) %>% 
  ggplot(aes(x = PC1, y = PC2,
             color = Time)) +
  geom_point()

# pca plot colored by HE

pca$x %>% 
  as_tibble() %>% 
  mutate(Sample = data_filted$Sample, Time = data_filted$Time, HE = data_filted$Hydro_extent) %>% 
  ggplot(aes(x = PC1, y = PC3,
             color = HE)) +
  geom_point(alpha = 0.07) 



# seems the results from the above are too messy, let's try to do the pca with each time point

data_selected <- data_6P %>% 
  filter(!is.na(Hydro_extent), !is.na(Amylose_content)) %>% 
  filter(Time == 1800) 

data_scaled <- data_selected %>% 
  select(Amylose_content, D1, D5, D9) %>% 
  scale

pca <- prcomp(data_scaled)

summary(pca)

data_filted <- data_6P %>% 
  filter(!is.na(Hydro_extent), !is.na(Amylose_content)) %>% 
  filter(Time == 1800)

pca$x %>% 
  as_tibble() %>% 
  mutate(Sample = data_filted$Sample, Time = data_filted$Time, HE = data_filted$Hydro_extent) %>% 
  ggplot(aes(x = PC1, y = PC2,
             color = HE)) +
  geom_point(alpha = 0.4) 

# it's not good enough to see

# map(1:100, ~ sample_income(30) %>% mean)

# draft
Sample <- data_6P %>% 
  select(Sample)

data_scaled <- as_tibble(data_selected)

df <- bind_cols(Sample, data_scaled)

head(data_selected)


#
names(data_6P$Sample) <- NULL

colnames(data_6P[1,1]) <- NULL

samp.with.rownames <- data.frame(data_6P[,-1], row.names=data_6P[,1])

row_names(data_6P) <- data_6P$names

data_6P %>% 
  colnames(do.NULL = TRUE) 


data_6P %>% 
  colnames(Sample) <- value

samp[1] <- NULL

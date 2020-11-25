
######### this script is to combine the DSC data with the other structural data #########

######### and to see if there are any correlations in between #########

library(tidyverse)

library(readxl)

# import the DSC dataset

df <- read_xlsx("data/DSC_total.xlsx", sheet = "tableau_R") %>%
  mutate(Sample = as.character(sample)) %>%
  select(-sample)

# import the dataset of other parameters

df0 <- read_csv("analysis/total_new_convert_replaced_by_granular.csv") 

# combine these two datasets

df1 <- left_join(df, df0) %>%
  rename(H1 = '∆H1',
         H2 = '∆H2') %>%
  mutate(H2 = as.numeric(H2))

# calculate the H(total)

df2 <- df1 %>%
  mutate(H = H1+ H2)

# save the dataset

write_csv(df1, "analysis/total_new_convert_DSC.csv")

write_csv(df2, "analysis/total_new_convert_DSC_withH.csv")

# correlation matrix

# corrplot

library(corrplot)

my_subset <- df1 %>%
  select(1:10, 15:24, 34:46)

cor_result <- cor(my_subset, use = "complete.obs")

correlation <- corrplot(cor_result, method = "number", type = "lower",
                        tl.cex = 0.5, number.cex = 0.5) 
# Here, we find a weak positive relationship between the Tp1 and DP13-24, need to look into the papers to check it out 

# plot of DSC parameters and amylose content

p1 <- df1 %>% 
  ggplot(aes(x = To1, 
             y = Amylose_Con)) +
  geom_point()

p1

p2 <- df1 %>% 
  ggplot(aes(x = Tp1, 
             y = Amylose_Con)) +
  geom_point()

p2

p3 <- df1 %>% 
  ggplot(aes(x = Tc1, 
             y = Amylose_Con)) +
  geom_point()

p3

p4 <- df1 %>% 
  ggplot(aes(x = Range1, 
             y = Amylose_Con)) +
  geom_point()

p4

p5 <- df1 %>% 
  ggplot(aes(x = H1, 
             y = Amylose_Con)) +
  geom_point()

p5

p6 <- df1 %>% 
  ggplot(aes(x = To2, 
             y = Amylose_Con)) +
  geom_point()

p6

p7 <- df1 %>% 
  ggplot(aes(x = Tp2, 
             y = Amylose_Con)) +
  geom_point()

p7

p8 <- df1 %>% 
  ggplot(aes(x = Tc2, 
             y = Amylose_Con)) +
  geom_point()

p8

p9 <- df1 %>% 
  ggplot(aes(x = Range2, 
             y = Amylose_Con)) +
  geom_point()

p9

p10 <- df1 %>% 
  ggplot(aes(x = H2, 
             y = Amylose_Con)) +
  geom_point()

p10

# plot of H1, H2 and amylopectin chain length

p11 <- df1 %>% 
  ggplot(aes(x = H1, 
             y = DP6_12)) +
  geom_point()

p11
# several points are lined up (shall thak a look at it)

p12 <- df1 %>% 
  ggplot(aes(x = H1, 
             y = DP13_24)) +
  geom_point()

p12

p13 <- df1 %>% 
  ggplot(aes(x = H1, 
             y = DP25_36)) +
  geom_point()

p13

p14 <- df1 %>% 
  ggplot(aes(x = H1, 
             y = DP37_47)) +
  geom_point()

p14

p15 <- df1 %>% 
  ggplot(aes(x = H2, 
             y = DP6_12)) +
  geom_point()

p15

p16 <- df1 %>% 
  ggplot(aes(x = H2, 
             y = DP13_24)) +
  geom_point()

p16

p17 <- df1 %>% 
  ggplot(aes(x = H2, 
             y = DP25_36)) +
  geom_point()

p17

p18 <- df1 %>% 
  ggplot(aes(x = H2, 
             y = DP37_47)) +
  geom_point()

p18


# plot of To1, Tp1, Range1 and A/B ratio

p19 <- df1 %>% 
  ggplot(aes(x = To1, 
             y = pi_AB_ratio)) +
  geom_point()

p19

p20 <- df1 %>% 
  ggplot(aes(x = Range1, 
             y = mu_C)) +
  geom_point()

p20


library(tidyverse)

#### 1st round fitting ####

df <- read_csv("analysis/granular_output_01.csv")

# remove samples that were not well fitted 

df2 <- df %>%
  filter(!sample %in% c(4, 12, 14, 17, 18, 20, 21, 22, 25, 27, 28, 29, 30, 32, 35, 36, 37, 41,
                       44, 47, 57, 58, 59, 60, 63, 69, 73, 74, 76, 78, 79, 80, 83, 84, 89, 90, 91,
                       93, 95, 96, 97, 98, 100, 101, 102, 104, 109, 116, 117, 122, 127, 136, 144,
                       146, 151, 153, 154, 155, 156, 157, 158, 161, 163, 164, 165, 166, 167, 168,
                       170, 172, 176, 177, 178, 180, 182, 185, 187, 188, 189, 191, 192, 193, 194,
                       197, 198, 199, 200, 202, 203, 206, 208, 211, 217, 218, 219, 220)) %>%
  mutate(sample = as.numeric(sample))

# save the dataset

write_csv(df2, "analysis/granular_output_01_tidy.csv")

#### 2nd round fitting ####

df3 <- read_csv("analysis/granular_output_02.csv")

df4 <- df3 %>%
  filter(!sample %in% c(17, 20, 21, 27, 28, 30, 32, 89, 90, 93, 95, 98, 101, 104, 109, 146, 153, 154, 156,
                       158, 161, 163, 176, 177, 178, 182, 189, 197, 198, 199, 203, 206, 219, 220))

# save the dataset

write_csv(df4, "analysis/granular_output_02_tidy.csv")

#### 3rd round fitting ####

df5 <- read_csv("analysis/granular_output_03.csv")

df6 <- df5 %>%
  filter(!sample %in% c(17, 20, 21, 27, 28, 30, 32, 89, 90, 95, 98, 101, 104, 109, 154, 156,
                       158, 163, 176, 177, 178, 197, 198, 203, 220))

# save the dataset

write_csv(df6, "analysis/granular_output_03_tidy.csv")

#### 4rd round fitting ####

df7 <- read_csv("analysis/granular_output_04.csv")

# save the dataset

write_csv(df7, "analysis/granular_output_04_tidy.csv")

#### refitting of sample 43 ####

df43 <- read_csv("analysis/granular_output_05.csv")

# combine all these four datasets together 

df8 <- full_join(df2, df4)

df9 <- full_join(df8, df6)

df10 <- full_join(df9, df7)

# remove the wrong sample 43

df_inter <- df10 %>%
  filter(!sample %in% "43")

# add the correct sample 43

df11 <- full_join(df_inter, df43)

write_csv(df11, "analysis/granular_output_final.csv")

#### calculate A/B granule proportion ####

df_pi <- df11 %>%
  select(sample, peak, pi) %>%
  spread(key = peak, value = pi) %>%
  mutate(pi_AB_ratio = peak_A / peak_B) %>%
  rename(pi_A = peak_A, pi_B = peak_B, pi_C = peak_C)

df_mu <- df11 %>%
  select(sample, peak, mu) %>%
  spread(key = peak, value = mu) %>%
  rename(mu_A = peak_A, mu_B = peak_B, mu_C = peak_C)

df_sigma <- df11 %>%
  select(sample, peak, sigma) %>%
  spread(key = peak, value = sigma) %>%
  rename(sigma_A = peak_A, sigma_B = peak_B, sigma_C = peak_C)

# put the above subsets together

df_inter <- left_join(df_pi, df_mu)

df_total <- left_join(df_inter, df_sigma) 
## there is one NA in ths dataset (sample name 82*)

# replace the NA 

df_total[is.na(df_total)] = "81"

write_csv(df_total, "analysis/granular_output_final_tidy.csv")


 

#                                                      ** tidy the dataset **


library(tidyverse)

library(readxl)

# assigning the unchangeable part of the path

filename1 <- "C:/Users/WAN333/Documents/Thesis/Experiments/2020-01/20200107_DNS_Qin/raw_data/20200107_DNS_Qin_t"
filename2 <- 'min.xlsx'

# assigning the changeable part of the path

filetime <- c('20','60','120','180', '240', '360', '1440', '1800') 

# read the 1st spreadsheet et and assign it to a new dataframe

df <- read_excel(paste0(filename1,'0',filename2), range = "A15:E19") 

# add two columns: plate number and time point for the 1st spreadsheet

df <- mutate(df, Time = 0)

# creat a loop to import all the spreadsheet
# add two columns: plate number and time point from the 2nd spreadsheet on 
# bind them by raws

for (fltime in filetime) {
  filename_real <- paste0(filename1,fltime,filename2)
  print(filename_real)
  temp_data <- read_excel(filename_real, range = "A15:E19",na = 'NA')
  temp_data <- mutate(temp_data, Time = as.numeric(fltime))
  df <- bind_rows(df, temp_data)
}

# modify the default name for the first col & raw

df <- rename(df, Row = ...1) 

# creat a new column to count the replicates and remove the controls

df_sample <- df %>% 
  filter(Row != "D") %>% 
  mutate(Rep = case_when( 
    Row == "A" ~ "1", 
    Row == "B" ~ "2", 
    Row == "C" ~ "3"
  ))

# transfer the data into two columns 

df_sample <- gather(df_sample, Col, OD, -Row, -Time, -Rep) 

df_sample

# ordering the column names

df_sample <- select(df_sample, Row, Col, Time, Rep, OD)

df_sample

# check the NAs

filter(df_sample,is.na(OD)) %>% 
  nrow() # 0 NAs

write_csv(df_sample, "data/Qin/tidydata/sample_data.csv")




#                                                      ** tidy the mass data **


# import the dataset

mass <- read_xlsx("C:/Users/WAN333/Documents/Thesis/Experiments/2020-01/20200107_DNS_Qin/raw_data/mass_starch.xlsx", range = "A1:E5")

mass # the Row D is the control without starch

# rename the first cell

mass <- rename(mass, Row = ...1) 

mass

# transfer the data into two columns

mass <- mass %>%  
  gather(col, mass, -Row)

mass

# arrange by Row

mass <- mass %>% 
  rename(Column = col, Mass = mass) %>% 
  arrange(Row)

mass

# in order to join this dataset with the other, each row has to repete 9 times 

mass_rep  <- mass[rep(seq_len(nrow(mass)), each = 9), ] # 16*9 = 108 rows

# replace all the 0 by NA

mass_rep <- mass_rep %>% 
  mutate(Mass = na_if(Mass, "0")) # if the value in column Mass is 0, replace them by NA 

mass_rep

# check the NA

missing_value <- mass_rep %>% 
  filter(is.na(Mass)) # 36 NAs correspond to 4*9 = 36 controls 

# save the total mass dataset

write_csv(mass_rep, "data/Qin/tidydata/mass_tidy.csv")


#                                                      ** combination **


# read in the tidied mass dataset

mass_rep <- read_csv("data/Qin/tidydata/mass_tidy.csv") %>% 
  filter(Row != "D")

sample <- read_csv("data/Qin/tidydata/sample_data.csv") %>% 
  arrange(Row)

# select just the Mass column to be merged in the next step

mass_selected <- select(mass, Mass)

df02 <- bind_cols(sample, mass_selected) %>% 
  select(Row, Col, Time, Rep, Mass, OD) %>%  # ordering the columns
  rename(OD_sample = OD)

df02

# import the layout

layout <- read_xlsx("C:/Users/WAN333/Documents/Thesis/Experiments/2020-01/20200107_DNS_Qin/raw_data/layout.xlsx", range = "A2:C18")

# add the layout to the previous dataset

df03 <- left_join(df02, layout) %>% 
  select(Row, Col, Time, Sample, Rep, Mass, OD_sample)

df03

## creat subsets for different controls 

# control for samples without enzyme

control <- df %>% 
  filter(Row == "D")

control

control <- control %>% 
  gather(Col, OD_ctl, -Row, -Time)

control

# calculate the mean value of the control for samples without enzyme

mean_ctl <- control %>% 
  group_by(Time) %>% 
  summarise(mean_OD_ctl = mean(OD_ctl))

mean_ctl

# add the mean_ctl to the control dataset

control_final <- left_join(control, mean_ctl) %>% 
  mutate(mean_C_ctl = mean_OD_ctl / 0.2035) %>% # the slope of the standard curve is 0.2035 
  select(Time, mean_C_ctl) %>% 
  unique()

control_final

# select subset for sample without enzyme

sample_without_enz <- df03 %>%
  filter(Sample == "pos_without_enz" | Sample == "neg_without_enz") %>%
  mutate(C_sample = OD_sample / 0.2035,
         C_sample_nor = 10*C_sample / Mass)

# combine the sample_without_enz with its control

sample_without_enz_final <- left_join(sample_without_enz, control_final) %>%
  mutate(C_final = C_sample_nor - mean_C_ctl) %>%
  select(Time, Sample, Rep, C_final) %>%
  arrange(Time)

sample_without_enz_final

# control for neg_with_enz

control_neg <- df03 %>% 
  filter(Sample == "neg_without_enz") %>% 
  mutate(C_ctl_neg = OD_sample/0.2035,
         C_ctl_neg_nor = 10*C_ctl_neg/Mass) %>% 
  group_by(Time) %>% 
  summarise(mean_C_ctl_neg_nor = mean(C_ctl_neg_nor))

control_neg

# select the subset neg_with_enz

neg_with_enz <- df03 %>% 
  filter(Sample == "neg_with_enz") %>% 
  mutate(C_negwithenz = OD_sample/0.2035,
         C_negwithenz_nor = 10*C_negwithenz/Mass)

neg_with_enz

# combine the neg_with_enz and its control

neg_with_enz_final <- left_join(neg_with_enz, control_neg) %>% 
  mutate(C_final = C_negwithenz_nor - mean_C_ctl_neg_nor) %>% 
  select(Time, Sample, Rep, C_final) %>% 
  arrange(Time)

neg_with_enz_final

# control for pos_with_enz

control_pos <- df03 %>% 
  filter(Sample == "pos_without_enz") %>% 
  mutate(C_ctl_pos = OD_sample/0.2035,
         C_ctl_with_pos_nor = 10*C_ctl_pos/Mass) %>% 
  group_by(Time) %>% 
  summarise(mean_C_ctl_pos_nor = mean(C_ctl_with_pos_nor))

control_pos

# select the subset pos_with_enz

pos_with_enz <- df03 %>% 
  filter(Sample == "pos_with_enz") %>% 
  mutate(C_poswithenz = OD_sample/0.2035,
         C_poswithenz_nor = 10*C_poswithenz/Mass)

pos_with_enz

# combine the pos_with_enz and its control

pos_with_enz_final <- left_join(pos_with_enz, control_pos) %>% 
  mutate(C_final = C_poswithenz_nor - mean_C_ctl_pos_nor) %>% 
  select(Time, Sample, Rep, C_final) %>% 
  arrange(Time)

pos_with_enz_final

# combine the three subsets together

half_combine <- bind_rows(pos_with_enz_final, neg_with_enz_final)

half_combine

full_combine <- bind_rows(half_combine, sample_without_enz_final)

full_combine

# calculate the hydrolysis extent

cal_HE <- full_combine %>% 
  mutate(HE = C_final / 11.1 * 100)

cal_HE

cal_var <- cal_HE %>% 
  group_by(Sample, Time) %>% 
  mutate(Mean_C = mean(C_final, na.rm = TRUE),
         Sd_C = sd(C_final, na.rm = TRUE),
         Cov = Sd_C / Mean_C * 100,
         Se_C = Sd_C/sqrt(3)) %>% 
  select(Sample, Time, Rep, C_final, Mean_C, Sd_C, Cov, Se_C)

cal_var

write_csv(cal_var, "analysis/Qin/DNS_result.csv")


#                                                      ** plot **


# import the dataset

cal_var <- read_csv("analysis/Qin/cal_var.csv") %>% 
  select(Sample, Time, Mean_C, Se_C) %>% 
  unique()

mean_C_plot <- ggplot(data = cal_var, 
                      aes(x = Time, 
                          y = Mean_C,
                          group = Sample,
                          color = Sample)) +
  geom_point(size = 1, shape = 1) + # add the transparency
  geom_line(size = 0.005, alpha = 0.8) +
  geom_errorbar(aes(x = Time, 
                    ymin = Mean_C - Se_C, 
                    ymax = Mean_C + Se_C), 
                width=20,
                color = "red") +
  scale_y_continuous(limits = c(0,10), expand = c(0, 0)) + ## set the range of the y axis
  scale_x_continuous(limits = c(0, 2000), expand = c(0, 0)) +
  ylab("Hydrolysis extent (%)") + ## change the label for the y axis
  xlab("Time (min)") + ## change the name of the x axis
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.ticks=element_line(
          colour="black",
          size=.5)) +
  labs(x = "Time (min)", y = "Hydrolysis extent (%)") +
  theme(axis.text.x = element_text(color="black", size=10), 
        axis.text.y = element_text(color="black", size=10)) +
  theme(legend.key = element_blank(),
        legend.position = "bottom")+
  theme(plot.margin = unit(c(5.5,12,5.5,5.5), "pt"))

mean_C_plot

# save the plot

ggsave("figures/mean_C_plot.png", 
       plot = mean_C_plot, 
       width = 15, 
       height = 15, 
       units = "cm") 


#                                                      ** stat **


cal_var <- read_csv("analysis/Qin/cal_var.csv") 

# select two subsets

# with enzyme at ?min

with_enz <- cal_var %>% 
  filter(Time == 1800) %>% 
  filter(Sample == "pos_with_enz" | Sample == "neg_with_enz")

# anova test

aov1 <- aov(C_final ~ Sample, data = with_enz)

summary(aov1)

# significatnly different at 20, 60, 120, 240, 360, 1440 min

# without enzyme at ?min

without_enz <- cal_var %>% 
  filter(Time == 20) %>% 
  filter(Sample == "pos_without_enz" | Sample == "neg_without_enz")

# anova test

aov2 <- aov(C_final ~ Sample, data = without_enz)

summary(aov2)

# significantly different from 180 minutes on
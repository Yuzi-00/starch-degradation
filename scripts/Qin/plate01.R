 

#                                                      ** tidy the dataset **


library(tidyverse)

library(readxl)

filename1 <- "C:/Users/WAN333/Documents/Thesis/Experiments/2020-01/20200107_DNS_Qin/raw_data/20200107_DNS_Qin_t"
filename2 <- 'min.xlsx'
# assigning the unchangeable part of the path

filetime <- c('20','60','120','180', '240', '360', '1440', '1800') 
# assigning the changeable part of the path

df <- read_excel(paste0(filename1,'0',filename2), range = "A15:E19") 
# read the 1st spreadsheet et and assign it to a new dataframe

df <- mutate(df, Time = 0)
# add two columns: plate number and time point for the 1st spreadsheet

for (fltime in filetime) {
  filename_real <- paste0(filename1,fltime,filename2)
  print(filename_real)
  temp_data <- read_excel(filename_real, range = "A15:E19",na = 'NA')
  temp_data <- mutate(temp_data, Time = as.numeric(fltime))
  df <- bind_rows(df, temp_data)
}
# creat a loop to import all the spreadsheet
# add two columns: plate number and time point from the 2nd spreadsheet on 
# bind them by raws

df <- rename(df, Row = ...1) 
# modify the default name for the first col & raw

# divide into two subsets

df_sample <- df %>% 
  filter(Row != "D") %>% 
  mutate(Rep = case_when( ## creat a new column to count the repetition
    Row == "A" ~ "1", 
    Row == "B" ~ "2", 
    Row == "C" ~ "3"
  ))

df_control <- df %>% 
  filter(Row == "D") %>% 
  select(-"1", -"3") # remove unuseful data

# transfer the data into two columns 

df_sample_trans <- gather(df_sample, Col, OD, -Row, -Time, -Rep) 

# ordering the column names

df_sample_trans <- select(df_sample_trans, Row, Col, Time, Rep, OD)

write_csv(df_sample_trans, "data/Qin/tidydata/sample_data.csv")

# check the NAs

filter(df_sample_trans,is.na(OD)) %>% 
  nrow() # 0 NAs


#                                                      ** tidy the mass data **


# import the dataset

mass <- read_xlsx("C:/Users/WAN333/Documents/Thesis/Experiments/2020-01/20200107_DNS_Qin/raw_data/mass_starch.xlsx", range = "A1:E5")

# rename the first cell

mass <- rename(mass, Row = ...1) 

# # transfer the data into two columns

mass <- mass %>%  
  gather(col, mass, -Row)

# arrange by Row

mass <- mass %>% 
  rename(Column = col, Mass = mass) %>% 
  arrange(Row)

# in order to join this dataset with the other, each row has to repete 5 times 

mass_rep  <- mass[rep(seq_len(nrow(mass)), each = 9), ] # 16*9 = 108 rows

# replace all the 0 by NA

mass_rep <- mass_rep %>% 
  mutate(Mass = na_if(Mass, "0")) # if the value in column Mass is 0, replace them by NA 

# check the NA

missing_value <- mass_rep %>% 
  filter(is.na(Mass)) # 20 NAs correspond to 20 controls 

# save the total mass dataset

write_csv(mass_rep, "data/Qin/tidydata/mass_tidy.csv")


#                                                      ** combination **


# read in the tidied mass dataset

mass <- read_csv("data/Qin/tidydata/mass_tidy.csv") %>% 
  filter(Row != "D")

sample_data <- read_csv("data/Qin/tidydata/sample_data.csv") %>% 
  arrange(Row)

# select just the Mass column to be merged in the next step

mass_selected <- select(mass, Mass)

df_with_mass <-  bind_cols(sample_data, mass_selected) %>% 
  select(Row, Col, Time, Rep, Mass, OD) %>%  # ordering the columns
  rename(OD_sample = OD)

# import the layout

layout <- read_xlsx("C:/Users/WAN333/Documents/Thesis/Experiments/2020-01/20200107_DNS_Qin/raw_data/layout.xlsx", range = "A2:C18")

# add the sample name to the previous dataset

df_with_mass_sample <- left_join(df_with_mass, layout) %>% 
  select(Row, Col, Time, Sample, Rep, Mass, OD_sample)

# transform the control dataset for samples without enzyme

control_without_enz <- df_control %>% 
  gather(Col, OD_control, -Row, -Time)

# calculate the mean value of for the control without enzyme

mean_ctl_without_enz <- control_without_enz %>% 
  group_by(Time) %>% 
  summarise(mean_OD = mean(OD_control))

# add it to the previous dataset

ctl_without_enz <- left_join(control_without_enz, mean_ctl_without_enz) %>% 
  rename(OD_ctl_without_enz = OD_control, mean_OD_ctl_without_enz = mean_OD) %>% 
  mutate(C_ctl_for_sample_without_enz = mean_OD_ctl_without_enz / 0.2035) %>% 
  select(Time, C_ctl_for_sample_without_enz) %>% 
  unique()

# select the subset for sample without enzyme

sample_without_enz <- df_with_mass_sample %>% 
  filter(Sample == "pos_without_enz" | Sample == "neg_without_enz") %>% 
  mutate(C_sample = OD_sample / 0.2035,
         C_sample_nor = 10*C_sample / Mass)

# combine the sample_without_enz with ctl_without_enz

sample_without_enz_final <- left_join(sample_without_enz, ctl_without_enz) %>% 
  mutate(C_final = C_sample_nor - C_ctl_for_sample_without_enz) %>% 
  select(Time, Sample, Rep, C_final) %>% 
  arrange(Time)

# select the control subset (which is the neg_without_enz) for neg_with_enz

control_for_neg_with_enz <- df_with_mass_sample %>% 
  filter(Sample == "neg_without_enz") %>% 
  mutate(C_ctl_with_enz = OD_sample/0.2035,
         C_ctl_with_enz_nor = 10*C_ctl_with_enz/Mass) %>% 
  group_by(Time) %>% 
  summarise(C_ctl_for_neg_with_enz = mean(C_ctl_with_enz_nor))

# select the subset neg_with_enz

neg_with_enz <- df_with_mass_sample %>% 
  filter(Sample == "neg_with_enz") %>% 
  mutate(C_sample = OD_sample/0.2035,
         C_sample_nor = 10*C_sample/Mass)

# combine the neg_with_enz and its control

neg_with_enz_final <- left_join(neg_with_enz, control_for_neg_with_enz) %>% 
  mutate(C_final = C_sample_nor - C_ctl_for_neg_with_enz) %>% 
  select(Time, Sample, Rep, C_final) %>% 
  arrange(Time)

# select the control subset (which is the pos_without_enz) for pos_with_enz

control_for_pos_with_enz <- df_with_mass_sample %>% 
  filter(Sample == "pos_without_enz") %>% 
  mutate(C_ctl_with_enz = OD_sample/0.2035,
         C_ctl_with_enz_nor = 10*C_ctl_with_enz/Mass) %>% 
  group_by(Time) %>% 
  summarise(C_ctl_for_pos_with_enz = mean(C_ctl_with_enz_nor))

# select the subset pos_with_enz

pos_with_enz <- df_with_mass_sample %>% 
  filter(Sample == "pos_with_enz") %>% 
  mutate(C_sample = OD_sample/0.2035,
         C_sample_nor = 10*C_sample/Mass)

# combine the pos_with_enz and its control

pos_with_enz_final <- left_join(pos_with_enz, control_for_pos_with_enz) %>% 
  mutate(C_final = C_sample_nor - C_ctl_for_pos_with_enz) %>% 
  select(Time, Sample, Rep, C_final) %>% 
  arrange(Time)

# combine the three subsets together

half_combine <- bind_rows(pos_with_enz_final, neg_with_enz_final)

full_combine <- bind_rows(half_combine, sample_without_enz_final)

# calculate the hydrolysis extent

cal_HE <- full_combine %>% 
  mutate(HE = C_final / 11.1 * 100)


cal_var <- cal_HE %>% 
  group_by(Sample, Time) %>% 
  mutate(Mean_HE = mean(HE, na.rm = TRUE),
         Sd_HE = sd(HE, na.rm = TRUE),
         Cov = Sd_HE / Mean_HE * 100,
         Se_HE = Sd_HE/sqrt(3))

write_csv(cal_var, "analysis/Qin/cal_var.csv")


#                                                      ** plot **


# import the dataset

cal_var <- read_csv("analysis/Qin/cal_var.csv") %>% 
  select(Sample, Time, Mean_HE, Se_HE) %>% 
  unique()

mean_HE_plot <- ggplot(data = cal_var, 
                      aes(x = Time, 
                          y = Mean_HE,
                          group = Sample,
                          color = Sample)) +
  geom_point(size = 1, shape = 1) + # add the transparency
  geom_line(size = 0.005, alpha = 0.8) +
  geom_errorbar(aes(x = Time, 
                    ymin = Mean_HE - Se_HE, 
                    ymax = Mean_HE + Se_HE), 
                width=20,
                color = "red") +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) + ## set the range of the y axis
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

# save the plot

ggsave("figures/mean_HE_plot.png", 
       plot = mean_HE_plot, 
       width = 15, 
       height = 15, 
       units = "cm") 


#                                                      ** stat **


cal_var <- read_csv("analysis/Qin/cal_var.csv") 

# select two subsets

# with enzyme at 1440min

with_enz <- cal_var %>% 
  filter(Time == 1440) %>% 
  filter(Sample == "pos_with_enz" | Sample == "neg_with_enz")

# anova test

aov1 <- aov(HE ~ Sample, data = with_enz)

summary(aov1)

# without enzyme at 1800min

without_enz <- cal_var %>% 
  filter(Time == 1800) %>% 
  filter(Sample == "pos_without_enz" | Sample == "neg_without_enz")

# anova test

aov2 <- aov(HE ~ Sample, data = without_enz)

summary(aov2)

 

#                                                      ** tidy the dataset **


library(tidyverse)

library(readxl)

filename1 <- "C:/Users/WAN333/Documents/Thesis/Experiments/2020-01/20200107_DNS_Qin/raw_data/20200107_DNS_Qin_t"
filename2 <- 'min.xlsx'
# assigning the unchangeable part of the path

filetime <- c('20','60','120','180') 
# assigning the changeable part of the path

df <- read_excel(paste0(filename1,'0',filename2), range = "A15:E19") 
# read the 1st spreadsheet et and assign it to a new dataframe

df <- mutate(df, time = 0)
# add two columns: plate number and time point for the 1st spreadsheet

for (fltime in filetime) {
  filename_real <- paste0(filename1,fltime,filename2)
  print(filename_real)
  temp_data <- read_excel(filename_real, range = "A15:E19",na = 'NA')
  temp_data <- mutate(temp_data, time = as.numeric(fltime))
  df <- bind_rows(df, temp_data)
}
# creat a loop to import all the spreadsheet
# add two columns: plate number and time point from the 2nd spreadsheet on 
# bind them by raws

df <- rename(df, raw = ...1) 
# modify the default name for the first col & raw


df <- gather(df, col, OD, -raw, -time) 
# transfer the data into two columns 

df <- select(df, raw, col, time, OD) %>% 
  rename(Col = col,
         Row = raw,
         Time = time) %>% 
  arrange(Row)
# ordering the column names and arrange by the raw

write_csv(df, "data/Qin/tidydata/DNS_data.csv")

# check the NAs

filter(df,is.na(OD)) %>% 
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

mass_rep  <- mass[rep(seq_len(nrow(mass)), each = 5), ] # 16*5 = 80 rows

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

mass <- read_csv("data/Qin/tidydata/mass_tidy.csv") 

df <- read_csv("data/Qin/tidydata/DNS_data.csv")

# select just the Mass column to be merged in the next step

mass_selected <- select(mass, Mass)

df_with_mass <-  bind_cols(df, mass_selected) %>% 
  select(Row, Col, Time, Mass, OD) 
# ordering the columns

# import the sample name

sample_name <- read_xlsx("C:/Users/WAN333/Documents/Thesis/Experiments/2020-01/20200107_DNS_Qin/raw_data/sample_name.xlsx", range = "A2:C18")

# add the sample name to the previous dataset

df_with_mass_sample <- left_join(df_with_mass, sample_name) %>% 
  select(Row, Col, Sample, Time, Mass, OD)

# extract the controls

controls_with_enz <- df_with_mass_sample %>% 
  filter(Row == "D") %>% 
  filter(Sample == "pos_with_enz" | Sample == "neg_with_enz")

controls_without_enz <- df_with_mass_sample %>% 
  filter(Row == "D") %>% 
  filter(Sample == "pos_without_enz" | Sample == "neg_without_enz")

# calculate the mean value of for each control

with_enz <- controls_with_enz %>% 
  group_by(Time) %>% 
  summarise(mean_OD = mean(OD))

controls_with_enz <- left_join(controls_with_enz, with_enz)

without_enz <- controls_without_enz %>% 
  group_by(Time) %>% 
  summarise(mean_OD = mean(OD))

controls_without_enz <- left_join(controls_without_enz, without_enz)

# select the useful columns

controls_with_enz <- controls_with_enz %>% 
  select(Sample, Time, mean_OD)

controls_without_enz <- controls_without_enz %>% 
  select(Sample, Time, mean_OD)

# extract the test sample with enzyme

sample_with_enz <- df_with_mass_sample %>% 
  filter(Sample == "pos_with_enz" | Sample == "neg_with_enz") %>% 
  filter(Row != "D") # remove the row for control

sample_without_enz <- df_with_mass_sample %>% 
  filter(Sample == "pos_without_enz" | Sample == "neg_without_enz") %>% 
  filter(Row != "D") # remove the row for control

# combine with the control data

sample_with_enz <- left_join(sample_with_enz, controls_with_enz)

sample_without_enz <- left_join(sample_without_enz, controls_without_enz)

# combine the previous two datasets together

total_df <- bind_rows(sample_with_enz, sample_without_enz)

# save the dataset

write_csv(total_df, "data/Qin/tidydata/combined_data.csv")


#                                                      ** calculation **


# import the dataset

total_df <- read_csv("data/Qin/tidydata/combined_data.csv") %>% 
  rename(mean_OD_control = mean_OD)

# calculate the concentration (C) of the reducing sugar for the sample and the blank: C = OD/slope

df_cal <- total_df %>% 
  mutate(C_sample = OD / 0.2, # calculate the concentration using the mean slope by plate
         C_control = mean_OD_control / 0.2,
         
         # the mass of the sample was weighed between 9.5mg and 10.5 mg, which are all normalized to 10mg while calculating the concentration
         
         C_spl_nor = 10 * C_sample / Mass)

# substrate the control and calculate the hydrolysis extent

df_cal_HE <- df_cal %>% 
  mutate(C = C_spl_nor - C_control, 
         # calculate the final concentration using the mean value of the blank
         HE = C / (10 / 0.9) * 100) # calculate the hydrolysis extent
# in theory, the HE should be 10/0.9 = 11 g/L

# calculate the mean HE of the test samples 

mean_HE <- df_cal_HE %>% 
  group_by(Col,Time) %>% 
  summarise(mean_HE = mean(HE)) %>% 
  ungroup()

# add the mean_HE to the df_cal_HE dataset

df_cal_HE_mean <- left_join(df_cal_HE, mean_HE)

# save the dataset

write_csv(df_cal_HE_mean, "analysis/Qin/data_cal_HE_mean.csv")


#                                                      ** plot **


# import the dataset

df_cal_HE_mean <- read_csv("analysis/Qin/data_cal_HE_mean.csv") %>% 
  select(Sample, Time, mean_HE) %>% 
  unique()

HE_line_15P <- ggplot(data = df_cal_HE_mean, 
                      aes(x = Time, 
                          y = mean_HE,
                          group = Sample,
                          color = Sample)) +
  geom_point(size = 1, shape = 1) + # add the transparency
  geom_line(size = 0.005, alpha = 0.8) +
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

ggsave("figures/line-plot_15P.png", 
       plot = HE_line_15P, 
       width = 15, 
       height = 15, 
       units = "cm") 
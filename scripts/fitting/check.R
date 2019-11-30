

#                                           ** the curve fitting doesn't work **

#                                         ** let's check something from the plot **


# we noticed that according to the line_plot_15P, some poinsts at 240min are lower than 180min, let's check this

# read in the dataset

data_15P <- read_csv("data/tidydata/data_15P_outlier_deleted.csv")

data_15P %>% 
  filter(Time == 180 | Time == 240) %>% 
  select(Plate, Row, Column, Sample, ID, Time, HE) %>% 
  spread(Time, HE) %>% 
  filter('240' < '180') # 0 results, which is good, indicating that there are no points at 240min that are higher than 180min

# going back to the line plot, why the plot looks weird ?

# let's paste the codes for the line plot here and have a check



#                                              ** check the line plot by adding a geom_text **



# read in the calculated dataset

data_15P <- read_csv("data/tidydata/data_15P_outlier_deleted.csv")

# add a new column that descripe the status of each samples 

data_status <- data_15P %>% 
  mutate(status = case_when( # number of conditions (left), discription of the results in that condition
    Sample == "C+" ~ "Positive control", 
    Sample == "C-" ~ "Negative control",
    TRUE ~ "Sample"
  ))

# add a column that distinguish each sample

final_data <- data_status %>% 
  mutate(Well = paste(Plate, Row, Column, sep = "_")) 

# plot

HE_line_15P <- ggplot(data = final_data, 
                      aes(x = Time, 
                          y = HE,
                          group = Well,
                          color = status)) + # colored by the status (distinguish the control from the other samples)
  geom_point(size = 1, shape = 1) + # add the transparency
  geom_line(size = 0.005) +
  geom_text(aes(label = Sample)) + # to identidy the sample name for the weird shape (curve)
  scale_y_continuous(limits = c(0,100)) + ## set the range of the y axis
  scale_x_continuous(limits = c(0, 2000)) +
  ylab("Hydrolysis extent (%)") + ## change the label for the y axis
  xlab("Time (min)") + ## change the name of the x axis
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5)，
        panel.background = element_rect(fill = "white"),
        axis.ticks=element_line(
          colour="black",
          size=.5)) +
  labs(x = "Time (min)", y = "Hydrolysis extent (%)") +
  theme(axis.text.x = element_text(color="black", size=10), 
        axis.text.y = element_text(color="black", size=10)) +
  scale_color_discrete(labels = c("Negtive control", "Sample", "Positive control")) +
  theme(legend.key = element_blank(),
        legend.position = "bottom")

# seems they are : C+, 103, 187 ... (too many samples, difficult too see)

# let's plot just C+ and have a quick check

pos <- final_data %>% 
  filter(Sample == "C+") %>% 
  filter(Time == 180 | Time == 240) 

pos %>% 
  ggplot(aes(x = Time,
             y = HE,
             group = Well)) +
  geom_line() +
  geom_point() 
# according to the graph, seems there is one sample that HE at 180min is = to the HE at 240, let's check

pos_arranged <- pos %>% 
  arrange(HE)

# ok, found it ! it's the sample in Well 11_A_3 that has an identical value at 180min and 240min, could be a mistake when tidying the data

# let's go back to the plate 11 and check



#                                               ** check the Plate 11 **

plate11 <- read_csv("data/tidydata/plate11.csv")

# let's find A_3

plate11 %>% 
  filter(raw == "A" & col == 3) # yes, identical values found 

# go back and check the raw data

# ok...the spreadsheet of plate 11 at 180min and 240min are the same, must be a mistake when downloading the documents

# all right...Coming back from the lab, I saved the wrong spreadsheet for plate 11 at 240min, glad I found it out 

# and now we can correct the mistake


#                        ** check other points **

# find the lowest value at 180min

arrange <- data_15P %>% 
  filter(Time == 180) %>% 
  arrange(HE) # 2 very low values here: 92 in plate 1 and 136 in plate 13

# draw the line plot for sample 92 and 136

Sample_136 <- data_15P %>% 
  filter(Sample == 136) %>% 
  mutate(Well = paste(Plate, Row, Column, sep = "_"))

ggplot(data = Sample_136, 
       aes(x = Time, 
           y = HE,
           group = Well,
           color = as.factor(Plate))) + # colored by the status (distinguish the control from the other samples)
  geom_point(size = 1, shape = 1) + # add the transparency
  geom_line(size = 0.005) +
  scale_y_continuous(limits = c(0,100)) + ## set the range of the y axis
  scale_x_continuous(limits = c(0, 2000)) +
  ylab("Hydrolysis extent (%)") + ## change the label for the y axis
  xlab("Time (min)") + ## change the name of the x axis
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5)，
        panel.background = element_rect(fill = "white"),
        axis.ticks=element_line(
          colour="black",
          size=.5)) +
  labs(x = "Time (min)", y = "Hydrolysis extent (%)") +
  theme(axis.text.x = element_text(color="black", size=10), 
        axis.text.y = element_text(color="black", size=10)) +
  scale_color_discrete(labels = c("Negtive control", "Sample", "Positive control")) +
  theme(legend.key = element_blank(),
        legend.position = "bottom")
# according to the plot, the values of the plate 13 at 120min and 180min seems quite similar
# let's have a look of plate 13 

# read in the data of the plate 13

df <- read_csv("data/tidydata/data_15P_cal.csv")

Sample136 <- df %>% 
  filter(Time == 120 | Time == 180) %>% 
  filter(Sample == "136")
# yes, they are very close, but they are different valaues, seems no problem

# let's check that indivudual plot and find if there are other weird looking samples 

# yes, several samples might have some problems 

# wrong connecting lines : Sample 82*, 

# let's check sample 82*

Sample81 <- data_15P %>% 
  filter(Sample == "82*") %>% 
  filter(!is.na(HE)) %>% # remove the NAs
  mutate(Well = paste(Plate, Row, Column, sep = "_"))

ggplot(data = Sample81, 
      aes(x = Time, 
          y = HE,
          group = Well,
          color = as.factor(Plate))) + # colored by the status (distinguish the control from the other samples)
  geom_point(size = 1, shape = 1) + # add the transparency
  geom_line(size = 0.005) +
  scale_y_continuous(limits = c(0,100)) + ## set the range of the y axis
  scale_x_continuous(limits = c(0, 2000)) +
  ylab("Hydrolysis extent (%)") + ## change the label for the y axis
  xlab("Time (min)") + ## change the name of the x axis
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5)，
        panel.background = element_rect(fill = "white"),
        axis.ticks=element_line(
          colour="black",
          size=.5)) +
  labs(x = "Time (min)", y = "Hydrolysis extent (%)") +
  theme(axis.text.x = element_text(color="black", size=10), 
        axis.text.y = element_text(color="black", size=10)) +
  scale_color_discrete(labels = c("Negtive control", "Sample", "Positive control")) +
  theme(legend.key = element_blank(),
        legend.position = "bottom")
# this plot is ok
# the previous saved line plot was wrong due to the wrong grouping 
# should group by Well not by Plate 
# ok, this problem is solved

#                         ** samples remained to check **

#                           ** 103, 134, 127, 165,26,136 **

#                           ** let's check one by one **

# check sample 103 at 180 and 240min

Sample103 <- data_15P_outlier_deleted %>% 
  filter(Sample == "103") %>% 
  filter(Time == 180 | Time == 240) # the weird one is Well 11_C_11

# let's go back and check the raw data at this Well

# after checking, the 11_C_11 at 180min is too high, must be contaminated
# and so this one should be replaced by NA or removed from the dataset

# check sample 134 at 240 and 360min | 120 and 180min

Sample134 <- data_15P_outlier_deleted %>% 
  filter(Sample == "134") %>% 
  filter(Time == 240 | Time == 360) # the problem came from Well 11_E_9

# let's go back and check the raw data at this Well

# after checking, the 11_E_9 at 360min is too low, unknown reason
# and so this one should be replaced by NA or removed from the dataset

Sample134_bis <- data_15P_outlier_deleted %>% 
  filter(Sample == "134") %>% 
  filter(Time == 120 | Time == 180)

# after recheck, the 11_E_9 ar 180min is too low too, let's remove this as well

# check sample 127 at 120 and 180min

Sample127 <- data_15P_outlier_deleted %>% 
  filter(Sample == "127") %>% 
  filter(Time == 120 | Time == 180) # the problem came from Well 13_C_11

# let's go back and check the raw data at this Well

# after checking, the 13_C_11 at 120min is too high, must be contaminated 
# and so this one should be replaced by NA or removed from the dataset

# check sample 165 at 180 and 240min

Sample165 <- data_15P_outlier_deleted %>% 
  filter(Sample == "165") %>% 
  filter(Time == 180 | Time == 240) # the problem came from Well 8_G_5

# let's go back and check the raw data at this Well

# after checking, the 8_G_5 at 180min is too high, must be contaminated 
# and so this one should be replaced by NA or removed from the dataset

# check sample 26 at 240 and 360min

Sample26 <- data_15P_outlier_deleted %>% 
  filter(Sample == "26") %>% 
  filter(Time == 240 | Time == 360) # the problem came from Well 10_E_9

# let's go back and check the raw data at this Well

# after checking, the 10_E_9 at 360min is too low, unknown reason 
# and so this one should be replaced by NA or removed from the dataset

# check sample 136 at 120 and 180min

Sample136 <- data_15P_outlier_deleted %>% 
  filter(Sample == "136") %>% 
  filter(Time == 120 | Time == 180) # the problem came from Well 13_G_3

# let's go back and check the raw data at this Well

# after checking, the 13_G_3 at 180min is too low, unknown reason 
# and so this one should be replaced by NA or removed from the dataset

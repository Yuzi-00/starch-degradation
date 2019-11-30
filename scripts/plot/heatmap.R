library(tidyverse)

# import the dataset

data_6P <- read_csv("data/tidydata/data_6P_cal_3nd.csv")

# select the colmuns needed

data_selected <- data_6P %>% 
  filter(Sample != "C+" & Sample != "C-") %>%  # remove the controls
  select(Plate, Row, Col, Time, HE) %>% # select the column needed
  rename(Column = Col) 

data_selected



# choose a time point and remove the "outlier"

data_t <- data_selected %>% 
  filter(Time == 360) %>% # randomly choose a time point
  filter(!(Column == 7 & Row == "F" & Plate == 6)) %>% # remove an "outlier"
  filter(!(Column == 1 & Row == "F" & Plate == 1)) %>% # remove an "outlier"
  filter(!(Column == 9 & Row == "H" & Plate == 6)) # remove an "outlier"

### plotting

data_t %>% 
  ggplot(aes(x = Column, y = Row, fill = HE)) + 
  scale_fill_continuous(name = "Hydrolysis extent", type = "viridis") + # set the legend title and the fill 
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11)) +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.15, "cm"),
        legend.key.width = unit(0.8, "cm")) +
  geom_tile(color = "white", size = 0.1) +
  facet_wrap(~Plate) +
  theme(strip.background = element_blank())

# check the number of the observation

with(data_t,table(Column, Row, Plate)) # one observation/col/row/per



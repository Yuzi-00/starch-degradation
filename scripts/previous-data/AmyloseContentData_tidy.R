library(tidyverse)

library(readxl)


####################################################### tidy the data #############################################################


# read in the amylose content dataset (data from the flour, not the starch, so do not have the corresponding sample names)

amy <- read_xlsx("C:/Users/WAN333/Documents/Thesis/Thesis infomation/MAGIC population/Data_MAGIC Population/AmyloseContentData+Lmax_Flour.xlsx")

# tidy the dataset

amy_selected <- amy %>% 
  select(ID, `amylose content estimate`) %>% # select just the cols ID and amycontent
  filter(ID != "BLANK", ID != "STDCURVE") # remove the blanks and the standard curves

# checking if there are some duplicated row

amy_selected[duplicated(amy_selected),] # no duplicated row in this dataset

# add a column that contains the mean value of the amylose content

amy_tidy <- amy_selected %>% 
  group_by(ID) %>% 
  mutate(mean_amy = mean(`amylose content estimate`, na.rm = TRUE)) %>% 
  # each ID has a few amy% 
  # add a new column that contains the mean amylose content for each ID
  arrange(mean_amy) %>% 
  rename(amylose_content = `amylose content estimate`)

# write out the tidy dataset

write_csv(amy_tidy, "data/tidydata/previous_data/amylose_tidy.csv")

#################################################### plot the amylose content ######################################################

amylose_plot <- amy_tidy %>% 
  ggplot() +
  geom_point(aes(x = reorder(ID, amylose_content), # order the ID by amylose content (high to low)
                 y = amylose_content),
             alpha = 0.3,
             size = 0.5,
             shape = 1) + # hollow circles
  labs(x = "ID", y = "Amylose content (%)") + # change the labels for the x and y axis
  geom_point(aes(x = ID, 
             y = mean_amy,
             color = "Mean values"), # plot the mean amylose content as well  
             
             size = 0.5,
             shape = 1) +
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black", size = 0.3),
        panel.background = element_rect(fill = "white"),
        axis.ticks=element_line(
          colour="black",
          size=.3)) +
  theme(axis.text.y = element_text(color = "black", size = 5),
        axis.title = element_blank()) +
  scale_y_continuous(limits = c(20, 40), expand = c(0, 0)) +
  scale_x_discrete( +
  theme(legend.key = element_blank(),
        legend.text = element_text(size = 5)) # remove that grep rectangle in the legend

# write out the plot

ggsave("figures/amylose_content.png",
       plot = amylose_plot,
       width = 10, 
       height = 6,
       dpi = 300,
       units = "cm")

       


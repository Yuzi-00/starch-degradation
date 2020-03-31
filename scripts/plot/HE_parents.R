
library(tidyverse)


#                                      ** scatter plot of the HE **


# import the dataset

df <- read_csv("analysis/total_new_update.csv") 

# replace the NAs within the Sample column by "control"

df$Sample <- as.character(df$Sample)

df$Sample[is.na(df$Sample)] <- "control"

# select the parents and controls

df_sel <- df %>%
  filter(Category != "descendant")

# plot Westonia only 

wes <- df_sel %>%
  filter(ID == "Westonia")

HE <- ggplot(data = wes, 
             aes(x = Time, 
                 y = Hydro_extent,
                 color = Well)) + # colored by the status (distinguish the control from the other samples)
  geom_point(size = 1, shape = 1) + # add the transparency
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) + ## set the range of the y axis
  scale_x_continuous(limits = c(0, 2000), expand = c(0, 0)) +
  ylab("Hydrolysis extent (%)") + ## change the label for the y axis
  xlab("Time (min)") +
  theme(axis.text.x = element_text(color="black", size=10),
      axis.text.y = element_text(color="black", size=10),
      axis.title = element_text(color = "black", size = 10)) +
  ggtitle("Westonia hydrolysis extent (3 replicates)")

HE

# save the plot

ggsave("figures/HE_westonia.png", 
       plot = HE, 
       width = 15, 
       height = 8, 
       units = "cm") 
HE

# plot Baxter only 

bax <- df_sel %>%
  filter(ID == "Baxter")

HE <- ggplot(data = bax, 
             aes(x = Time, 
                 y = Hydro_extent,
                 color = Category)) + # colored by the status (distinguish the control from the other samples)
  geom_point(size = 1, shape = 1) + # add the transparency
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) + ## set the range of the y axis
  scale_x_continuous(limits = c(0, 2000), expand = c(0, 0)) +
  ylab("Hydrolysis extent (%)") + ## change the label for the y axis
  xlab("Time (min)")

HE

# plot Chara only 

cha <- df_sel %>%
  filter(ID == "Chara")

HE <- ggplot(data = cha, 
             aes(x = Time, 
                 y = Hydro_extent,
                 color = Category)) + # colored by the status (distinguish the control from the other samples)
  geom_point(size = 1, shape = 1) + # add the transparency
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) + ## set the range of the y axis
  scale_x_continuous(limits = c(0, 2000), expand = c(0, 0)) +
  ylab("Hydrolysis extent (%)") + ## change the label for the y axis
  xlab("Time (min)")

HE

# plot these three parents together 

parent <- df_sel %>%
  filter(Sample != "control")

HE <- ggplot(data = parent, 
             aes(x = Time, 
                 y = Hydro_extent,
                 color = Category)) + # colored by the status (distinguish the control from the other samples)
  geom_point(size = 1, shape = 1) + # add the transparency
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) + ## set the range of the y axis
  scale_x_continuous(limits = c(0, 2000), expand = c(0, 0)) +
  ylab("Hydrolysis extent (%)") + ## change the label for the y axis
  xlab("Time (min)") +
  theme(axis.text.x = element_text(color="black", size=10),
        axis.text.y = element_text(color="black", size=10),
        axis.title = element_text(color = "black", size = 10)) +
          ggtitle("Parents hydrolysis extent")

HE

# save the plot

ggsave("figures/HE_parents.png", 
       plot = HE, 
       width = 16, 
       height = 8, 
       units = "cm") 

# plotting

HE_scatter_15P <- ggplot(data = df_sel , 
                         aes(x = Time, 
                             y = Hydro_extent,
                             color = Category)) + # colored by the status (distinguish the control from the other samples)
  geom_point(size = 1, shape = 1) + # add the transparency
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
        legend.position = "bottom") +
  theme(plot.margin = unit(c(5.5,12,5.5,5.5), "pt"))

HE_scatter_15P

# save the plot

ggsave("figures/scatter-plot_15P.png", 
       plot = HE_scatter_15P, 
       width = 10, 
       height = 8, 
       units = "cm") 
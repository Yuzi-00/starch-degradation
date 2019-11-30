
mean_HE_6P %>% 
ggplot(aes(x = Time, 
           y = mean_HE,
           group = Sample)) + 
  geom_line() +
  geom_point() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5)，
        panel.background = element_rect(fill = "white", color = "black"),
        axis.ticks=element_line(
          colour="black",
          size=.5)) +
  labs(x = "Time (min)", y = "Hydrolysis extent (%)") +
  theme(axis.text.x = element_text(color="black", size=10), 
        axis.text.y = element_text(color="black", size=10))


# filter the data frame to have just the sample 92

## sample 77 

sample_77 <- hydro %>% 
  filter(Sample == "77") %>% 
  filter(!is.na(HE)) ### remove the missing value

model_77 <- nls(formula = HE ~ Xinf*(1-exp(-k*Time**(1-h))), 
                data = sample_77,
                start = list(Xinf = 100,
                             k = 0.001,
                             h = 0.001)) ### the estimated Xinf is 72.8686, h is 0.0977, k is 0.0044

## positive control

pos_control <- hydro %>% 
  filter(Sample == "C+") %>% 
  filter(!is.na(HE))

model_pos <- nls(formula = HE ~ Xinf*(1-exp(-k*Time**(1-h))), 
                 data = pos_control,
                 start = list(Xinf = 100,
                              k = 0.001,
                              h = 0.001)) ### the estimated Xinf is 84.8588, h is -0.2282, k is 0.0016

## negative control

neg_control <- hydro %>% 
  filter(Sample == "C-") %>% 
  filter(!is.na(HE))

model_neg <- nls(formula = HE ~ Xinf*(1-exp(-k*Time**(1-h))), 
                 data = neg_control,
                 start = list(Xinf = 100,
                              k = 0.001,
                              h = 0.001)) ### the estimated Xinf is 55.3105, h is 0.1469, k is 0.0088

# creat a tibble that contains these three samples together

three_samples <- hydro %>% 
  filter(Sample == "C+" | Sample == "C-" | Sample == "77") %>% 
  filter(!is.na(HE))

# define the sample col as a factor

three_samples$status <- factor(three_samples$status, levels = c("", "Sample", "Negative control"))

# plotting

ggplot(data = three_samples, aes(x = Time, y = HE, alpha = 0.6, color = Sample)) +
  
  ## add sample 77
  
  geom_point(size = 2, shape = 1) + 
  scale_color_manual(labels = c("Positive control", "Sample","Negative control"), values = c("#fc8d62", "#66c2a5", "#8da0cb")) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 2000), expand = c(0, 0)) +
  ylab("Hydrolysis extent (%)") + ### change the label for the y axis
  xlab("Time (min)") + ### change the name of the x axis
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.ticks=element_line(colour="black", size=.5)) +
  labs(x = "Time (min)", y = "Hydrolysis extent (%)") +
  theme(axis.text.x = element_text(color="black", size=10), 
        axis.text.y = element_text(color="black", size=10)) +
  theme(legend.key = element_blank(),
        legend.position = "bottom") +
  theme(plot.margin = unit(c(5.5,12,5.5,5.5), "pt")) +
  
  ## add negative control
  
  stat_function(data = neg_control, fun = function(Time){55.3105*(1-exp(-0.0088*Time**(1-0.1469)))}, col = "#66c2a5", size = 0.7) +
  
  ## add sample
  
  stat_function(data = sample_77, fun = function(Time){72.8686*(1-exp(-0.0044*Time**(1-0.0977)))}, col = "#fc8d62", size = 0.7) +
  
  ## add positive control
  
  stat_function(data = pos_control, fun = function(Time){84.8588*(1-exp(-0.0016*Time**(1+0.2282)))}, col = "#8da0cb", size = 0.7) 
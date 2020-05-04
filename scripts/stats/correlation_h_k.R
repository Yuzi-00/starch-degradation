
library(tidyverse)

# import the dataset

df <- read_csv("analysis/total_new_convert.csv")

# add a new column K (=log(k))

df <- df %>% 
  mutate(K = log(k))

# add a column to distinguish test samples from the controls. Go back and conserve the WellGroupType
# column 

df <- df %>%
  mutate(WellGroupType = case_when(
    Sample == "C+" ~ "Control+",
    Sample == "C-" ~ "Control-",
    TRUE ~ "Test sample"
  ))

#### test samples ####

# select the test samples

test_sample <- df %>%
  filter(Sample != "C+" & Sample != "C-")

# plot h and k

test_sample %>% 
  ggplot(aes(x = h,
             y = k)) +
  geom_point() 

# h and k are strongly correlated 

# plot h and K

test_sample %>% 
  ggplot(aes(x = h, 
             y = K)) +
  geom_point()

# check the correlation between h and K 

cor(test_sample$h, test_sample$K, use = "complete.obs") # strong correlation 0.97

# linear regression

lm_h_k <- lm(data = test_sample, K ~ h)

summary(lm_h_k) # K = 5.31h - 5.93

# add the regression line to the plot

cor_h_K <- test_sample %>% 
  ggplot(aes(x = h, 
             y = K)) +
  geom_point() +
  geom_abline(aes(slope = 5.31, intercept = -5.93), col = "red")

cor_h_K # K = 5.31h - 5.93

# save the plot

ggsave("figures/correlation_h_k.png", 
       plot = cor_h_K, 
       width = 15, 
       height = 15, 
       units = "cm") 

#### positive controls ####

# select the positive controls

pos_control <- df %>%
  filter(Sample == "C+")

# plot h and k

pos_control %>% 
  ggplot(aes(x = h,
             y = k)) +
  geom_point() 

# plot h and K

pos_control %>% 
  ggplot(aes(x = h, 
             y = K)) +
  geom_point()

# check the correlation between h and K 

cor(pos_control$h, pos_control$K, use = "complete.obs") # strong correlation 0.97

# linear regression

lm_h_k <- lm(data = pos_control, K ~ h)

summary(lm_h_k) # K = 5.48h - 5.14

# add the regression line to the plot

cor_h_K <- pos_control %>% 
  ggplot(aes(x = h, 
             y = K)) +
  geom_point() +
  geom_abline(aes(slope = 5.48, intercept = -5.14), col = "red")

cor_h_K # K = 5.48h - 5.14

# save the plot

ggsave("figures/hk_PosControl.png", 
       plot = cor_h_K, 
       width = 15, 
       height = 15, 
       units = "cm") 

#### negative controls ####

# select the negative controls

neg_control <- df %>%
  filter(Sample == "C-")

# plot h and k

neg_control %>% 
  ggplot(aes(x = h,
             y = k)) +
  geom_point() 

# plot h and K

neg_control %>% 
  ggplot(aes(x = h, 
             y = K)) +
  geom_point()

# check the correlation between h and K 

cor(neg_control$h, neg_control$K, use = "complete.obs") # strong correlation 0.98

# linear regression

lm_h_k <- lm(data = neg_control, K ~ h)

summary(lm_h_k) # K = 5.7h - 5.64

# add the regression line to the plot

cor_h_K <- neg_control %>% 
  ggplot(aes(x = h, 
             y = K)) +
  geom_point() +
  geom_abline(aes(slope = 5.7, intercept = -5.64), col = "red")

cor_h_K # K = 5.48h - 5.14

# save the plot

ggsave("figures/hk_NegControl.png", 
       plot = cor_h_K, 
       width = 15, 
       height = 15, 
       units = "cm") 

#### all in the same plot ###

# scatter plot

plot01 <- df %>% 
  ggplot(aes(x = h, 
             y = K,
             color = WellGroupType)) +
  scale_color_brewer(palette = "Dark2") +
  geom_point(alpha = 0.5)

plot01

# save the plot

ggsave("figures/hk.png", 
       plot = plot01, 
       width = 17, 
       height = 12, 
       units = "cm") 

# add the ablines

plot02 <- plot01 +
  geom_abline(aes(slope = 5.31, intercept = -5.93), col = "#7570b3") + # for test samples
  geom_abline(aes(slope = 5.48, intercept = -5.14), col = "#d95f02") + # for positive controls
  geom_abline(aes(slope = 5.7, intercept = -5.64), col = "#1b9e77") # for negative controls

plot02

# save the plot

ggsave("figures/hk_abline.png", 
       plot = plot02, 
       width = 17, 
       height = 12, 
       units = "cm") 

# install.packages("magrittr")

library(ggpubr)

plot01 <- test_sample %>% 
  ggplot(aes(x = h, 
             y = K)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point(alpha = 0.5) +
  stat_cor(label.y = -6)+ #this means at 35th unit in the y axis, the r squared and p value will be shown
  stat_regline_equation(label.y = -7) 

P <- ggplot() +
  geom_point(data = test_sample,
             aes(x = k, 
                 y = h,
                 color = "Test_sample"),
             alpha = 0.5) +
  geom_smooth(data = test_sample,
              aes(x = k, 
                  y = h),
              method = "lm", se=FALSE, color="black", 
              formula = y ~ log(x, base = exp(1))) +
  stat_cor(data = test_sample, aes(x = k, 
                                   y = h),
           label.x = 0.003, label.y = 0.23) + #this means at 35th unit in the y axis, the r squared and p value will be shown
  stat_regline_equation(data = test_sample, aes(x = k, 
                                                y = h),
                        formula = y ~ log(x, base = exp(1)),
                        label.x = 0.003, label.y = 0.27) +
  # add positive control
  geom_point(data = pos_control,
             aes(x = k, 
                 y = h,
                 color = "Pos_control"),
             alpha = 0.5,
             ) +
  geom_smooth(data = pos_control,
              aes(x = k, 
                  y = h
              ),
              method = "lm", se=FALSE, color="red", 
              formula = y ~ log(x, base = exp(1))) +
  stat_cor(data = pos_control, aes(x = k, 
                                   y = h),
           label.x = 0.005, label.y = -0.15,
           color="red") + #this means at 35th unit in the y axis, the r squared and p value will be shown
  stat_regline_equation(data = pos_control, aes(x = k, 
                                                y = h),
                        formula = y ~ log(x, base = exp(1)),
                        label.x = 0.005, label.y = -0.1, color="red") +
  # add negative control
  geom_point(data = neg_control,
             aes(x = k, 
                 y = h,
                 color = "Neg_control"),
             alpha = 0.5
             ) +
  geom_smooth(data = neg_control,
              aes(x = k, 
                  y = h
              ),
              method = "lm", se=FALSE, color="blue", 
              formula = y ~ log(x, base = exp(1))) +
  stat_cor(data = neg_control, aes(x = k, 
                                   y = h),
           label.x = 0.01,
           label.y = 0.1,
           color="blue") + #this means at 35th unit in the y axis, the r squared and p value will be shown
  stat_regline_equation(data = neg_control, aes(x = k, 
                                                y = h),
                        formula = y ~ log(x, base = exp(1)),
                        label.x = 0.01, label.y = 0.15, color="blue") +
  scale_colour_manual(name="Sample type",
                      values=c(Pos_control="red", Test_sample="black", Neg_control = "blue")) +
  geom_hline(yintercept=0.04, color = "black", linetype = "dashed", size = 0.7)

P

# save the plot

ggsave("figures/hk_abline.png", 
       plot = P, 
       width = 25, 
       height = 20, 
       units = "cm") 

#### residual plot ####

# A residual plot lets you see if your data appears homoscedastic. Homoscedasticity 
# means that the residuals, the difference between the observed value and the predicted 
# value, are equal across all values of your predictor variable. If your data are 
# homoscedastic then you will see the points randomly scattered around the x axis. If 
# they are not (e.g. if they form a curve, bowtie, fan etc.) then it suggests that your 
# data doesn't meet the assumption.

# In order to make valid inferences from your regression, the residuals of the regression 
# should follow a normal distribution. The residuals are simply the error terms, or the 
# differences between the observed value of the dependent variable and the predicted value.

# When the residuals are not normally distributed, then the hypothesis that they are a random 
# dataset, takes the value NO. This means that in that case your (regression) model does not 
# explain all trends in the dataset. I guess, you don´t want unkown trends to remain in your 
# dataset. I would feel uncomfortable with that, because this would mean that your model is not 
# fully explaining the behaviour of your system. Only solution is to find a model that fully 
# explains the behaviour of your system. That means that you have to find a model, that shows 
# residuals which are normally distributed.

# linear regression

# remove the NAs

test_sample <- test_sample %>%
  na.omit() 

# linear regression 

lm_kinetics <- lm(data = test_sample, h ~ log(k, base = exp(1)))

summary(lm_kinetics) 

## in the summary, we can see the values of the intercept (1.055733) and the slope (0.177971).
## A good way to test the quality of the fit of the model is to look at the residuals or the 
## differences between the real values and the predicted values. The idea in here is that the 
## sum of the residuals is approximately zero or as low as possible. In the R summary of the lm 
## function, we can see descriptive statistics about the residuals of the model, these values show 
## us how the residuals are approximately zero.

## notice that there’s two different R², one multiple and one adjusted. One problem with the
## multiple R² is that it cannot decrease as you add more independent variables to your model, 
## it will continue increasing as you make the model more complex, even if these variables don’t 
## add anything to your predictions (like the example of the number of siblings). For this reason, 
## the adjusted R² is probably better to look at if you are adding more than one variable to the 
## model, since it only increases if it reduces the overall error of the predictions.

# transform the data into a format that ggplot() can use

fortify(lm_kinetics) # noted that there is a column called .fitted and .resid

ggplot(data = lm_kinetics,
       aes(x = .fitted, 
           y = .resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-0.1,0.1), expand = c(0, 0))  ## set the range of the y axis

# alternative method using plot() function

plot(resid(lm_kinetics))

# other plots that we can generate

qqnorm(resid(lm_kinetics)) # A quantile normal plot - good for checking normality.
                           # qqnorm(x)用于画出x的常态qq图，用以判断x是否为常态分布

## Normal Q-Q Plot: This is used to assess if your residuals are normally distributed. 
## Basically what you are looking for here is the data points closely following the 
## straight line at a 45% angle upwards (left to right). Again what to watch here is 
## any patterns that deviate from this - particularly anything that looks curvilinear 
## (bending at either end) or s shaped.

## Q-Q plots let you check that the data meet the assumption of normality. They compare 
## the distribution of your data to a normal distribution by plotting the quartiles of 
## your data against the quartiles of a normal distribution. If your data are normally 
## distributed then they should form an approximately straight line.

## 注：qqnorm为样本与样本期望的正态性对比；qqplot为两样本的正态性对比(即二者的分布
## 是否一致)

qqline(resid(lm_kinetics))

# so, according to the results, we can see that the points are randomly scattered around the x axis
# in the residual plot (no pattern here), and form a straight line in the normal qq-plot. So that
# the residuals are normally distributed.

# save the residuals 

res <- fortify(lm_kinetics) %>%
  select(h, .resid)

write_csv(res, "analysis/hkmodel_residuals.csv")



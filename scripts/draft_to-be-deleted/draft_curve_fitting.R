library(tidyverse)

data_6P <- read_csv("data/tidydata/data_6P_cal.csv")

sample_92 <- data_6P %>% 
  filter(Sample == "92") %>% 
  group_by(Time) %>% 
  summarise(mean_HE = mean(HE,  na.rm = TRUE))

sample_92 %>% 
  ggplot(aes(x = Time, y = mean_HE)) +
  geom_point() 

fit1 <- lm(data = sample_92, mean_HE ~ Time)

anova(fit1)

abline(a = 12.38237, b = 0.03217, h = sample_92$mean_HE, v = sample_92$Time, untf = FALSE, 
       col = "red")

abline(lm(data = sample_92, mean_HE ~ Time))

abline(a=coef(fit1)[1], b=coef(fit1)[2])

sample_92 %>% 
  ggplot(aes(x = Time, y = mean_HE)) +
  geom_point() +
  geom_abline(intercept = 12.38237, slope = 0.03217, col = "red")

mod <- lm(data = sample_92, formula = mean_HE ~ (1 - exp(1 - (exp(1))^(-k * Time^(1 - h)))) * x)

x <- fitted(mean_HE ~ (1 - exp(1 - (exp(1))^(-k * Time^(1 - h)))) * x)

x <- function(mean_HE = 1 - exp(1 - (exp(1))^(-k * Time^(1 - h)))*x)   

mod <- lm(data = sample_92, mean_HE ~ Time) # just try it
pre <- predict(mod)

sample_92 %>% 
  ggplot(aes(x = Time, y = mean_HE)) +
  geom_point() +
  geom_line(aes(y = pre, x = Time, col = "red")) # it works 

mod <- lm(data = sample_92, formula = (1 - exp(1 - (exp(1))^(-k * Time^(1 - h)))) * x)
# this doesn't work

mod <- function(Time, k, h, x){(1 - exp(1 - (exp(1))^(-k * Time^(1 - h)))) * x}
# enter the equation that we want to apply


nls(mean_HE ~ (1 - exp(1 - (exp(1))**(-k * Time**(1 - 0.3))))*50, 
    data = sample_92)


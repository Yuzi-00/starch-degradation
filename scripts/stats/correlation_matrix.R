
# install.packages("PerformanceAnalytics")

library("PerformanceAnalytics")

library(tidyverse)

# import the dataset

df <- read_csv("analysis/total_new_convert.csv")

#### chart.Correlation ####

df_sel <- df %>%
  select(29:31)

chart.Correlation(df_sel, histogram=TRUE, pch=19)

## the default correlation coefficient of chart.correlation is "pearson",
## we can also use the argument "method = " to change it into "kendall" or
## "sprearman"

df_sel <- df %>%
  select(20:31)

chart.Correlation(df_sel, histogram=TRUE, pch=19)

#### correlation_function ####

df_sel <- df %>%
  select(11:14, 29:31)

chart.Correlation(df_sel, histogram=TRUE, pch=19)

#### correlation_GranuleSize-kinetics ####

df_sel <- df %>%
  select(6:10, 29:31)

chart.Correlation(df_sel, histogram=TRUE, pch=19)

#### correlation_amylose-kinetics ####

df_sel <- df %>%
  select(5, 29:31)

chart.Correlation(df_sel, histogram=TRUE, pch=19)

#### correlation_amylose-HE ####

df_sel <- df %>%
  select(5, 20:28)

chart.Correlation(df_sel, histogram=TRUE, pch=19)

#### correlation_Ap-kinetics ####

df_sel <- df %>%
  select(15:18, 20:28)

chart.Correlation(df_sel, histogram=TRUE, pch=19)

# interaction of am and ap, ANOVA ?

#### ggpairs ####

# install.packages("GGally")

library(GGally)

# plot the kinetics 

df_sel <- df %>%
  select(k:Xinf)

CM <- ggpairs(df_sel, axisLabels = "show")

CM

ggsave("figures/CM_kinetics.png", CM, width = 8, height = 5)

# plot kinetics with HEs

df_sel <- df %>%
  select(HE_0min:Xinf)

CM <- ggpairs(df_sel, axisLabels = "show", showStrips = NULL)

CM

ggsave("figures/CM_kinetics_HE.png", CM, width = 8, height = 5)

# plot kinetics with granule size

df_sel <- df %>%
  select(SSA:D0.9 , k:Xinf)

CM <- ggpairs(df_sel, axisLabels = "show", showStrips = NULL)

CM

ggsave("figures/CM_kinetics_HE.png", CM, width = 8, height = 5)

# plot kinetics with RVA

df_sel <- df %>%
  select(Peak_Vis:Pasting_Temp, k:Xinf)

CM <- ggpairs(df_sel, axisLabels = "show", showStrips = NULL)

CM

ggsave("figures/CM_kinetics_HE.png", CM, width = 8, height = 5)

# plot kinetics with Ap chains

df_sel <- df %>%
  select(DP6_12:DP37_47, k:Xinf)

CM <- ggpairs(df_sel, axisLabels = "show", showStrips = NULL)

CM

ggsave("figures/CM_kinetics_HE.png", CM, width = 8, height = 5)

# plot kinetics with D1

df_sel <- df %>%
  select(D1, HE_0min:HE_1800min)

CM <- ggpairs(df_sel, axisLabels = "show", showStrips = NULL)

CM

ggsave("figures/CM_kinetics_HE.png", CM, width = 8, height = 5)

# plot amylose with RVA

df_sel <- df %>%
  select(Amylose_Con, Peak_Vis:Pasting_Temp)

CM <- ggpairs(df_sel, axisLabels = "show", showStrips = NULL)

CM

ggsave("figures/CM_kinetics_HE.png", CM, width = 8, height = 5)

# corrplot

library(corrplot)

my_subset <- df %>%
  select(-(1:4))

cor_result <- cor(my_subset, use = "complete.obs")

correlation <- corrplot(cor_result, method = "number", type = "lower",
                        tl.cex = 0.5, number.cex = 0.5) 

## tl.cex:change the size of table; number.cex:change the size of number

# save the corrplot

pdf(file = "corrplot.pdf")

corrplot(correlation, method = "number", type = "lower", tl.cex = 0.5, 
         title = "relationships", 
         mar = c(0,0,1,0), number.cex = 0.5, number.digits = 2)

dev.off()

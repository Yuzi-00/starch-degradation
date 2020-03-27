
library("FactoMineR")
library("factoextra")

data(decathlon2) # the data is built-in in Factoextra

decathlon2

decathlon2.active <- decathlon2[1:23, 1:10]

# PCA计算
res.pca <- PCA(decathlon2.active, graph = FALSE)
# 提取变量的分析结果
var <- get_pca_var(res.pca)
var

# Coordinates of variables
head(var$coord, 4)
# col.var设定线条颜色
fviz_pca_var(res.pca, col.var = "black")

head(var$cos2)
library("corrplot")
# is.corr表示输入的矩阵不是相关系数矩阵
corrplot(var$cos2, is.corr=FALSE)

# Total cos2 of variables on Dim.1 and Dim.2
# 在第一第二主成分是显示结果（通过值的叠加显示）
fviz_cos2(res.pca, choice = "var", axes = 1:2)

# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

head(var$contrib, 4)
library("corrplot")
corrplot(var$contrib, is.corr=FALSE)

res.pca = PCA(decathlon2[,1:12], scale.unit=TRUE, ncp=5, quanti.sup=c(11: 12), graph=T)

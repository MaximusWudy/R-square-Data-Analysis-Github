#Beautical Correlation analysis
head(mtcars)
#cor(x, method = c("pearson", "kendall", "spearman"))
mcor <- cor(mtcars)
mcor
cor(mtcars, use = "complete.obs")
install.packages('Hmisc')
#----correlation summary----
library(Hmisc)
#rcorr(x, type=c("pearson","spearman"))
rcorr(as.matrix(mtcars[,1:7]))
symnum(mcor, abbr.colnames=FALSE)

#----correlation plot----
install.packages('corrplot')
library(corrplot)
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)

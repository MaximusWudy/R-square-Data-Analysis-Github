options(family='Kai')
pre7_order=pre7_classified[order(pre7_classified$num,pre7_classified$time),]
library(plyr)
head(pre7_order)
#first tag different classes with different numbers
pre7_order=transform(pre7_order,class_num=NA)
class=levels(factor(pre7_order$classes))
for (i in 1:15){
    pre7_order$class_num[which(pre7_order$classes==class[i])]=i
}

#-----reshape2
library(reshape2)
pre7.melt=melt(pre7_order,id=c('num','classes'),measure.vars = 'class_num')
head(pre7.melt)
pre7.cast=dcast(pre7.melt,num~classes)
head(pre7.cast,100)
#-----OK by accident, we made an heatmap
library(gplots)
pre.mat=data.matrix(pre7.cast[1:500,2:ncol(pre7.cast)])
heatmap(pre.mat,family='Kai')

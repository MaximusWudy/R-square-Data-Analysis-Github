#R-square W19
#--------------------------
#try to find something interesting
#-------------------------

#First take a look at how frequently audience will see a certain type of program that day
load("~/Documents/R Programming/R-squareW15/pre7_classified.RData")
setwd("~/Documents/R Programming/R-squareW19")

library(plyr)
freq_audience<-ddply(pre7_classified,.(classes,program),summarize,freq_of_aud=length(num))
#if the program number is enough to show on one plot
data1<-subset(freq_audience,classes=='CCTV');data1<-data1[order(data1$freq_of_aud,decreasing = T),];data1<-data1[1:70,];data1<-data1[order(data1$freq_of_aud),]
#if the program number is too many
data1<-subset(freq_audience,classes=='卡通');data1<-data1[order(data1$freq_of_aud),]
#我错了= =记录片这个错别字坑到自己了
opar<-par(mar=c(3.7,8,0,0.2),family='STXihei',cex=0.7)#mar设置绘图边距，单位行，顺时针
with(data1,barplot(freq_of_aud,names.arg = program,las=2,col='#4DAF4A',horiz=T))
par(opar)

#————————————————————
#repeat the same process across all classes
with(pre7_classified,levels(classes))
# [1] "广播"   "记录片" "健康"   "教育"   "卡通"   "美食"   "其他"   "少儿"   "体育"  
# [10] "网络"   "卫视"   "新闻"   "音乐"   "娱乐"   "CCTV"  


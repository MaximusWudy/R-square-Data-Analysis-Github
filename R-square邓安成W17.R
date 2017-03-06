#R-square W17 data analysis DAC
setwd("~/Documents/R Programming/R-squareW16")
load("~/Documents/R Programming/R-squareW15/pre7_classified.RData")

library(ggplot2)
#View(pre7_classified)
p<-ggplot(pre7_classified,aes(time,length_min,group=classes))
p+geom_point(position='jitter')+geom_tile(mapping=aes(group=classes))
qplot(time,length_min,data=pre7_classified,facets=classes~.)+theme(text=element_text(family="STKaiti",size=14))
qplot(time,..density..,data=pre7_classified,facets=classes~.,geom='histogram',binwidth=100)+theme(text=element_text(family="STKaiti",size=14))

p+geom_density()+geom_tile(mapping=aes(group=classes))

pre7<-subset(pre7_classified,classes=='新闻',drop=6)
ggplot(pre7,aes(time))+geom_point(aes(time,length_min),data=pre7,position='jitter')+geom_density(aes(time))

qplot(time,data=pre7,geom='density')+geom_point(aes(time,length_min),data=pre7)

pre71<-subset(pre7_classified,classes=='卫视' | classes=='CCTV' |classes=='教育'| classes=='卡通', drop=6)
pre72<-subset(pre7_classified,classes=='音乐' | classes=='健康' |classes=='网络'| classes=='记录片', drop=6)
pre73<-subset(pre7_classified,classes=='音乐' | classes=='健康' |classes=='网络'| classes=='记录片' | classes=='卫视' | classes=='CCTV' |classes=='教育'| classes=='卡通', drop=6)

qplot(time,data=pre71,geom='density',color=classes,main='卫视、CCTV、教育、卡通类节目的播放量')+theme(text=element_text(family="STXihei",size=12))
qplot(time,data=pre72,geom='density',color=classes,main='音乐、健康、网络、纪录片类节目的播放量',ylim=c(0, 4e-5))+theme(text=element_text(family="STXihei",size=12))
color <- c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33","#A65628","#F781BF",'#999999')
qplot(time,data=pre73,geom='density',color=classes,main='两组节目播放量同时比较')+theme(text=element_text(family="STXihei",size=12))+scale_colour_manual(values=color)

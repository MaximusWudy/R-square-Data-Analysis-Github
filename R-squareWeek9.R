#R-squareWeek9
#先对于时间的数据转化成num数值（与1970年1月1日0点相差的
#秒数），做归一化处理之后生成新的数据集
num_time<-as.numeric(pre_6$time)
num_length<-pre_6$length_min
summary(num_time)
new_data<-data.frame(num_time,num_length)
new_scale<-scale(new_data,scale = T,center = T)
summary(new_scale)
#首先使用霍普金斯统计量看均匀度，之后进行不同的聚类分析的比较
library(comato)
Hopkins.index(new_scale)
#0.9844306，在这个版本的Hopkins统计量中，接近0.5是均匀分布，接近1是高度倾斜的。

#我们还需要增加一下变量或者衍生变量，比如节目名称的长度
program<-pre_6$program
program<-as.character(program)
head(nchar(program))
program_char<-nchar(program)

prog_perday<-stat_ID$ID_program
count<-sapply(prog_perday,length)
head(count)
chnl_perday<-stat_ID$ID_channel
stat_ID<-transform(stat_ID,prognum_perday=sapply(prog_perday,length))
stat_ID<-transform(stat_ID,chnlnum_perday=sapply(chnl_perday,length))

#用于聚类的数据表
stat_ID$ID_start<-as.POSIXct(stat_ID$ID_start)
clust_data<-stat_ID[,c(3,6,7,8)]
pairs(clust_data)#可以直观看出每个变量的组合都是偏斜的
#因此对于两个变量的聚类效果肯定不会特别好，我们使用多变量聚类。
#该数据集较大，因此在尽量高效的情况下我们选择不用聚类方法中聚类效果最好的一个
#聚类的目的是：不同观众有着不同的收视习惯，这个可以在收视时的换台次数和平均收视时间体现
#通过将不同收视习惯的观众聚类到一起，我们能够分别对于不同的行为习惯模式进行研究
#并且对于电视台提出可行的提高节目播放效益和收视率的建议。

#进行中心化和标准化
clust_data$ID_start<-as.numeric(clust_data$ID_start)
clust_scaled<-scale(clust_data,center=TRUE,scale=TRUE)  
head(clust_scaled)
#首先使用方法确定簇数
#k=sqrt(n/2)  用不了
#1.使用层次聚类法
set.seed(19)
x<-clust_scaled[sample(nrow(clust_scaled),10000,replace=F),]
d1<-dist(x,method='euclidean',diag=FALSE,upper=FALSE,p=2) #欧式距离  
d2<-dist(x,method='maximum' ,diag=FALSE,upper=FALSE,p=2)#切比雪夫距离  
d3<-dist(x,method='manhattan' ,diag=FALSE,upper=FALSE,p=2)#绝对值距离  
d4<-dist(x,method='canberra', diag=FALSE,upper=FALSE,p=2)#Lance距离； 

d<-dist(x,method='euclidean')  
hc1<-hclust(d,"single")#最短距离法  
hc2<-hclust(d,"complete")#最长距离法 
hc3<-hclust(d,'average')#平均距离法
hc4<-hclust(d,"median")#中间距离法  
hc5<-hclust(d,"ward.D")#离差平方和法  
hc6<-hclust(d,'ward.D2')
hc7<-hclust(d,'mcquitty')
hc8<-hclust(d,'centroid')

cpar<-par(mfrow=c(2,4))  
plot(hc1,hang=-1,main='single')  
plot(hc2,hang=-1,main='complete')
plot(hc3,hang=-1,main='average')  
plot(hc4,hang=-1,main='median')
plot(hc5,hang=-1,main='ward.D')
plot(hc6,hang=-1,main='ward.D2')
plot(hc7,hang=-1,main='mcquitty')
plot(hc8,hang=-1,main='centroid')
par(cpar)  
#发现ward.D聚类效果较好，将不同人群分成了4类
plot(hc5,hang=-1,main='ward.D')
hc.id<-cutree(hc5,4)#将每个id分到不同的类别
head(hc.id)
save(hc.id,file='hc.id.rdata')
#heatmap(as.matrix(d),labRow=F,labCol=F)

#尝试使用kmeans
library(fpc)
(kmeans<-kmeans(na.omit(x),4,nstart=10))#根据动态聚类的结果蔟数为4
plotcluster(na.omit(x),kmeans$cluster)
#显示质心
kmeans$centers

#k中心聚类pamk
pamk.result<-pamk(x)
pamk.result$nc
plot(x[,1],x[,2],col=pamk.result$pamobject$clustering)

#k中心聚类pam
library(cluster)
pam.result<-pam(na.omit(x),4)
plot(x[,1],x[,2],col=pam.result$clustering)
points(pam.result$medoids[,c(1,2)],col=1:4,pch=8,cex=2,lwd=5)
pairs(x,col=pam.result$clustering)


#mds=cmdscale(d,k=4,eig=T)#数据量大跑不出来
#x=mds$points[,1];y=mds$points[,2]
#library(ggplot2);p=ggplot(data.frame(x,y),aes(x,y))
#p+geom_point(size=3,alpha=0.8,aes(colour=factor(result),shape=iris$Species))


#使用CLARA方法进行聚类。
library(cluster)
cls4<-clara(clust_scaled,4)
clusplot(cls4)
#分类效果不好

#试一下层次聚类,由于函数原因只能去除开始时间的变量
CluSingle<-agnes(daisy(x[,2:4]),diss=TRUE,method="single")
CluComplete<-agnes(daisy(x[,2:4]),diss=TRUE,method="complete")
CluAverage<-agnes(daisy(x[,2:4]),diss=TRUE,method="average")
#这个方法不能用于POSIXct格式数据,并且跑不出来

#DBscan
library(fpc)
model<-dbscan(na.omit(d),.5,2,showplot=T,method="dist")#数据就是距离矩阵
model
#效果较差

#EM算法
library('mclust')
EM<-Mclust(na.omit(x))
summary(EM,parameters=T)
plot(EM,what='classification')
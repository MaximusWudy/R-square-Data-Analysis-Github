#Week2 邓安成清洗出的数据分析与对新清洗数据集进行重新聚类

#聚类分析
#操作时间变量
new_data<-clean_preference_changename[,3:7]
data_time<-as.numeric(strptime(new_preference$time,format="%Y-%m-%d %H:%M:%S"))
time_clust<-as.POSIXct(data_time,origin="1970-01-01 00:00:00")
clust_data<-data.frame(new_preference$program,time_clust,new_preference$length_min)

head(clust_data)
str(clust_data)
#使用duplicate函数
dim(clust_data);clust_data0<-subset(clust_data,!duplicated(clust_data$new_preference.program));dim(clust_data0)
#筛去重复数据得到3359行数据（原有1176271行），即筛选掉了那些被重复同一时间段收看的节目名称，留下其中之一进行聚类
row.names(clust_data0)<-clust_data0$new_preference.program
clust_data0<-clust_data0[,-1]
head(clust_data0)
#将时间换成秒数（since Jan 1 1970)
time_clust_num<-as.numeric(clust_data0$time_clust)
clust_data0<-cbind(clust_data0,time_clust_num)
head(clust_data0)

#尝试作图
#普通的系统聚类法
data_hc<-hclust(dist(clust_data0[,2:3]))
plot( data_hc, labels = FALSE, hang = -1,main='Cluster of TV Programs Viewed in a Day',xlab='')#不要长长的label
re <- rect.hclust(data_hc, k = 4)
#可以看到系统聚类法倾向于把数据集按照开始观看时间和观看时长分成4类

#动态聚类法
which(is.na(clust_data0)==1)
km<-kmeans(clust_data0[,2:3],4,nstart=4)#根据系统聚类的特点预计他们会分为4类
library(fpc)
plotcluster(clust_data0[,2:3],km$cluster,main='Cluster of TV Programs Viewed in a Day',xlab='Time of Start Watching',ylab='Watch Time')
head(clust_data0[,2:3])
head(clust_data0[(order(-clust_data0$new_preference.length_min)),])

#------------------------
#对于电视台的聚类
clust_data_channel<-data.frame(new_preference$channel,time_clust,new_preference$length_min)
head(clust_data_channel)
str(clust_data_channel)
dim(clust_data_channel);clust_data1<-subset(clust_data_channel,!duplicated(clust_data_channel$new_preference.channel));dim(clust_data1)
row.names(clust_data1)<-clust_data1$new_preference.channel
clust_data1<-clust_data1[,-1]
View(clust_data1)

clust_data1$time_clust<-as.numeric(clust_data1$time_clust)
channel_hc<-hclust(dist(clust_data1[,1:2]))
plot( channel_hc, hang = -1)
plot( channel_hc, labels = FALSE, hang = -1,main='Cluster of TV Channels Viewed in a Day',xlab='')#不要长长的label
re <- rect.hclust(channel_hc, k = 4)
#分为四类的时候效果较好

km<-kmeans(clust_data1,4,nstart=4) 
data_frame<-data.frame(km$cluster)
#作图
library(fpc)
plotcluster(clust_data1[,1:2],km$cluster,main='Cluster of TV Channels Viewed in a Day',xlab='Time of Start Watching',ylab='Watch Time')
head(clust_data1[(order(-clust_data1$new_preference.length_min)),])

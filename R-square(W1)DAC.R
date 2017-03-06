#R-square学习小组第一周报告【邓安成】
#首先数据查看
hist(as.vector(preference$V5), breaks=100)
hist(as.vector(preference$V5), breaks=10000,xlim=range(0,2000))

program<-as.factor(preference$V4);str(program)
tv<-as.factor(preference$V3);str(tv)

which(preference$V4=='NULL')
preference$V5<-as.numeric(preference$V5)
pref_filter<-subset(preference,preference$V5>60,select=-V1)

nrow(preference)-nrow(pref_filter)

#改列名
clean_preference<-clean_preference[,-c(1,3)]
clean_preference<-clean_preference[,-5]
colnames(clean_preference)[2:4]<-c('time_start','channel','program')
clean_preference<-na.omit(clean_preference)
which(is.na(clean_preference)==1)
#删除缺失值，剩余1176268
#需要数据清洗，工作分配

#———————————————————————
#拿到关小欣清洗后数据
#期望采用关联规则分析的办法
clean<-data.frame(CID=as.numeric(clean_preference_changename$num),item=as.character(clean_preference_changename$program))
clean_trans<-as(split(clean[,'item'],clean[,'CID']),'transactions')

###########借鉴
aggrData <- split(clean$item,clean$CID)

listData <- list()
for (i in 1:length(aggrData)) {
    listData[[i]] <- as.character(aggrData[[i]][!duplicated(aggrData[[i]])])
}

trnsData <- as(listData,"transactions")#完成格式转化

#接下来开始购物篮分析
frequentset<-eclat(trnsData,parameter=list(support=0.0001,maxlen=10))#support表示一天内观看这个节目的观众约100人;认为>100人为一个团体值
summary(frequentset)#看看汇总，中位数是support=0.028,筛选出69个program
inspect(frequentset[1:10])
inspect(sort(frequentset,by='support'))#观看频繁项集
#利用apriori函数提取关联规则
rules=apriori(trnsData,parameter = list(support=0.0001,confidence=0.2))#confidence解释：观看了A节目的观众有20%也观看了B节目
summary(rules) #一共有161，013条关联
inspect(rules[100:200])
#选择出lift值较大的子集，lift=（L,R)/(P(R)P(L))是一个类似相关系数的指标，越大表示L和R存在于一个basket中不是一个偶然现象
x=subset(rules,subset=lift>=100)
length(x)#lift阈值设置为100时得到的结果个数比较符合可阅读范围，lift阈值实则可更换
inspect(sort(x,by='support’))#观察结果
result_apeiori<-as.list(inspect(sort(x,by='support')))
write.table(result_apeiori,'result_apriori.txt')#导出为txt结果表格

#--------------------------------------

#聚类分析
#首先要看如何操作时间变量
new_data<-clean_preference_changename[,3:7]
data_time<-as.numeric(strptime(new_data$time,format="%Y-%m-%d %H:%M:%S"))
time_clust<-as.POSIXct(data_time,origin="1970-01-01 00:00:00")
clust_data<-data.frame(new_data$program,time_clust,new_data$time_internal_min)

head(clust_data)
str(clust_data)
km<-kmeans(clust_data,5,nstart=20)
#没有跑出来，说有NA产生
sum(is.na(clust_data))#看看原始数据中有没有,没有
#发现是因为没有把program放在rowname，但是有重复的program，想到用duplicate函数
dim(clust_data);clust_data0<-subset(clust_data,!duplicated(clust_data$new_data.program));dim(clust_data0)
row.names(clust_data0)<-clust_data0$new_data.program
clust_data0<-clust_data0[,-1]
km<-kmeans(clust_data0,5,nstart=20)
#还是不行，估计是时间格式的问题，ok那换成秒数

clust_data0$time_clust<-as.numeric(clust_data0$time_clust)
km<-kmeans(clust_data0,5,nstart=20) #nail it!!!初步人为将他们分为5类，nstart为随机集合的个数
head(clust_list) #list里头的节目都是row.names，因此之后提取的时候需要处理

data_frame<-data.frame(km$cluster)
data_frame<-transform(data_frame,program=as.character(row.names(data_frmae)));row.names(data_frame)<-NULL

head(data_frame)
Clust_1<-as.list(subset(data_frame,km.cluster==1,select=program))
Clust_2<-as.list(subset(data_frame,km.cluster==2,select=program))
Clust_3<-as.list(subset(data_frame,km.cluster==3,select=program))
Clust_4<-as.list(subset(data_frame,km.cluster==4,select=program))
Clust_5<-as.list(subset(data_frame,km.cluster==5,select=program))
write.table(Clust_1,'result_clust_1.txt')
write.table(Clust_2,'result_clust_2.txt')
write.table(Clust_3,'result_clust_3.txt')
write.table(Clust_4,'result_clust_4.txt')
write.table(Clust_5,'result_clust_5.txt')
#输出的文件便是对于电视节目的聚类，一共为5类。

#尝试作图
library(fpc)
plotcluster(clust_data0[,1:2],km$cluster,main='Cluster of TV Programs Viewed in a Day',xlab='Time of Start Watching',ylab='Watch Time')
#从结果上看出我们分类的效果一般，不是很能够体现出每一类之间的区别
#-------------------------
#这个结果是对应着kmeans的动态剧烈的法，我们还可以与普通的系统聚类法进行一个对比
data_hc<-hclust(dist(clust_data0[,1:2]))
plot( data_hc, hang = -1)
plot( data_hc, labels = FALSE, hang = -1,main='Cluster of TV Programs Viewed in a Day',xlab='')#不要长长的label
re <- rect.hclust(data_hc, k = 6)
#可以看到系统聚类法更倾向于把数据集按照开始观看时间和观看时长分成6类
#未来还有有序样本聚类法和模糊聚类法等等，我们都可以继续进行比对研究

#------------------------
#下面尝试做对于电视台的聚类
clust_data_channel<-data.frame(new_data$channel,time_clust,new_data$time_internal_min)
#同上，有重复的TVchannel，这回我们先使用系统聚类看看样子，再决定动态聚类的k值
View(new_data)
clust_data_channel<-data.frame(new_data$channel,time_clust,new_data$time_internal_min)
head(clust_data_channel)
str(clust_data_channel)
dim(clust_data_channel);clust_data1<-subset(clust_data_channel,!duplicated(clust_data_channel$new_data.channel));dim(clust_data1)
row.names(clust_data1)<-clust_data1$new_data.channel
clust_data1<-clust_data1[,-1]
View(clust_data1)

clust_data1$time_clust<-as.numeric(clust_data1$time_clust)
channel_hc<-hclust(dist(clust_data1[,1:2]))
plot( channel_hc, hang = -1)
plot( channel_hc, labels = FALSE, hang = -1,main='Cluster of TV Channels Viewed in a Day',xlab='')#不要长长的label
re <- rect.hclust(channel_hc, k = 5)

km<-kmeans(clust_data1,5,nstart=20) 
data_frame<-data.frame(km$cluster)
data_frame<-transform(data_frame,channel=as.character(row.names(data_frmae)));row.names(data_frame)<-NULL

head(data_frame)
Clust_1<-as.list(subset(data_frame,km.cluster==1,select=channel))
Clust_2<-as.list(subset(data_frame,km.cluster==2,select=channel))
Clust_3<-as.list(subset(data_frame,km.cluster==3,select=channel))
Clust_4<-as.list(subset(data_frame,km.cluster==4,select=channel))
Clust_5<-as.list(subset(data_frame,km.cluster==5,select=channel))
write.table(Clust_1,'result_clust_channel1.txt')
write.table(Clust_2,'result_clust_channel2.txt')
write.table(Clust_3,'result_clust_channel3.txt')
write.table(Clust_4,'result_clust_channel4.txt')
write.table(Clust_5,'result_clust_channel5.txt')

#输出的文件便是对于电视台的聚类，一共为5类。
#作图
library(fpc)
plotcluster(clust_data1[,1:2],km$cluster,main='Cluster of TV Channels Viewed in a Day',xlab='Time of Start Watching',ylab='Watch Time')
#可以看到，分类区间类似program的分类区间，效果一般，应该需要探索更加优化的聚类算法


#不会做list里头套list= =以后再研究下。
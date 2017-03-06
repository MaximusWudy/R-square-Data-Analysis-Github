#R-square Week 4 邓安成
library(data.table)
pre<-fread('new_preference.txt',sep=',',stringsAsFactors = F,encoding='UTF-8')
View(pre)
#pre_order<-pre[order(pre$channel,pre$program),]#排序频道、节目
#View(pre_order)#发现有很多空白的channel，
#空白的数据可以通过orderchannel找到
pre_order2<-pre[order(-pre$channel,pre$time),]#排序频道、开始时间
View(pre_order2)#合适的原始数据集
pre_order2<-data.frame(pre_order2)#发现class不对
pre_order2<-pre_order2[,-3]#去除原始的ID

#----------------
#建立节目表
pre_order2$time<-as.POSIXct(pre_order2$time)
f_time<-pre_order2$time+pre_order2$length_min*60
head(f_time)
pre_order2<-cbind(pre_order2,f_time)
#得到结束时间,开始拿出头和尾
pre_order2$time<-as.character(pre_order2$time)
#pre_order2$f_time<-as.character(pre_order2$f_time)
str(pre_order2)
save(pre_order2,file='pre_order2_428.RData')#保存

#####
program_table<-matrix(NA,1,8);program_table<-data.frame(program_table);colnames(program_table)<-c('V1','num','X','channel','program','time','length_min','f_time');
program_table[1,]<-pre_order2[1,]#初始化节目表
abnormal_table<-matrix(NA,1,8);abnormal_table<-data.frame(abnormal_table);colnames(abnormal_table)<-c('V1','num','X','channel','program','time','length_min','f_time');
index=matrix(NA,1,1)
a=which(pre_order2$channel=='')
for (i in 2:(nrow(pre_order2)-1)){
    if (i==a){next}
    else if (pre_order2[i,]$program==pre_order2[i-1,]$program){next}
    else if (pre_order2[i,]$program!=pre_order2[i-1,]$program & pre_order2[i,]$program==pre_order2[i+1,]$program){program_table<-rbind(program_table,pre_order2[i-1,]);program_table<-rbind(program_table,pre_order2[i,])}
    else if (pre_order2[i,]$program!=pre_order2[i-1,]$program & pre_order2[i,]$program!=pre_order2[i+1,]$program){abnormal_table<-rbind(abnormal_table,pre_order2[i,]);index=cbind(index,i)}
    
}
save(index,file='index.Rdata')
save(program_table,file='Program_table.RData')
write.table(program_table,file='program_table.txt',sep=',',fileEncoding = 'UTF-8')
save(abnormal_table,file='abnormal_table.RData')
write.table(abnormal_table,file='abnormal_table.txt',sep=',',fileEncoding = 'UTF-8')
Null_channel<-pre_order2[a,]
save(Null_channel,file='Null_channel.RData')
write.table(Null_channel,file='Null_channel.txt',sep=',',fileEncoding = 'UTF-8')

#由前一步的算法，可以同时挑选出在一个时间段里头的跳跃频道（很有可能不是同一天的节目）
#先从节目时间表里头筛选出跳跃频道
abnormal_table<-matrix(NA,1,8);abnormal_table<-data.frame(abnormal_table);colnames(abnormal_table)<-c('V1','num','X','channel','program','time','length_min','f_time');
a=which(program_table$channel=='');index=matrix(NA,1,1)
for (i in 2:nrow(program_table)){
    if (i==a){next}
    else if (pre_order2[i,]$program==pre_order2[i-1,]$program){next}
    else if (pre_order2[i,]$program!=pre_order2[i-1,]$program & pre_order2[i,]$program!=pre_order2[i+1,]$program){abnormal_table<-rbind(abnormal_table,pre_order2[i,]);index=cbind(index,i)}
    
}


#将这些i从时间表中去掉
p_table<-program_table[-as.numeric(index),]

#超算节目表
load('pre_order2_428.RData')
library(data.table)
program_table<-matrix(NA,1,10);program_table<-data.frame(program_table);colnames(program_table)<-c('V1','num','channel','program','time','length','length_min','length_quantile','length_mean','f_time');
program_table[1,]<-pre_order2[1,]#初始化节目表
abnormal_table<-matrix(NA,1,10);abnormal_table<-data.frame(abnormal_table);colnames(abnormal_table)<-c('V1','num','channel','program','time','length','length_min','length_quantile','length_mean','f_time');
a=which(pre_order2$channel=='')
index=matrix(1,1,1)
#pre_order3<-pre_order2;pre_order2<-pre_order2[1:100,]
for (i in 2:(nrow(pre_order2)-1)){
    if (i==a){next}
    else if (pre_order2[i,]$program==pre_order2[i-1,]$program){next}
    else if (pre_order2[i,]$program!=pre_order2[i-1,]$program & pre_order2[i,]$program==pre_order2[i+1,]$program){
        pre_order2[i,]$f_time<-as.character(max(pre_order2[last(index):(i-1),]$f_time));program_table<-rbind(program_table,pre_order2[i,]);index<-cbind(index,i)}
    else if (pre_order2[i,]$program!=pre_order2[i-1,]$program & pre_order2[i,]$program!=pre_order2[i+1,]$program){abnormal_table<-rbind(abnormal_table,pre_order2[i,])}
}

program_table$f_time<-as.character(as.POSIXct(program_table$f_time,origin='1960-01-01'))
save(program_table,file='Program_table.RData')
save(abnormal_table,file='Abnormal_table.RData')
write.table(program_table,file='program_table.txt',sep=',',fileEncoding = 'UTF-8')
write.table(abnormal_table,file='abnormal_table.txt',sep=',',fileEncoding = 'UTF-8')

#发现出现的节目表里头有些节目的开始时间最晚，但是关闭的时间很早，没有时间表结束时间的统计价值
#更新代码，进行每一个program里头的比较筛选
index<-matrix(1,1,1)
for (i in 2:(nrow(pre_order2)-1)){
    if (i==a){next}
    else if (pre_order2[i,]$program==pre_order2[i-1,]$program){next}
    else if (pre_order2[i,]
             $program!=pre_order2[i-1,]$program & pre_order2[i,]$program==pre_order2[i+1,]$program){
        program_table<-rbind(program_table,pre_order2[last(order(pre_order2[last(index):(i-1),]$f_time))-1+last(index),]);
        program_table<-rbind(program_table,pre_order2[i,]);index<-cbind(index,i)}    
}

program_table<-program_table[-1,]
save(program_table,file='Program_table.RData')
write.table(program_table,file='program_table.txt',sep=',',fileEncoding = 'UTF-8')

#开始制作节目表
prog_table<-matrix(NA,1,8);prog_table<-data.frame(prog_table);colnames(prog_table)<-c('V1','num','X','channel','program','time','length_min','f_time');
for (i in seq(2,nrow(program_table)-1,2)){program_table[i-1,]$f_time<-program_table[i,]$f_time;prog_table<-rbind(prog_table,program_table[i,])}
prog_table<-prog_table[-1,]
View(prog_table)#完成
#最后出来的时间表里头包含有8943条节目数据，异常的单个节目已经被洗出
#其中time和ftime变量分别代表了节目的开始时间和结束时间。
#从中可以看出每一个电视台在这一天的所有节目和播出时间
count_fun<-function(x){sum(!is.na(x))}
count<-tapply(chan_prog$program,channel_list,count_fun)
head(count)
#得到每个频道每日播放的节目数量的统计
class(names(count))
Channel_count<-data.frame(row.names=NULL,names(count),count)
barplot(Channel_count$count,col='bisque',border='black',space=0,main='Programs Count for Each Channel in a day',cex.axis = 1,horiz=F,beside=T,names.arg = '')
title(xlab='Each Program represents one bar')
#lbls<-round(count/sum(count)*100)
#lbls<-paste(lbls,'%',sep='')
#lbls<-paste(names(count),lbls)
#legend('topleft',lbls,fill='bisque')
#可以看到每一天的节目数量最多的是CCTV3，有913个节目
CCTV_3<-prog_table[which(prog_table$channel=='CCTV-3'),]
#挑选出前4频道和前10的节目对于观众的收视时长分布进行统计
sort(count)
library(ggplot2)
qplot(length_min,data=pre_order2[which(pre_order2$channel=='CCTV-3'),],main='Duration Distribution for CCTV-3',geom='histogram',fill=I('bisque'),color=I('white'),binwidth=5)
qplot(length_min,data=pre_order2[which(pre_order2$channel=='CCTV-3'),],main='Duration Distribution for CCTV-3',geom='histogram',fill=I('bisque'),color=I('white'),binwidth=5,xlim=c(0,100))
p<-qplot(length_min,data=pre_order2[which(pre_order2$channel=='江苏城市'),],main='Duration Distribution for 江苏城市',geom='histogram',fill=I('bisque'),color=I('white'),binwidth=5)
p+theme(text=element_text(family="STKaiti",size=14))
p<-qplot(length_min,data=pre_order2[which(pre_order2$channel=='江苏城市'),],main='Duration Distribution for 江苏城市',geom='histogram',fill=I('bisque'),color=I('white'),binwidth=5,xlim=c(0,100))
p+theme(text=element_text(family="STKaiti",size=14))
qplot(length_min,data=pre_order2[which(pre_order2$channel=='CCTV-1'),],main='Duration Distribution for CCTV-1',geom='histogram',fill=I('bisque'),color=I('white'),binwidth=5)
qplot(length_min,data=pre_order2[which(pre_order2$channel=='CCTV-1'),],main='Duration Distribution for CCTV-1',geom='histogram',fill=I('bisque'),color=I('white'),binwidth=5,xlim=c(0,100))
qplot(length_min,data=pre_order2[which(pre_order2$channel=='CCTV-4'),],main='Duration Distribution for CCTV-4',geom='histogram',fill=I('bisque'),color=I('white'),binwidth=5)
qplot(length_min,data=pre_order2[which(pre_order2$channel=='CCTV-4'),],main='Duration Distribution for CCTV-4',geom='histogram',fill=I('bisque'),color=I('white'),binwidth=5,xlim=c(0,100))

#试图去看看是否频道的一天播放节目数量的多少与观众对于这个频道收视的平均时长时间有相关性
channel_list_whole<-list(pre_order2$channel)
ave_watch<-tapply(pre_order2$length_min,channel_list_whole,mean)
ave_watch<-ave_watch[-(8:23)]
ave_watch_data<-data.frame(row.names=NULL,names(ave_watch),ave_watch)
#发现行数大于count，其中有许多测试节目，应该除去
ave_watch_data<-ave_watch_data[-20,]
ave_watch_data<-ave_watch_data[-49,]
ave_watch_data<-ave_watch_data[-127,]#删去预留通道
ave_watch_data<-ave_watch_data[-114,]#删去新华财经
#至此，两个数据集吻合
Channel_count<-cbind(Channel_count,ave_watch_data$ave_watch)
qplot(count,ave_watch_data$ave_watch,data=Channel_count,main='Average Duration vs. Number of Programs',geom=c('point'),color=I('blue'))
p<-qplot(Channel_count$count,Channel_count$ave_watch,data=Channel_count,main='Average Duration vs. Number of Programs',geom=c('point'),color=I('blue'))
p+geom_smooth(colour='black',alpha=I(1/5),show.legend=T)#loess
p+geom_smooth(method='lm',color='green',se=F) + geom_smooth(method='lm',formula=y~I(x^(1/2)),color='yellow',se=T)+ geom_smooth(method = "lm", formula = y ~ poly(x, 3), colour = "red")  #+ coord_flip()这个可以翻转x-y
p+geom_smooth(method='gam',formula=y ~ s(x, k = 3),size=1)
x<-Channel_count$count; y<-Channel_count$`ave_watch_data$ave_watch`
fit1<-lm(y~x)
fit2<-lm(y~x^3+x^2+x)
fit3<-lm(y~x^2)
p<-qplot(x,geom='histogram',binwidth=30,color=I('white'),fill=I('bisque'),main='每日各频道节目数量分布,binwidth=30')
p+theme(text=element_text(family="STKaiti",size=14))
p<-qplot(y,geom='histogram',binwidth=2,color=I('white'),fill=I('bisque'),main='每日各频道观众平均收视时长分布,binwidth=2min')
p+theme(text=element_text(family="STKaiti",size=14))

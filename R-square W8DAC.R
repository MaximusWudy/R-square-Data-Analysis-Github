#获得官昕的清洗数据集
#读取为pre6
attach(pre6)
time.x<-as.POSIXct(time.x)
f_time=time.x+length_min*60
pre_6<-data.frame(num,channel=V3,program=V4,time=time.x,length_min,f_time)
head(pre_6)
#OK这个是我们需要的pre_6表
length(which(pre_6$channel==''))#1420个空频道
length(which(pre_6$program=='NULL'))#没有NULL节目
blankchanl<-subset(pre_6,channel=='')#取出空的子集
#看看官昕的节目表
new_ptab<-read.csv('channel_table2.csv',sep=',',stringsAsFactors = F,header = T)
head(new_ptab)
#发现了，program_length是我们需要的，做成f_time看看
class(new_ptab)
new_ptab<-transform(new_ptab,f_time=as.POSIXct(time)+program_length*60)
program_table[program_table$program=='《幕后故事》(复)',]
#1 1416           《幕后故事》(复)  0.3166667          13.90 2016-05-15 11:06:15
#1477 94220516 南京新闻 《幕后故事》(复) 2016-04-23 11:06:16 24.08 2016-04-23 11:30:21
#个人感觉之前的programtable更合理一点，尽管节目丢失率相比为15.9%和28.4%（之前的丢失的节目更多）
#还是用我的节目表进行填补，然后删去未填补成功的
library(data.table)
program_table<-data.table(program_table)
blankchanl<-data.table(blankchanl)
blankchanl<-blankchanl[order(program,time),]
head(blankchanl)
keys<-as.character(levels(as.factor(program_table$program)))
for (i in 1:nrow(blankchanl)){
    if(all(blankchanl[i,]$program!=keys)) {next}
    else blankchanl[i,]$channel<-program_table[head(which(program_table$program==blankchanl[i,]$program),1),]$channel
}

blankchanl_filled<-blankchanl
class(blankchanl_filled)
blankchanl_filled<-data.frame(blankchanl_filled)
blankchanl_filled<-blankchanl_filled[-which(blankchanl_filled$channel==''),]
length(which(blankchanl_filled$channel==''))#没有空频道了，可以补回
pre_6<-rbind(pre_6,blankchanl_filled);pre_6<-pre_6[-which(pre_6$channel==''),];length(which(pre_6$channel==''))
#OK！
pre_6<-pre_6[order(pre_6$program),];save(pre_6,file='pre_6_final.rdata')
length(which(pre_6$program=='NULL'))
#这个数据集看看分布，剩下1737524条数据
hist(pre_6$length_min,breaks=100);range(pre_6$length_min)#最长收看达到了22h

#----------------------
#下面开始在清洗好的数据集基础上进行分析操作
library(ggplot2)

p<-qplot(length_min,data=pre_6,geom='histogram',binwidth=10,main='收视时长整体分布',xlab='时长（分钟）')
p+theme(text=element_text(family="STKaiti",size=14))
p<-qplot(length_min,data=pre_6,geom='histogram',binwidth=10,xlim=c(0,500),ylim=c(0,250000),main='收视时长局部分布(0-500分钟)',xlab='时长（分钟）')
p<-qplot(length_min,data=pre_6,geom='histogram',binwidth=10,xlim=c(0,150),ylim=c(0,250000),main='收视时长局部分布(0-150分钟)',xlab='时长（分钟）')
p<-qplot(length_min,data=pre_6,geom='histogram',binwidth=3,xlim=c(0,150),ylim=c(0,250000),main='收视时长局部分布(0-150分钟)',xlab='时长（分钟）binwidth=3')

#看看时间序列
p<-qplot(time,length_min,data=pre_6,geom='point',alpha=I(1/10),main='电视观看分钟数与收视时间的关系',ylab='观看分钟数',xlab='收视时间')

#节目观看时间的分布
p<-qplot(time,data=pre_6,geom='histogram',binwidth=3000,main='收视时间的分布',colour=I('white'),xlab='收视时间段binwidth=3000',ylab='该时间段内的收视单位数')
p<-qplot(time,data=pre_6,geom='histogram',binwidth=100,main='收视时间的分布',xlab='收视时间段binwidth=100',ylab='该时间段内的收视单位数')

#对于节目的分析
a<-strptime('2016-04-23 19:00:00',format='%Y-%m-%d %H:%M:%S')
b<-strptime('2016-04-23 23:00:00',format='%Y-%m-%d %H:%M:%S')
program_table$time<-as.POSIXct(program_table$time);program_table$f_time<-as.POSIXct(program_table$f_time)
ptab_select<-subset(program_table,time<=b & time>=a & f_time<=b & length_min<=240)
p<-qplot(length_min,data=ptab_select,geom='histogram',binwidth=10,colour=I('white'),xlim=c(0,240),main='黄金收视时段不同播放时长的各节目数量分布',xlab='播放时长binwidth=10min',ylab='该播放时长范围内节目的数量')
p+theme(text=element_text(family="STKaiti",size=14))

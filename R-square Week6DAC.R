#R-square W6【邓安成】
load("~/Documents/R Programming/R-square Week5/new_pre424.RData")
pre_order1<-pre[order(num,time),]
class(pre_order1)
pre_order1<-data.frame(pre_order1);pre_order1<-pre_order1[,-1]#格式转化成纯dataframe
num_list=list(pre_order1$num)#准备使用tapply
#做一个函数
timed<-function(x){round(sum(x),2)}
ID_duration<-tapply(pre_order1$length_min,num_list,timed)
head(ID_duration)#统计出来了每个收视单位的每日观看总时长
timeave<-function(x){round(mean(x),2)}
ID_dur_ave<-tapply(pre_order1$length_min,num_list,timeave)
head(ID_dur_ave)#统计出来了每个收视单位的每日观看平均时长
start_ID<- function(x){head(x,1)}
ID_start<-tapply(pre_order1$time,num_list,start_ID)
head(ID_start)#统计出来了每个收视单位的每日观看开始时间

pre_order1$time<-as.POSIXct(pre_order1$time)
f_time<-pre_order1$time+pre_order1$length_min*60
head(f_time)
pre_order1<-cbind(pre_order1,f_time)
#save(pre_order1,file='pre_order1_428.RData')
pre_order1<-pre_order1[order(pre_order1$num,pre_order1$f_time),]
stop_ID<- function(x){last(x)}
ID_stop<-tapply(pre_order1$time,num_list,stop_ID)
ID_stop<-as.character(as.POSIXct(ID_stop,origin='1970-01-01'))
head(ID_stop)#统计出来了每个收视单位的每日观看节目结束时间

whole_program<-function(x){unlist(levels(as.factor(x)))}
ID_program<-tapply(pre_order1$program,num_list,whole_program)
head(ID_program)#统计出来每个收视单位每日收看节目的名称

whole_channel<-function(x){unlist(levels(as.factor(x)))}
ID_channel<-tapply(pre_order1$channel,num_list,whole_channel)
head(ID_channel)#统计出来每个收视单位每日收看频道的名称

#制作总表
stat_ID<-data.frame(ID_channel,ID_program,ID_start,ID_stop,ID_duration,ID_dur_ave)
save(stat_ID,file='stat_ID')#保存

#看看其中有没有空值
sum(is.na(stat_ID))#6
stat_ID<-na.omit(stat_ID)#128608
View(stat_ID)
library(ggplot2)
p<-qplot(stat_ID$ID_duration,..density..,geom='histogram',binwidth=3,main='Total Duration Distribution of Watch Unit per Day',xlab='Total Duration(min) of Watch Unit per Day (binwidth=3)')
p+geom_hline(yintercept = 0.005)                          
p<-qplot(stat_ID$ID_dur_ave,..density..,geom='histogram',binwidth=3,main='Average Duration Distribution of Watch Unit per Day',xlab='Average Duration(min) of Watch Unit per Day (binwidth=3)')
p+geom_hline(yintercept = 0.07)


Null_program_table<-program_table[which(program_table$program=='NULL'),]
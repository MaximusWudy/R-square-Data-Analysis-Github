#超算节目表version2
#首先缩减节目表的列数，我们实际上只需要6列
load('program_table.RData')
library(data.table)
#看看ftime的格式
program_table$f_time<-as.POSIXct(program_table$f_time)
pre_order2<-program_table
program_table<-matrix(NA,1,6);program_table<-data.frame(program_table);colnames(program_table)<-c('num','channel','program','time','length_min','f_time');
#pre_order2<-pre_order2[,c(2,3,4,5,7,10)]#删去4列不需要的数据
program_table[1,]<-pre_order2[1,]#初始化节目表
a=length(which(pre_order2$channel==''))
index=matrix(1,1,1)
program_table<-data.frame(program_table)
pre_order2<-data.frame(pre_order2)
#pre_order2<-pre_order2[1:100,]
#之后的代码主要是改动了对于末尾节目的处理
#我们不考虑空的频道数据，因为它们最后都是在补齐过程才会使用的（共删去了a个空频道）
for (i in 2:(nrow(pre_order2)-a)){
    if (i==(nrow(pre_order2)-a)){program_table[nrow(program_table),]$f_time<-max(pre_order2[last(index):(i-1),]$f_time);program_table<-rbind(program_table,pre_order2[i,]);index<-cbind(index,i)}
    else if (pre_order2[i,]$program!=pre_order2[i-1,]$program & pre_order2[i,]$program!=pre_order2[i+1,]$program){program_table<-rbind(program_table,pre_order2[i,])}
    else if (pre_order2[i,]$program==pre_order2[i-1,]$program & pre_order2[i,]$program!=pre_order2[i+1,]$program){index<-cbind(index,i);program_table[nrow(program_table),]$f_time<-max(pre_order2[(last(index)-1):last(index),]$f_time)}
    else if (pre_order2[i,]$program!=pre_order2[i-1,]$program & pre_order2[i,]$program==pre_order2[i+1,]$program){program_table<-rbind(program_table,pre_order2[i,]);index<-cbind(index,i)}
}
program_table$f_time<-as.character(as.POSIXct(program_table$f_time,origin='1970-01-01'))
save(program_table,file='Program_table_56.RData')
write.table(program_table,file='program_table.txt',sep=',',fileEncoding = 'UTF-8')

#R-squareWeek8 DAC
#凯文整理出来了对于program==NULL的数据，填补上去之后还有一些空白的channel，现在需要
#将他的数据表和我的部分填补的数据表整合。
#先加载官昕清洗好的preference4.txt,然后填补数据，
#rbind之后，删去NULL和空白的，然后order，就可以想当于填上去了
setwd("~/Documents/R Programming/R-square Week8")
library(data.table)
library(bit64)
pre_new<-fread('preference4.txt',sep=',',encoding = 'UTF-8')
length(which(pre_new$channel==''))
Blank_channel<-pre_new[which(pre_new$channel==''),]

Blank_channel<-Blank_channel[order(program,time),]
head(Blank_channel)
keys<-as.character(levels(as.factor(program_table$program)))
for (i in 1:nrow(Blank_channel)){
    if(all(Blank_channel[i,]$program!=keys)) {next}
    else Blank_channel[i,]$channel<-program_table[head(which(program_table$program==Blank_channel[i,]$program),1),]$channel
}
Blank_channel_pfilled<-Blank_channel
#这样就部分填充完成,
length(which(Blank_channel_pfilled$channel==''))
#发现还有402个空白频道无法在节目表中搜索到匹配，现在由于week7分析认为这些无法匹配到的频道都位于异常节目当中需要额外的分析
#而且删去402个节目对于研究总体影响可以忽略不计，占比0.018%。因此有理由将它们不纳入正常总体中减少总体的异常程度
#——但是对于总体中已经存在的这些异常节目，我们选择将它们不做处理，以保留一定的variation。
#下面从填补数据集中删去这些异常节目所在的条目。
class(Blank_channel_pfilled)
Blank_channel_pfilled<-data.frame(Blank_channel_pfilled)
Blank_channel_fill<-Blank_channel_pfilled[-which(Blank_channel_pfilled$channel==''),]
save(Blank_channel_fill,file='Blank_channel_fill.rdata')
#现在将Blank_channel_fill填入凯文已经填补好的complete数据集中
rm(channel,i,j,keys,pro.channel.null,program,rtime,time)
class(pre);pre<-data.frame(pre)
pre<-pre[,-ncol(pre)];colnames(pre)
colnames(Blank_channel_fill)
pre<-rbind(pre,Blank_channel_fill);pre<-pre[-which(pre$channel==''),];length(which(pre$channel==''))
#填补完毕，空channel数为0
pre_filled<-pre;save(pre_filled,file='pre_filled514.rdata')

#来看看节目表中的节目时长
str(program_table)
time<-as.POSIXct(program_table$time);f_time<-as.POSIXct(program_table$f_time)
program_table$length_min<- round((f_time-time)/60,2)
program_table<-program_table[-2560,]
ptab_simple<-data.frame(program_table$length_min);row.names(ptab_simple)<-program_table$program

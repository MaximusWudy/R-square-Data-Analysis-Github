#R-squareW7
#下面是对节目和频道的分析
count<-function(x){length(which(x!=''))}
count_program<-sapply(stat_ID$ID_program,count)
head(count_program)#节目的数量统计（每个收视单位每天）
count_channel<-sapply(stat_ID$ID_channel,count)
head(count_channel)#频道的数量统计（每个收视单位每天）
p<-qplot(count_program,..density..,geom='histogram',binwidth=3,main='Distribution of Number of Programs been Watched by Each Watch Unit per Day',xlab='Number of Programs (binwidth=3)')
p+geom_hline(yintercept = 0.06)
p<-qplot(count_channel,..density..,geom='histogram',binwidth=3,main='Distribution of Number of Channels been Watched by Each Watch Unit per Day',xlab='Number of Channels (binwidth=3)')
p+geom_hline(yintercept = 0.071)
#解释：例如频道收看数量的分布图，我们可以看到大多数收视单位（占比7.2%）一天内收看的频道数在4-6个左右。

#可以去看看是否每个收视单位有这样的关系：收看节目数量越多的观众，收看平均每个节目的时间也就越长？
plot(count_program,stat_ID$ID_dur_ave)
qplot(count_program,stat_ID$ID_dur_ave,geom='point',main='Number of Program vs. Average Duration')
#解释，可以看到服从一个神奇的分布

#然后我们看看家庭的起始时间和结束时间的联系
start_time<-as.POSIXlt(stat_ID$ID_start);stop_time<-as.POSIXlt(stat_ID$ID_stop)
#作图
p<-qplot(start_time,stop_time,geom='point',main='Start_time vs. Stop_time in Audience Group',color=I('blue'),alpha=I(1/5))
p<-p+geom_abline(slope=1,intercept=seq(5000,70000,5000))
#as.numeric(strptime('2016-04-23 6:00',format='%Y-%m-%d %H:%M')),color=I('black'))
#labl<-data.frame(x=rep(0,14),y=seq(5000,70000,5000),label=paste(seq(1,14,1)))
#p+annotate("text", label = labl$label, x = labl$x, y = labl$y, size = 1, colour = "black")
#想看看有多少样本点分布在每个区间上
#由for_loop for classification可以得出每个区间内的样本点数
#之后可以将每个区间内的样本点数进行统计
#ssdata在for_loop for classification中给出
a<-paste(as.character(round(ssdata/nrow(stat_ID)*100,2)),'%',sep='')
barplot(ssdata,col=rainbow(15),main='Barplot of Audience Pattern in Watching TV',xlab='Time duration between 1st turning on and last turning off',ylab='Num of audience',names.arg=c('1st','2nd',paste(3:15,'th',sep='')),axes=T)
#mtext(a,side = 3, line = 0, col = "black")
lbls<-c('1st','2nd',paste(3:15,'th',sep=''))
lbls<-paste(lbls,a,sep=' ')
legend('topright',lbls,fill=rainbow(15),ncol=3)
#单一颜色
barplot(ssdata,col='bisque',main='Barplot of Audience Pattern in Watching TV',xlab='Time duration between 1st turning on and last turning off',ylab='Num of audience',names.arg=c('1st','2nd',paste(3:15,'th',sep='')),axes=T)
#mtext(a,side = 3, line = 0, col = "black")
lbls<-c('1st','2nd',paste(3:15,'th',sep=''))
lbls<-paste(lbls,a,sep=' ')
legend('topright',lbls,fill='bisque',ncol=3)
#发现，这张图似乎很有用处，因为我们不仅可以看到一些收视单位在打开电视之后立刻
#就结束了一天的电视，而且可以看到存在一些收视单位是一整天都开着电视的(0:00-24:00)
#解释：差不多1/4的收视单位在打开电视后1h20min之内便关上电视再也没有打开，还有8.57%的收视单位
#一整天都开着电视没有关闭。除了这两个群体之外，还有一些正常的收视群体，他们在打开电视后收看了一段时间便关闭了
#单独将这一小类群体拿出来可以看出他们符合一定的正态分布特点（如图居中部分）

#现在超速节目表出来了，可以看看初始版本的数据表的样子
#发现有一些节目的时间表重叠了，可以知道应该是因为在被异常节目隔断的前后都有相同的节目导致的，现在可以同样
#使用之前的方法进行节目表的整合，也就是将相同的节目继续整合在一起
##并且发现了初版数据表之中还有一些NULL的节目有待于填充（共1334条）,还有
#45条空的频道有待于填充
#发现，整理出来的节目表较之前的pre表格缺少了15个频道，根据对于算法的分析合理猜测应该是这些频道对应的都是
#单个出现在原始排序>>channel>>start_time数据集中的条目，因此被整合到abnormal_table当中
#之后我们也会去检验是否两个数据集频道数量的集合就是我们原表中的这个频道种类（211类）

#-------------------------
#这周任务主要是将空白频道的数据集根据节目表补齐
load("~/Documents/R Programming/Null_channel.RData")
load("~/Documents/R Programming/R-square Week7/Program_table_56.RData")
program_table<-data.table(program_table)
NULL_channel<-NULL_channel[order(program,time),]
head(NULL_channel)
keys<-as.character(levels(as.factor(program_table$program)))
for (i in 1:nrow(NULL_channel)){
    if(all(NULL_channel[i,]$program!=keys)) {next}
    else NULL_channel[i,]$channel<-program_table[head(which(program_table$program==NULL_channel[i,]$program),1),]$channel
}

NULL_channel_pfilled<-NULL_channel
save(NULL_channel_pfilled,file='NULL_channel_pfilled.rdata')
#发现第二行的需要补充的2006年春晚语言节目集锦在节目表中没有对应的数据，并发现最后版本与第一次版本的节目表
#中都没有2006年春晚语言节目集锦的数据，因此怀疑是节目表制作代码有误。
#重新分析所有的节目表制作代码，其是前对频道和起始时间进行排序，之后再按照列进行节目表的逐行提取，
#我们的假设之一就是同一个频道在一天内不会有一个节目出现在另一个节目的两个起始时间中间，如果有则是异常的数据应该晒出
#然后我们去检查异常节目数据，发现所有17条节目为2006年春晚语言节目集锦的数据都在异常节目数据里头，说明所有的条目都出现了
#上述假设的不可能情况，因此我们初步判断原始数据出现了一定程度的问题，而节目表制作代码无误。

#我们补齐一部分数据，其余没有补齐都在异常节目数据表中，有待进一步分析

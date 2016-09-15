#set up the global palette
tropical=c('darkorange','dodgerblue','hotpink','limegreen','yellow')
palette(tropical)#tell R I use those color
par(pch=19) #let the plot dots to be filled in
#set up workdir
setwd('/Users/maximus/Documents/R Programming/R-squareW22/')
load("/Users/maximus/Documents/R Programming/R-squareW15/pre7_classified.RData")
head(pre7_classified)
str(pre7_classified)

#----something interesing about ID
par(mfrow=c(2,1),cex=0.7)
with(stat_ID,hist((ID_dur_ave),breaks=300,col=2))
with(stat_ID,hist(log(ID_dur_ave),breaks=300,col=1))

par(mfrow=c(2,1),cex=0.7)
with(stat_ID,hist((ID_duration),breaks=300,col=2))
with(stat_ID,hist(log(ID_duration),breaks=300,col=1))

#----statistical inference----
#Question: whether those who watch 'News' programs 3 times more than 'entertainment' 
#spend relative less ave time in watching TV than those who watch 'entertainment'?
#也就是，是否常看新闻的人(4倍于娱乐频道）每天花在电视上的时间较少，即更加高效地看电视？

#who watch 'entertainment' and '
stat_ID_log=data.frame(ID=row.names(stat_ID),logAve=log(stat_ID$ID_dur_ave))
library(plyr)
record_count<-function(df){
    return(data.frame(News=sum(df$classes=='新闻'),Entertain=sum(df$classes=='娱乐')))
}

sum_pre7=ddply(pre7_classified,.(num),record_count)

#seperate the 2 groups
sum_pre7_news=subset(sum_pre7,(News/Entertain)>4 & News>3)
sum_pre7_entertain=subset(sum_pre7,!(News/Entertain)>4 & Entertain > 3)
par(mfrow=c(2,2),cex=0.7)
with(stat_ID,hist((sum_pre7_news$News),col=2))
with(stat_ID,hist(log(sum_pre7_news$News),breaks=30,col=1))
with(stat_ID,hist((sum_pre7_entertain),breaks=30,col=2))
with(stat_ID,hist(log(sum_pre7_entertain),breaks=30,col=1))

x=subset(stat_ID,row.names(stat_ID) %in% sum_pre7_news$num, select=ID_dur_ave)
y=subset(stat_ID,row.names(stat_ID) %in% sum_pre7_entertain$num, select=ID_dur_ave)
x<-as.numeric(x$ID_dur_ave);y<-as.numeric(y$ID_dur_ave)
par(mfrow=c(2,2))
hist(x,breaks=50,col=2,main='People Watch News');hist(y,breaks=50,col=1,main='People Watch Entertainment')
hist(log(x),breaks=50,col=2,main='People Watch News(log)');hist(log(y),breaks=50,col=1,main='People Watch Entertainment(log)')

#test the normality of the distribution
install.packages('nortest')
library(nortest)
#Anderson-Darling normality test
ad.test(x);ad.test(log(x))
ad.test(y);ad.test(log(y))

#Cramer-von Mises test
cvm.test(x);cvm.test(log(x))
cvm.test(y);cvm.test(log(y))

#Pearson-Chi test
pearson.test(x);pearson.test(log(x))
pearson.test(y);pearson.test(log(y))

# Shapiro-Francia test
sf.test(x);sf.test(log(x))
sf.test(y);sf.test(log(y))

#From the 4 normality tests above, we can see that the log transformed model fits our assumption 
#for the t-test as normal distribution.

#then we can perform the t-test
t.test(x,y,alternative = 't',var.equal = F)

## Welch Two Sample t-test
# 
# data:  x and y
# t = 12.175, df = 699.63, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#     2.941149 4.072088
# sample estimates:
#     mean of x mean of y 
# 9.222780  5.716162 

#then we can make our conclusion.
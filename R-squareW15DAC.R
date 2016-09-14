#R-square Week 15
#Classify the TV programs from pre7
#total 210 channels into no more than 10 categories
'地方卫视','网络','地方卫视','CCTV','其他','南京','地方卫视','地方卫视','CCTV','地方卫视','体育','3D','CCTV','其他','体育','地方卫视','地方卫视','CCTV','CCTV','地方卫视','CCTV','CCTV','地方卫视','CCTV','地方卫视','地方卫视','地方卫视','卡通','少儿','CCTV','CCTV','体育','CCTV','美食','美食','其他','健康','地方卫视','地方卫视','地方卫视','地方卫视','地方卫视','少儿','体育','气象','CCTV','CCTV','地方卫视','地方卫视','游戏','旅游','体育','卡通','卡通','其他','教育','教育','电影','电影','财经','广播','CCTV‘,’其他','地方卫视','广播','购物','音乐','购物','地方卫视','教育','教育','地方卫视','购物','广播','教育','教育','电影','购物','购物','地方卫视','地方卫视','其他','地方卫视','广播','地方卫视','地方卫视','地方卫视','其他','纪录片','教育','广播','股市','卡通','电影','广播','电影','地方卫视',''

chnl<-pre7$channel
chnl_uni<-unique(chnl)
chnl_trans<-matrix(NA,nrow=210,ncol=1)
for (i in 1:210){
    if (grepl('卫视',chnl_uni[i])) chnl_trans[i,]='卫视'
    else if (grepl('CCTV',chnl_uni[i])) chnl_trans[i,]='CCTV'
    else if (grepl('足球',chnl_uni[i])) chnl_trans[i,]='体育'
    else if (grepl('体育',chnl_uni[i])) chnl_trans[i,]='体育'
    else if (grepl('篮球',chnl_uni[i])) chnl_trans[i,]='体育'
    else if (grepl('卡通',chnl_uni[i])) chnl_trans[i,]='卡通'
    else if (grepl('网络',chnl_uni[i])) chnl_trans[i,]='网络'
    else if (grepl('网球',chnl_uni[i])) chnl_trans[i,]='体育'
    else if (grepl('娱乐',chnl_uni[i])) chnl_trans[i,]='娱乐'
    else if (grepl('美食',chnl_uni[i])) chnl_trans[i,]='美食'
    else if (grepl('教',chnl_uni[i])) chnl_trans[i,]='教育'
    else if (grepl('纪',chnl_uni[i])) chnl_trans[i,]='记录片'
    else if (grepl('广播',chnl_uni[i])) chnl_trans[i,]='广播'
    else if (grepl('声',chnl_uni[i])) chnl_trans[i,]='广播'
    else if (grepl('动漫',chnl_uni[i])) chnl_trans[i,]='卡通'
    else if (grepl('音',chnl_uni[i])) chnl_trans[i,]='音乐'
    else if (grepl('健',chnl_uni[i])) chnl_trans[i,]='健康'
    else if (grepl('儿',chnl_uni[i])) chnl_trans[i,]='少儿'
    else if (grepl('宝贝',chnl_uni[i])) chnl_trans[i,]='少儿'
    else if (grepl('新闻',chnl_uni[i])) chnl_trans[i,]='新闻'
    else chnl_trans[i,]='其他'
}
trans_table<-data.frame(chnl_uni,chnl_trans)
#trans table done
trans_table[,2]<-as.character(trans_table[,2])
chnl_table<-data.frame(chnl,chnl_trans=rep(NA,length(chnl)))
for (i in 1:nrow(trans_table)){
    a<-which(chnl_table[,1]==trans_table[i,1])
    chnl_table[a,2]<-trans_table[i,2]
}

#big channel table done
pre7_classified<-transform(pre7,classes=chnl_table[,2])
save(pre7_classified,file='pre7_classified.RData')
#how many class levels?
levels(as.factor(trans_table[,2]))
#[1] "广播"   "记录片" "健康"   "教育"   "卡通"   "美食"   "其他"   "少儿"   "体育"   "网络"   "卫视"  
#[12] "新闻"   "音乐"   "娱乐"   "CCTV" 
#15 levels

table(pre7_classified$classes)

#广播 记录片   健康   教育   卡通   美食   其他   少儿   体育   网络   卫视   新闻 
#1274   1538    615  54608  71070    871 474385  22105  31268   1364 660319  37718 
#音乐   娱乐   CCTV 
#2096  24784 612254 
qplot(classes,data=pre7_classified,geom='bar')+theme(text=element_text(family="STKaiti",size=14))

#now we do some basic analysis
library(ggplot2)
View(pre7_classified)
p<-ggplot(pre7_classified,aes(time,length_min,group=classes))
p+geom_point(position='jitter')+geom_tile(mapping=aes(group=classes))
qplot(time,length_min,data=pre7_classified,facets=classes~.)+theme(text=element_text(family="STKaiti",size=14))
qplot(time,..density..,data=pre7_classified,facets=classes~.,geom='histogram',binwidth=100)+theme(text=element_text(family="STKaiti",size=14))

View(new_preference4)
#根据节目播放时间表重新对原始数据的时长进行清洗
pre1=fread("preference1.txt", sep=",", stringsAsFactors = FALSE, encoding = "UTF-8",drop = 1)
View(pre1)
pre1
nrow(subset(pre1,V5<0))#9177个数据为负值 删掉
pre1_normal=subset(pre1,V5>=0) #删去负值后，2,137,997条数据 
#3579种节目
View(pre1_normal)
time_table=read.csv("channel_table2.csv")
#colnames(time_table)=c("program","program_length_min")
View(time_table)
length(unique(time_table$program)) #时间表中有3009种节目
pre1_table=merge(pre1_normal,time_table,by.x="V4",by.y ="program")
View(pre1_table) #2,130,009   
#其中7988条记录因为时间表中节目不完善而删掉了
pre1_table$length_min=pre1_table$V5/60
pre1_table_normal=subset(pre1_table,length_min<=program_length)
View(pre1_table_normal) #1,690,316数据
summary(pre1_table_normal$length_min)
range(pre1_table_normal$length_min)
save(pre1_table_normal,file="preference6.Rdata")

View(subset(pre1_table_normal,length_min>200))#2,308条播放时长超过500min 
length(unique(subset(pre1_table_normal,length_min>120)$V4))#162个节目
null_program=subset(pre1_table_normal,V4=="NULL")
View(null_program)
null_channel=subset(pre1_table_normal,V3=="NULL")
View(null_channel)
View(subset(pre1_table_normal,V4=="以播出为准")) # 35,658条以播出为准
save(pre1_table_normal)
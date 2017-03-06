rownames(preID_time)<-preID_time$ID#ID名称要加上
preID_time<-preID_time[,-1]
heat<-as.matrix(preID_time[,1:24])
samp_heat<-heat[sample(nrow(heat),10000,replace=T),]

heat2<-as.matrix(preID_time[,25:48])
samp_heat2<-heat2[sample(nrow(heat2),10000,replace=T),]

heat3<-as.matrix(preID_time)
samp_heat3<-heat3[sample(nrow(heat3),10000,replace=T),]

library(gplots)
png(file="myplot1.png", bg="transparent",width=3000,height=2572)
heatmap.2(samp_heat,Colv=F,dendrogram='row',col=heat.colors(256),scale='row',colsep=c(6,11,20),
          sepcolor='white',sepwidth = c(0.2,0.2),key=T,density.info = 'none',trace='none',cexRow = 0.5,main='Heatmap & Cluster for ID based on Program Watch',
          xlab='TV broadcast timeset')
dev.off()

png(file="myplot2.png", bg="transparent",width=3000,height=2572)
heatmap.2(samp_heat2,Colv=F,dendrogram='row',col=heat.colors(256),scale='row',colsep=c(6,11,20),
          sepcolor='white',sepwidth = c(0.2,0.2),key=T,density.info = 'none',trace='none',cexRow = 0.5,main='Heatmap & Cluster for ID based on Watch Length',
          xlab='TV broadcast timeset')
dev.off()

png(file="myplot3.png", bg="transparent",width=3000,height=2572)
heatmap.2(samp_heat3,Colv=F,dendrogram='row',col=heat.colors(256),scale='row',colsep=c(6,11,20,30,35,44),
          sepcolor='white',sepwidth = c(0.2,0.2),key=T,density.info = 'none',trace='none',cexRow = 0.5,main='Heatmap & Cluster for ID',
          xlab='TV broadcast timeset (2 sets)')
dev.off()
#--------------

library(cluster)
cls4<-clara(heat3,10,metric='manhattan',samples=10000,pamLike = T,medoids.x = F,keep.data = F)

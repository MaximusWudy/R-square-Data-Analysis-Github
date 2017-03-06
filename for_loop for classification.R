length(which(stop_time<=start_time+5000))
#31822
length(which(stop_time<=start_time+10000 & stop_time>start_time+5000))
#12427
length(which(stop_time<=start_time+15000 & stop_time>start_time+10000))
#9291
length(which(stop_time<=start_time+20000 & stop_time>start_time+15000))
#6572
length(which(stop_time<=start_time+25000 & stop_time>start_time+20000))
#5872
length(which(stop_time<=start_time+30000 & stop_time>start_time+25000))
#6889
length(which(stop_time<=start_time+35000 & stop_time>start_time+30000))
#8688
length(which(stop_time<=start_time+ 40000  & stop_time>start_time+ 35000 ))
#9591
length(which(stop_time<=start_time+ 45000  & stop_time>start_time+ 40000 ))
#8969

paste('length(which(stop_time<=start_time+',seq(10000,65000,5000),' & stop_time>start_time+',seq(5000,60000,5000),'))')
#干脆还是写循环好了= =
ssdata<-matrix(NA,1,1)
for (i in 1:15) {
    if (i==1) ssdata[1,1]<-length(which(stop_time<=start_time+5000))
    else if (i==15) ssdata<-cbind(ssdata,length(which(stop_time>start_time+(i-1)*5000)))
    else ssdata<-cbind(ssdata,length(which(stop_time<=start_time+i*5000 & stop_time>start_time+(i-1)*5000)))
}

start_intervals <- read.csv("1/Martonvasar_maize.set",skip=1,stringsAsFactors=FALSE)

indices <- which(start_intervals[,3] != start_intervals[,4])

png("kichen_sink.png",width=30,height=30,res=600,units = "cm")

par(mfrow=c(5,4))

for(i in indices){
    ranges <- start_intervals[i,3:4]
    optimes <- numeric(10)
    for(j in 1:10){
        base_table <- read.csv(paste(j,"Martonvasar_maize_after_tree.set",sep="/"),
                               skip=1, stringsAsFactors=FALSE)
        ranges <- rbind(ranges,base_table[i,3:4])
        optimes[j] <- unlist(readRDS(paste0(j,"/results.RDS"))$parameters[start_intervals[indices,1]][indices==i])
    }
    plot(ranges[,1],11:1,type="l",xlim=range(ranges),main=base_table[i,1],xlab="",ylab="iterations",yaxt="n")
    axis(2,at=11:1,labels = 0:10)
    points(optimes,10:1)
    lines(ranges[,2],11:1,type="l")
}
dev.off()

postscript("kichen_sink.eps",paper="a4")

par(mfrow=c(5,4))

for(i in indices){
    ranges <- start_intervals[i,3:4]
    optimes <- numeric(10)
    for(j in 1:10){
        base_table <- read.csv(paste(j,"Martonvasar_maize_after_tree.set",sep="/"),
                               skip=1, stringsAsFactors=FALSE)
        ranges <- rbind(ranges,base_table[i,3:4])
        optimes[j] <- unlist(readRDS(paste0(j,"/results.RDS"))$parameters[start_intervals[indices,1]][indices==i])
    }
    plot(ranges[,1],11:1,type="l",xlim=range(ranges),main=base_table[i,1],xlab="",ylab="iterations",yaxt="n")
    axis(2,at=11:1,labels = 0:10)
    points(optimes,10:1)
    lines(ranges[,2],11:1,type="l")
}
dev.off()



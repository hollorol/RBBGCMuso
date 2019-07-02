#' Plant management
#'
#' This script plots the timeline of plant management types.
#'
#' @author Erzsebet Kristof 
#' @param
#' @export 

# Required packages:
lapply(list("data.table", "reshape2", "ggplot2"), FUN=require, character.only=TRUE)

# Managament types:
mgm <- data.frame("type"=c("planting", "thinning", "mowing", "grazing", "harvesting", "ploughing", "fertilizing", "irrigating"),
                  "abbr"=c("plt", "thi", "mow", "gra", "hrv", "plg", "frz" ,"irr"), "nr"=1:8)

# Reading files:
setwd("G:/...")
list1 <- list.files()

date <- list(0)

for (i in 1:length(list1)) {
  date[[i]] <- fread(list1[i], skip=1, select=1, fill=T, header=F)
}

nr <- 0

for (i in 1:length(list1)) {
  j <- 0
  continue <- TRUE
  while(continue) {
    j <- j+1
    if(grepl(mgm$abbr[j], list1[i])==TRUE) {
      continue <- FALSE
      nr[i] <- j
    }
  }
}
  
date.and.types <- list(0)

for (i in 1:length(list1)) {
  date.and.types[[i]] <- cbind(date[[i]],rep(mgm$type[nr[[i]]], dim(date[[i]])[1]))
}

date.and.types.DF <- melt(date.and.types)
date.and.types.DF <- date.and.types.DF[,1:2]
colnames(date.and.types.DF) <- c("date","type")
date.and.types.DF.merged <- merge(date.and.types.DF, mgm, by="type")

data.all <- date.and.types.DF.merged[order(date.and.types.DF.merged$date),]

rm(i, j, continue,date, date.and.types, date.and.types.DF, date.and.types.DF.merged, nr)

# Visualisation:
windows(width=1000, height=600)
  ggplot(data=data.all, aes(x=date, y=nr, fill=type)) +
  scale_fill_manual("Management types", values=c("planting"="blue", "thinning"="deepskyblue", "mowing"="chartreuse", "grazing"="dark orange",
                                                 "harvesting"="red", "ploughing"="purple", "fertilizing"="dark green", "irrigating"="grey50")) +
  geom_bar(stat="identity", width=0.5) +
  xlab("Date") +
  theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.direction="horizontal", legend.position="bottom",
          axis.text.x=element_text(color="black", size=10, angle=90)) # face="bold"

  
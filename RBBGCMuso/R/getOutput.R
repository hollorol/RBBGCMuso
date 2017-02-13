getthewholedata<-function(settings){
  f1<-settings$ininput[2]
  filename = paste(settings$inputloc,settings$outputname,"_ann.txt",sep="")
  alloutput<-read.table(filename,skip=22, header = FALSE)
  return(alloutput)
}

getthespecdata<-function(settings,colnumbers){
  filename<-paste(settings$inputloc,settings$outputname,"_ann.txt",sep="")
  specoutput<-read.table(filename,skip=22, header = FALSE)[,colnumbers]
  return(specoutput)
}

getdailyout<-function(settings){
  binaryname<-paste(settings$inputloc,settings$outputname,".dayout",sep="")
  d<-file(binaryname,"rb")
  dayoutput<-matrix(readBin(d,"double",size=4,n=(settings$numdata[1])),(settings$numyears*365),byrow=TRUE)
  close(d)
  return(dayoutput)
}

getmonthlyout<-function(settings){
  binaryname<-paste(settings$inputloc,settings$outputname,".monavgout",sep="")
  d<-file(binaryname,"rb")
  monoutput<-matrix(readBin(d,"double",size=4,n=(settings$numdata[2])),(settings$numyears*12),byrow=TRUE)
  close(d)
  return(monoutput)
}

getyearlyout<-function(settings){
  binaryname<-paste(settings$inputloc,settings$outputname,".annout",sep="")
  d<-file(binaryname,"rb")
  yearoutput<-matrix(readBin(d,"double",size=4,n=(settings$numdata[3])),(settings$numyears),byrow=TRUE)
  close(d)
  return(yearoutput)
}




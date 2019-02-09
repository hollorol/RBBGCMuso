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
  binaryname<-paste0(settings$outputLoc,"/",settings$outputNames[2],".dayout")
  d<-file(binaryname,"rb")
  ##leapyear is not implemented yet in this function
  dayoutput<-matrix(readBin(d,"double",size=8,n=(settings$numData[1])),(settings$numYears*365),byrow=TRUE)
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
  binaryname<-paste0(settings$inputLoc,"/",settings$outputName[2],".annout")
  ## d<-file(binaryname,"rb")
  ## yearoutput<-matrix(readBin(d,"double",size=4,n=(settings$numData[3])),(settings$numYears),byrow=TRUE)
  ## close(d)
  ## return(yearoutput)
  outPut <- read.table(binaryname,skip = 1)
  colnames(outPut) <- c("year", paste0("var_",settings$annualVarCodes))
  outPut
}





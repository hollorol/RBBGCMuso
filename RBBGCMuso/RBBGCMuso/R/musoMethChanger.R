## library(RBBGCMuso)
## library(BayesianTools)
## library(sensitivity)

metMusoGet <- function(metFile,skip=4,namerow=3,saveBackup=TRUE, revert=FALSE){

    
    
    metData<-read.table(file = metFile,skip=skip)
    namesMet <- unlist(read.table(file=metFile,skip = namerow-1,nrows = 1))
    colnames(metData)<-namesMet

    if(revert){
        file.copy(grep(basename(metFile),grep("mbck$",list.files(dirname(metFile)),value=TRUE),value = TRUE), metFile,overwrite = TRUE)
        return(cat("Meteorological data is succesfully reverted to backup data"))
    }

    if(saveBackup){
        file.copy(metFile,paste(metFile,"mbck",sep = "-"))
    }    
return(metData)
}

metMusoSet <- function(metFile,skip=4,namerow=3,saveBackup=TRUE, revert=FALSE,index, changedData){

    
    
    metData<-read.table(file = metFile,skip=skip)
    namesMet <- unlist(read.table(file=metFile,skip = namerow-1,nrows = 1))
    colnames(metData)<-namesMet

    if(revert){
        file.copy(grep("mbck$",list.files(),value=TRUE), metFile)    
    }

    if(saveBackup){
        file.copy(metFile,paste(metFile,"mbck",sep = "-"))
    }

    if(is.vector(changedData)&(length(metData[,index])==length(changedData))){
        metData[,index]<-changedData

        changedMet<- c(readLines(metFile,-1)[1:skip],apply(metData,1, function (x) paste(x,collapse = " ")))
        return(writeLines(changedMet,metFile))
    
    }else {
        return(cat("\n\tThe changedData is not a vector or not in a same length"))
    }
}

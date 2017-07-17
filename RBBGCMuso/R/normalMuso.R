normalMuso<- function(settings,parameters=c(" ECOPHYS"),timee="d",debugging=FALSE,logfilename=NULL){

   Linuxp <-(Sys.info()[1]=="Linux")

    ########################################################
###############################Preparational functions###############
#####################################################

    numcut<-function(string){
    #This function returns only the starting numbers of a string
    unlist(strsplit(grep("^[0-9]",string,value = TRUE),"[aAzZ-]"))[1]
        }

numcutall<-function(vector){
    #numcall apply numcut for all elements of a string vector
as.numeric(unlist(apply(as.matrix(vector),1,numcut)))
}

stamp<-function(path="./"){
    #It gives back a stamp wich is the maximum number of the output numcall
    numbers<-numcutall(list.files(path))
    if(length(numbers)==0){
        return (0)} else {
                   return(max(numbers))}
}




    changemulline(settings,parameters)

    inputloc<-settings$inputloc
    executable<-settings$executable
    ininput<-settings$ininput




    setwd(inputloc)
                                        #normal run
    system(paste(executable,ininput[2],sep=" "))
    
    switch(timee,
           "d"=(Reva<-getdailyout(settings)),
           "m"=(Reva<-getmonthlyout(settings)),
           "y"=(Reva<-getyearlyout(settings))
           )


    logfiles<-list.files(inputloc)[grep("log$",list.files(inputloc))]

#############LOG SECTION#######################
    perror<-as.numeric(as.vector(lapply(paste(inputloc,logfiles,sep=""),function(x) tail(readLines(x,-1),1))))
    dirName<-paste(inputloc,"/LOG",sep="")
    dirERROR<-paste(inputloc,"/ERROR",sep="")
    ERROR_EPC<-paste(inputloc,"/ERROR_EPC",sep="")

    if(!dir.exists(dirName)){
        dir.create(dirName)
    }

    if(!dir.exists(dirERROR)){
        dir.create(dirERROR)
    }

    if(length(perror)>sum(perror)){
        errorsign <- 1
    } else {
        errorsign <- 0
    }



    if(debugging=="stamplog"){
        stampnum<-stamp(dirName)
        lapply( logfiles, function (x) file.rename(from=paste(inputloc,x, sep=""), to=paste(dirName, "/",(stampnum+1),"-",x,sep="")))
        if(errorsign==1){
            lapply( logfiles, function (x) file.copy(from=paste(dirName, "/",(stampnum+1),"-",x,sep=""), to=dirERROR  ))}

    } else { if(debugging){
                 if(is.null(logfilename)){
                     lapply( logfiles, function (x) file.rename(from=paste(inputloc,x, sep=""), to=paste(dirName,"/", x, sep="")))
                     if(errorsign==1){
                         lapply( logfiles, function (x) file.rename(from=paste(dirName,"/", x, sep=""), to=dirERROR))
                     }

                 } else {
                     lapply( logfiles, function (x) file.rename(from=paste(inputloc,x, sep=""), to=paste(dirName, "/",logfilename,"-",x,sep="")))
                     if(errorsign==1){
                         lapply( logfiles, function (x) file.rename(from=paste(dirName, "/",logfilename,"-",x,sep=""), to=dirERROR))
                     }
                 }    
                 
             }}


    cleanupMuso()
    if(errorsign==1){
        return("Modell Failure")
    }

    

    
    return(Reva)

}

normalMuso<- function(settings,parameters=NULL,timee="d",debugging=FALSE,logfilename=NULL,keepEpc=FALSE, export=FALSE,silent=FALSE,aggressive=FALSE,leapYear=FALSE, binaryPlace="./",fileToChange="epc"){


##########################################################################
###########################Set local variables########################
########################################################################

    Linuxp <-(Sys.info()[1]=="Linux")
    ##Copy the variables from settings
    inputLoc <- settings$inputLoc
    outputLoc <- settings$outputLoc
    executable <- settings$executable
    iniInput <- settings$iniInput
    epc <- settings$epcInput
    calibrationPar <- settings$calibrationPar
    whereAmI<-getwd()




    


        if(!is.null(parameters)){

        switch(fileToChange,
               "epc"=(changemulline(filename=epc[2],calibrationPar,parameters)),
               "ini"=(changemulline(filename=iniInput[2],calibrationPar,parameters)),
               "both"=(stop("This option is not implemented yet, please choose epc or ini"))
               )
    }



    setwd(inputLoc)
                                        #normal run

    if(silent){
        if(Linuxp){
            system(paste(executable,iniInput[2],"> /dev/null",sep=" "))
        } else {
            system(paste(executable,iniInput[2],sep=" "),show.output.on.console = FALSE)
        }
        
    } else {
        system(paste(executable,iniInput[2],sep=" "))
    }


    
    system(paste(executable,iniInput[2],sep=" "))
    
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

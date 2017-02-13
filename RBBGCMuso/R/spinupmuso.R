spinupMuso <- function(settings,parameters=NULL, timee="d", debugging=FALSE, logfilename=NULL, keepEpc=FALSE, silent=FALSE, aggressive=FALSE){
    ##Copy the variables from settings
    inputloc <- settings$inputloc
    executable <- settings$executable
    ininput <- settings$ininput
    epc <- settings$epcinput
    calibrationpar <- settings$calibrationpar

    ##Sometimes a bug occure due to logfiles and controlfiles in the input loc directory
    
    if(silent!=TRUE){
        if(length(grep("(dayout$)|(log$)",list.files(inputloc)))>0){    
            cat(" \n \n WARMING: there is a log or dayout file nearby the ini files, that may cause problemes. \n \n If you want to avoid that possible problemes, please copy the log or dayout files into a save place, and after do a cleanupMuso(), or delete these manually, or run the rungetMuso(), with the agressive=TRUE parameter \n \n")

        }
        
    }
    ##With the aggressive option every unneeded file will deleted
    if(aggressive==TRUE){
        cleanupMuso()
    }

    
    ##change the epc file if and only if there are given parameters
    if(!is.null(parameters)){
        changemulline(settings, parameters)
    }

    ##We change the working directory becase of the model, but we want to avoid sideeffects, so we save the current location and after that we will change everything to it.
    
    whereAmI<-getwd()
    ## Set the working directory to the inputloc temporary.
    setwd(inputloc)
    ##Run the spinup
    system(paste(executable,ininput[1],sep=" "))

    logfiles<-list.files(inputloc)[grep("log$",list.files(inputloc))]
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


    if(keepEpc){#if keepepc option tured on
        
        if(length(unique(dirname(epc)))>1){
            print("Why are you playing with my nervs? Seriously? You hold your epc-s in different folders?")
        } else {
            epcdir <- dirname(epc[1])
            
            WRONGEPC<-paste(inputloc,"WRONGEPC",sep="")
            EPCS<-paste(inputloc,"EPCS",sep="")
            
            if(!dir.exists(WRONGEPC)){
                dir.create(WRONGEPC)
            }

            if(!dir.exists(EPCS)){
                dir.create(EPCS)
            }
            
            epcfiles <- list.files(epcdir)[grep("epc$",list.files(epcdir))]
            stampnum<-stamp(EPCS)
            lapply(epcfiles,function (x) file.copy(from = paste(epcdir,"/",x,sep=""),to=paste(EPCS,"/",(stampnum+1),"-",x,sep="")))
            if(errorsign==1){
                lapply(epcfiles,function (x) file.copy(from = paste(EPCS,"/",(stampnum+1),"-",x,sep=""), to=WRONGEPC))
            }

        }
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

    
}


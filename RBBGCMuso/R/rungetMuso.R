#' This runs the BBGC-MuSo model
#' @author Roland Hollós
#' @param filename Name of the initialisation files
#' @return No return, outputs are written to file 
#' @usage The function works only, if ...

Linuxp <-(Sys.info()[1]=="Linux")

rungetMuso <- function(settings, timee="d", debugging=FALSE, logfilename=NULL, keepEpc=FALSE, export=FALSE, silent=FALSE, aggressive=FALSE, leapYear=FALSE){

#############################################################
############################spinup run############################
   ########################################################## 

    ##Copy the variables from settings
    inputloc <- settings$inputloc
    outputloc <- settings$outputloc
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
    
    if(aggressive==TRUE){
        cleanupMuso(location=outputloc)
    }

    ##We change the working directory becase of the model, but we want to avoid sideeffects, so we save the current location and after that we will change everything to it.
    
    whereAmI<-getwd()
    ## Set the working directory to the inputloc temporary.
    setwd(inputloc)


    ##Run the model for the spinup run.

    if(silent){#silenc mode
        if(Linuxp){
            #In this case, in linux machines
            system(paste(executable,ininput[1],"> /dev/null",sep=" "))
        } else {
            #In windows machines there is a show.output.on.console option
            system(paste(executable,ininput[1],sep=" "),show.output.on.console = FALSE)
        }
        
    } else {
        system(paste(executable,ininput[1],sep=" "))
    }

    
    
    logspinup<-list.files(outputloc)[grep("log$",list.files(outputloc))]#load the logfiles
    if(length(logspinup)==0){
        return("Modell Failure")#in that case the modell did not create even a logfile
    }
    
    spincrash<-tail(readLines(paste(outputloc,logspinup,sep="/"),-1),1)==0 #If the last line in the logfile is 0 There are mistakes so the spinup crashes
    
    if(!spincrash){##If spinup did not crashed, run the normal run.
        
       ##################################################################### 
       ###########################normal run#########################
      #################################################################

        ##for the sake of safe we set the location again
        setwd(inputloc)

        if(silent){
            if(Linuxp){
                system(paste(executable,ininput[2],"> /dev/null",sep=" "))
            } else {
                system(paste(executable,ininput[2],sep=" "),show.output.on.console = FALSE)
            }
            
        } else {
            system(paste(executable,ininput[2],sep=" "))
        }


        ##read the output
        
        switch(timee,
               "d"=(Reva<-getdailyout(settings)),
               "m"=(Reva<-getmonthlyout(settings)),
               "y"=(Reva<-getyearlyout(settings))
               )
    }

    
    logfiles <- list.files(outputloc)[grep("log$",list.files(outputloc))]#creating a vector for logfilenames

###############################################    
#############LOG SECTION#######################
###############################################
    
    perror<-as.numeric(as.vector(lapply(paste(outputloc,logfiles,sep="/"),function(x) tail(readLines(x,-1),1)))) #vector of spinup and normalrun error
    
    if((debugging=="stamplog")|(debugging==TRUE)){#If debugging option turned on
        #If log or ERROR directory does not exists create it!
        dirName<-paste(inputloc,"LOG",sep="") 
        dirERROR<-paste(inputloc,"ERROR",sep="")
        
        if(!dir.exists(dirName)){
            dir.create(dirName)
        }

        if(!dir.exists(dirERROR)){
            dir.create(dirERROR)
        }
    }

##if errorsign is 1 there is error, if it is 0 everything ok
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
    
    cleanupMuso(location=outputloc)
    if(errorsign==1){
        return("Modell Failure")
    }

    if(timee=="d"){
        colnames(Reva) <- unlist(settings$outputvars[[1]])
    } else {
        if(timee=="y")
            colnames(Reva) <- unlist(settings$outputvars[[2]])
    }

    if(leapYear){
        Reva <- corrigMuso(settings,Reva)
         rownames(Reva) <- musoDate(settings)
     } else { 
         rownames(Reva) <- musoDate(settings, corrigated=FALSE)
    }

    if(export!=FALSE){
        setwd(whereAmI)

        ## switch(fextension(export),
        ##        "csv"=(write.csv(Reva,export)),
        ##        "xlsx"=(),
        ##        "odt"=
                  
            
        ## )
        write.csv(Reva,export)
        
    } else{
        setwd(whereAmI)
        return(Reva)}
}

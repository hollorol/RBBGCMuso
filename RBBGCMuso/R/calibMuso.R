#' calibMuso
#'
#' This function changes the epc file and after that  runs the BBGC-MuSo model and reads in its outputfile in a very structured way.
#' 
#' @author Roland Hollos
#' @param settings You have to run the setupMuso function before calibMuso. It is its output which contains all of the necessary system variables. It sets the whole running environment
#' @param timee The required timesteps in the modell output. It can be "d", if it is daily, "m", if it's monthly, "y", it it is yearly
#' @param debugging If it is TRUE, it copies the log file to a Log directory to store it, if it is stamplog it contatenate a number before the logfile, which is one more than the maximum of the represented ones in the LOG directory. If it is true or stamplog it collects the "wrong" logfiles
#' @param keepEpc If TRUE, it keeps the epc file and stamp it, after these copies it to the EPCS directory. If debugging True or false, it copies the wrong epc files to the wrong epc directory.
#' @param export if it is yes or you give a filename here, it converts the output to the specific extension. For example, if you set export to "example.csv", it converts the output to "csv", if you set it to "example.xls" it converts to example.xls with the xlsx package. If it is not installed it gives back a warning message and converts it to csv.
#' @param silent If you set it TRUE all off the modells output to the screen will be suppressed. It can be usefull, because it increases the model-speed.
#' @param aggressive It deletes every possible modell-outputs from the previous modell runs.
#' @param parameters In the settings variable you have set the row indexes of the variables, you wish to change. In this parameter you can give an exact value for them in a vector like: c(1,2,3,4)
#' @param logfilename If you want to set a specific name for your logfiles you can set this via logfile parameter
#' @param leapYear  Should the function do a leapyear correction on the outputdata? If TRUE, then the 31.12 day will be doubled. 
#' @return No return, outputs are written to file 
#' @usage calibMuso(settings,parameters=NULL, timee="d", debugging=FALSE, logfilename=NULL,
#' keepEpc=FALSE, export=FALSE, silent=FALSE, aggressive=FALSE, leapYear=FALSE)
#' @import utils
#' @export


calibMuso <- function(settings,parameters=NULL, timee="d", debugging=FALSE, logfilename=NULL, keepEpc=FALSE, export=FALSE, silent=FALSE, aggressive=FALSE, leapYear=FALSE,keepBinary=FALSE, binayPlace="./", fileToChange="epc"){


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



##########################################################################
###########################Defining Functions########################
########################################################################

    

    
    
#############################################################
############################spinup run############################
   ########################################################## 

    
    
    ##Sometimes a bug occure due to logfiles and controlfiles in the input loc directory
##alma    

    if(silent!=TRUE){
        if(length(grep("(dayout$)|(log$)",list.files(inputLoc)))>0){    
            cat(" \n \n WARMING: there is a log or dayout file nearby the ini files, that may cause problemes. \n \n If you want to avoid that possible problemes, please copy the log or dayout files into a save place, and after do a cleanupMuso(), or delete these manually, or run the rungetMuso(), with the agressive=TRUE parameter \n \n")

        }
        
    }
    
    if(aggressive==TRUE){
        cleanupMuso(location=outputLoc,deep = TRUE)
    }
    
    ##change the epc file if and only if there are given parameters
    if(!is.null(parameters)){

        switch(fileToChange,
               "epc"=(changemulline(filename=epc[2],calibrationPar,parameters)),
               "ini"=(changemulline(filename=iniInput[2],calibrationPar,parameters)),
               "both"=(stop(This option is not implemented yet, please choose epc or ini))
               )
    }

    ##We change the working directory becase of the model, but we want to avoid sideeffects, so we save the current location and after that we will change everything to it.
    

    ## Set the working directory to the inputLoc temporary.
    setwd(inputLoc)


    ##Run the model for the spinup run.

    if(silent){#silenc mode
        if(Linuxp){
            #In this case, in linux machines
            system(paste(executable,iniInput[1],"> /dev/null",sep=" "))
        } else {
            #In windows machines there is a show.output.on.console option
            system(paste(executable,iniInput[1],sep=" "),show.output.on.console = FALSE)
        }
        
    } else {
        system(paste(executable,iniInput[1],sep=" "))
    }

    
    
    logspinup<-list.files(outputLoc)[grep("log$",list.files(outputLoc))]#load the logfiles
    if(length(logspinup)==0){
        return("Modell Failure")#in that case the modell did not create even a logfile
    }

    if(length(logspinup)>1){
        spincrash<-TRUE
    } else {
        if(identical(tail(readLines(paste(outputLoc,logspinup,sep="/"),-1),1),character(0))){
            spincrash<-TRUE
        } else {
            spincrash<-(tail(readLines(paste(outputLoc,logspinup,sep="/"),-1),1)!=1)
        }
    }
    
                                        #If the last line in the logfile is 0 There are mistakes so the spinup crashes
    
    if(!spincrash){##If spinup did not crashed, run the normal run.
        
       ##################################################################### 
       ###########################normal run#########################
      #################################################################

        ##for the sake of safe we set the location again
        setwd(inputLoc)

        if(silent){
            if(Linuxp){
                system(paste(executable,iniInput[2],"> /dev/null",sep=" "))
            } else {
                system(paste(executable,iniInput[2],sep=" "),show.output.on.console = FALSE)
            }
            
        } else {
            system(paste(executable,iniInput[2],sep=" "))
        }


        ##read the output
        
        switch(timee,
               "d"=(Reva<-getdailyout(settings)),
               "m"=(Reva<-getmonthlyout(settings)),
               "y"=(Reva<-getyearlyout(settings))
               )
        if(keepBinary){
            file.copy(grep("out$",list.files(outputLoc),value=TRUE)
                    ,file.path(binaryPlace,paste0(stamp(binaryPlace),"-",grep("out$",list.files(outputLoc),value=TRUE))))
        }
    }
    
    logfiles <- list.files(outputLoc)[grep("log$",list.files(outputLoc))]#creating a vector for logfilenames

###############################################    
#############LOG SECTION#######################
###############################################


    
    perror<-as.numeric(as.vector(lapply(paste(outputLoc,logfiles,sep="/"),function(x) tail(readLines(x,-1),1))))                                      #vector of spinup and normalrun error
    
    
    if((debugging=="stamplog")|(debugging==TRUE)){#If debugging option turned on
        #If log or ERROR directory does not exists create it!
        dirName<-paste(inputLoc,"LOG",sep="") 
        dirERROR<-paste(inputLoc,"ERROR",sep="")
        
        if(!dir.exists(dirName)){
            dir.create(dirName)
        }

        if(!dir.exists(dirERROR)){
            dir.create(dirERROR)
        }
    }

    ##if errorsign is 1 there is error, if it is 0 everything ok
    perror[is.na(perror)]<-0
    if(length(perror)>sum(perror)){
        errorsign <- 1
    } else {
        if(spincrash){
            errorsign <- 1
        } else {
            errorsign <- 0
        } 
    }


    if(keepEpc){#if keepepc option tured on
        
        if(length(unique(dirname(epc)))>1){
            print("Why are you playing with my nervs? Seriously? You hold your epc-s in different folders?")
        } else {
            epcdir <- dirname(epc[1])
            
            WRONGEPC<-paste(inputLoc,"WRONGEPC",sep="")
            EPCS<-paste(inputLoc,"EPCS",sep="")
            
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
        if(inputLoc==outputLoc){
            lapply( logfiles, function (x) file.rename(from=paste(outputLoc,x, sep=""), to=paste(dirName, "/",(stampnum+1),"-",x,sep="")))
            
        } else {
            lapply( logfiles, function (x) file.rename(from=paste(outputLoc,x, sep="/"), to=paste(dirName, "/",(stampnum+1),"-",x,sep="")))
        }
        
        if(errorsign==1){
            lapply( logfiles, function (x) file.copy(from=paste(dirName, "/",(stampnum+1),"-",x,sep=""), to=dirERROR  ))}

    } else { if(debugging){
                 if(is.null(logfilename)){

                     if(inputLoc==outputLoc){
                          lapply( logfiles, function (x) file.rename(from=paste(outputLoc,x, sep=""), to=paste(dirName,"/", x, sep="")))
                     } else {
                         lapply( logfiles, function (x) file.rename(from=paste(outputLoc,x, sep="/"), to=paste(dirName,"/", x, sep="")))     
                     }

                     if(errorsign==1){
                         lapply( logfiles, function (x) file.copy(from=paste(dirName,"/", x, sep=""), to=dirERROR))
                     }

                 } else {

                     if(inputLoc==outputLoc){#These are very ugly solutions for a string problem: inputLoc: "./", if outputLoc equalent of inputLoc, it ends with "/", the string manipulation can not handle this. The better solution is easy, but I dont have enough time(Roland Hollo's) 
                         lapply( logfiles, function (x) file.rename(from=paste(outputLoc,x, sep=""), to=paste(dirName, "/",logfilename,"-",x,sep="")))                        
                     } else {
                         lapply( logfiles, function (x) file.rename(from=paste(outputLoc,x, sep="/"), to=paste(dirName, "/",logfilename,"-",x,sep="")))    
                     }
                     
                     if(errorsign==1){
                         lapply( logfiles, function (x) file.copy(from=paste(dirName, "/",logfilename,"-",x,sep=""), to=dirERROR))
                     }
                 }    
                 
             }}
    
    cleanupMuso(location=outputLoc,deep = TRUE)
    if(errorsign==1){
        return("Modell Failure")
    }

    if(timee=="d"){
        colnames(Reva) <- unlist(settings$outputVars[[1]])
    } else {
        if(timee=="y")
            colnames(Reva) <- unlist(settings$outputVars[[2]])
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

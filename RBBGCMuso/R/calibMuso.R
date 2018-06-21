#' calibMuso
#'
#' This function changes the epc file and after that  runs the BBGC-MuSo model and reads in its outputfile in a well-structured way.
#' 
#' @author Roland Hollos
#' @param settings You have to run the setupMuso function before calibMuso. It is its output which contains all of the necessary system variables. It sets the whole running environment
#' @param timee The required timesteps in the modell output. It can be "d", if it is daily, "m", if it's monthly, "y", it it is yearly. I recommend to use daily data, the yearly and monthly data is not well-tested yet. 
#' @param debugging If it is TRUE, it copies the log file to a Log directory to store it, if it is stamplog it contatenate a number before the logfile, which is one more than the maximum of the represented ones in the LOG directory. If it is true or stamplog it collects the "wrong" logfiles
#' @param keepEpc If TRUE, it keeps the epc file and stamp it, after these copies it to the EPCS directory. If debugging True or false, it copies the wrong epc files to the wrong epc directory.
#' @param export if it is yes or you give a filename here, it converts the ouxtput to the specific extension. For example, if you set export to "example.csv", it converts the output to "csv", if you set it to "example.xls" it converts to example.xls with the xlsx package. If it is not installed it gives back a warning message and converts it to csv.
#' @param silent If you set it TRUE all off the modells output to the screen will be suppressed. It can be usefull, because it increases the model-speed.
#' @param aggressive It deletes every possible modell-outputs from the previous modell runs.
#' @param parameters In the settings variable you have set the row indexes of the variables, you wish to change. In this parameter you can give an exact value for them in a vector like: c(1,2,3,4)
#' @param logfilename If you want to set a specific name for your logfiles you can set this via logfile parameter
#' @param leapYear  Should the function do a leapyear correction on the outputdata? If TRUE, then the 31.12 day will be doubled.
#' @param keepBinary In default RBBGCMuso to keep  working area as clean as possible, deletes all the regular output files. The results are directly printed to the standard output, but you can redirect it, and save it to a variable, or you can export your results to the desired destination in a desired format. Whith this variable you can enable to keep the binary output files. If you want to set the location of the binary output, please take a look at the binaryPlace argument.
#' @param binaryPlace The place of the binary output files.
#' @param fileToChange You can change any line of the epc or the ini file, you just have to specify with this variable which file you van a change. Two options possible: "epc", "ini"
#' @return No return, outputs are written to file 
#' @usage calibMuso(settings,parameters=NULL, timee="d", debugging=FALSE, logfilename=NULL,
#' keepEpc=FALSE, export=FALSE, silent=FALSE, aggressive=FALSE, leapYear=FALSE)
#' @import utils
#' @export

calibMuso <- function(settings=NULL,parameters=NULL, timee="d", debugging=FALSE, logfilename=NULL, keepEpc=FALSE, export=FALSE, silent=FALSE, aggressive=FALSE, leapYear=FALSE,keepBinary=FALSE, binaryPlace="./", fileToChange="epc"){


##########################################################################
###########################Set local variables and places########################
########################################################################
    if(is.null(settings)){
        settings <- setupMuso()
    }
    
    Linuxp <-(Sys.info()[1]=="Linux")
    ##Copy the variables from settings
    inputLoc <- settings$inputLoc
    outputLoc <- settings$outputLoc
    outputNames <- settings$outputNames
    executable <- settings$executable
    iniInput <- settings$iniInput
    epc <- settings$epcInput
    calibrationPar <- settings$calibrationPar
    binaryPlace <- normalizePath(binaryPlace)
    whereAmI<-getwd()


    ## Set the working directory to the inputLoc temporarly.
    setwd(inputLoc)

      
    if((debugging=="stamplog")|(debugging==TRUE)){#If debugging option turned on
        #If log or ERROR directory does not exists create it!
        dirName<-file.path(inputLoc,"LOG")
        dirERROR<-file.path(inputLoc,"ERROR")
        
        if(!dir.exists(dirName)){
            dir.create(dirName)
        }

        if(!dir.exists(dirERROR)){
            dir.create(dirERROR)
        }
    }
    
    if(keepEpc) {
        epcdir <- dirname(epc[1])
        print(epcdir)
        WRONGEPC<-file.path(inputLoc,"WRONGEPC")
        EPCS<-file.path(inputLoc,"EPCS")
        
        if(!dir.exists(WRONGEPC)){
            dir.create(WRONGEPC)
        }
        
        if(!dir.exists(EPCS)){
            dir.create(EPCS)
        }
    }   

#############################################################
############################spinup run############################
   ########################################################## 

    
    

     if(aggressive==TRUE){
         cleanupMuso(location=outputLoc,deep = TRUE)
     }

    

    ##change the epc file if and only if there are given parameters
    if(!is.null(parameters)){

        switch(fileToChange,
               "epc"=tryCatch(changemulline(filename=epc[2],calibrationPar,parameters),
                              error= function (e){
                                  setwd(whereAmI)
                                  stop("Cannot change the epc file")}),
               "ini"=tryCatch(changemulline(filename=iniInput[2],calibrationPar,parameters),
                              error= function (e){
                                  setwd((whereAmI))
                                  stop("Cannot change the ini file")}),
               "both"=(stop("This option is not implemented yet, please choose epc or ini"))
               )
    }

    ##We change the working directory becase of the model, but we want to avoid sideeffects, so we save the current location and after that we will change everything to it.
    


    ##Run the model for the spinup run.

    if(silent){#silenc mode
        if(Linuxp){
            #In this case, in linux machines
            tryCatch(system(paste(executable,iniInput[1],"> /dev/null",sep=" ")),
                     error= function (e){
                         setwd((whereAmI))
                         stop("Cannot run the modell-check the executable!")})
        } else {
            #In windows machines there is a show.output.on.console option
            tryCatch(system(paste(executable,iniInput[1],sep=" "),show.output.on.console = FALSE),
                     error= function (e){
                         setwd((whereAmI))
                         stop("Cannot run the modell-check the executable!")})
        }
        
    } else {
        system(paste(executable,iniInput[1],sep=" "))
    }

    
    logspinup <- getLogs(outputLoc,outputNames,type="spinup")
    ## logspinup <- grep(paste0(outputNames[1],".log"), list.files(outputLoc),value = TRUE)
    ## logspinup <- list.files(outputLoc)[grep("log$",list.files(outputLoc))]#load the logfiles
    
    if(length(logspinup)==0){
        if(keepEpc){
            stampnum<-stamp(EPCS)
            lapply(epc,function (x) file.copy(from = x ,to=paste(EPCS,"/",(stampnum+1),"-", basename(x),sep="")))
            lapply(epc, function (x) file.copy(from = paste(EPCS,"/",(stampnum+1),"-",basename(x),sep=""), to=WRONGEPC))
            setwd(whereAmI)
            stop("Modell Failure")
        }
        setwd(whereAmI)
        stop("Modell Failure") #in that case the modell did not create even a logfile
    }

    if(length(logspinup)>1){
        spincrash<-TRUE
    } else {
        if(identical(tail(readLines(paste(outputLoc,logspinup,sep="/"),-1),1),character(0))){
            spincrash<-TRUE
        } else {
            spincrash <- (tail(readLines(paste(outputLoc,logspinup,sep="/"),-1),1)!=1)
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
                tryCatch(system(paste(executable,iniInput[2],"> /dev/null",sep=" ")),
                         error =function (e){
                             setwd((whereAmI))
                             stop("Cannot run the modell-check the executable!")})
            } else {
                tryCatch(system(paste(executable,iniInput[2],sep=" "),show.output.on.console = FALSE),
                         error =function (e){
                             setwd((whereAmI))
                             stop("Cannot run the modell-check the executable!")} )
            }
            
        } else {
            tryCatch(system(paste(executable,iniInput[2],sep=" ")),
                     error =function (e){
                         setwd((whereAmI))
                         stop("Cannot run the modell-check the executable!")})
        }


        ##read the output
         
        switch(timee,
               "d"=(Reva <- tryCatch(getdailyout(settings),
                                    error = function (e){
                                        setwd((whereAmI))
                                        stop("Cannot read binary output, please check if the output type is set 2 in the ini files!")})),
               "m"=(Reva <- tryCatch(getmonthlyout(settings),
                                    error = function (e){
                                        setwd((whereAmI))
                                        stop("Cannot read binary output, please check if the output type is set 2 in the ini files!")})),
               "y"=(Reva <- tryCatch(getyearlyout(settings),
                                    error = function (e){
                                        setwd((whereAmI))
                                        stop("Cannot read binary output, please check if the output type is set 2 in the ini files!")}))
               )
        
        if(keepBinary){
            possibleNames <- tryCatch(getOutFiles(outputLoc = outputLoc,outputNames = outputNames),
                                     error=function (e){
                                         setwd((whereAmI))
                                         stop("Cannot find output files")})
            stampAndDir(outputLoc = outputLoc,names = possibleNames,stampDir=binaryPlace,type="output")
        }
    }

    
    logfiles <- tryCatch(getLogs(outputLoc,outputNames,type="both"),
                        error = function (e){
                            setwd(whereAmI)
                            "Cannot find log files, something went wrong"})
    ## list.files(outputLoc)[grep("log$",list.files(outputLoc))]#creating a vector for logfilenames

###############################################    
#############LOG SECTION#######################
###############################################


    
    perror <- readErrors(outputLoc=outputLoc,logfiles=logfiles)                                    #vector of spinup and normalrun error
    
  
    ##if errorsign is 1 there is error, if it is 0 everything ok
    perror[is.na(perror)]<-0
    if(length(perror)>sum(perror)){
        errorsign <- 1
    } else {
        if(length(perror)==1){
            errorsign <- 1
        } else {
            if(spincrash){
                errorsign <- 1
            } else {
                errorsign <- 0
            } }
    }


    if(keepEpc){#if keepepc option turned on

        if(length(unique(dirname(epc)))>1){
            stop("Why are you playing with my nervs? Seriously? You hold your epc-s in different folders?")
        } else {

            stampAndDir(stampDir=EPCS, wrongDir=WRONGEPC, names=epc, type="general", errorsign=errorsign, logfiles=logfiles)

        }
    }
    


    if(debugging==TRUE){
                       logfiles <- file.path(outputLoc,logfiles)
                       stampAndDir(stampDir=dirName, wrongDir=dirERROR, names=logfiles, type="general",errorsign=errorsign,logfiles=logfiles)}
  
    
    #cleanupMuso(location=outputLoc,deep = FALSE)
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

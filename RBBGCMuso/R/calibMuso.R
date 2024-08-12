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
#' @param parameters In the settings variable you have set the row indexes of the variables, you wish to change. In this parameter you can give an exact value for them in a vector like: c(1,2,3,4), Optionally you can have a list when you want to modify more than one file at once. In that case you have to provide fileToChange and parameters with the same length as a list.
#' @param logfilename If you want to set a specific name for your logfiles you can set this via logfile parameter
#' @param leapYear  Should the function do a leapyear correction on the outputdata? If TRUE, then the 31.12 day will be doubled.
#' @param keepBinary In default RBBGCMuso to keep  working area as clean as possible, deletes all the regular output files. The results are directly printed to the standard output, but you can redirect it, and save it to a variable, or you can export your results to the desired destination in a desired format. Whith this variable you can enable to keep the binary output files. If you want to set the location of the binary output, please take a look at the binaryPlace argument.
#' @param binaryPlace The place of the binary output files.
#' @param calibrationPar You might want to change some parameters in your EPC file before running the model. The function offers possibility for this without editing the EPC file. In this situation you have to select the appropirate model parameters first. You can refer to these parameters with the number of the line in the EPC file. Indexing of lines start from one. You should use a vector for this referencing like c(1,5,8).
#' @param fileToChange You can change any file with this parameter. You have to provide the path to that file or use the epc or soil keywords. When using epc or soild keywords, the filepath will be infered from the settings variable. Optionally you can have a list when you want to modify more than one file at once. In that case you have to provide calibrationPar and parameters with the same length as a list.
#' @param skipSpinup If TRUE, calibMuso wont do spinup simulation
#' @param prettyOut date ad Date type, separate year, month, day vectors
#' @return No return, outputs are written to file 
#' @usage calibMuso(settings,parameters=NULL, timee="d", debugging=FALSE, logfilename=NULL,
#' keepEpc=FALSE, export=FALSE, silent=FALSE, aggressive=FALSE, leapYear=FALSE)
#' @import utils
#' @export

calibMuso <- function(settings=setupMuso(), calibrationPar=NULL,
                      parameters=NULL, outVars = NULL, timee="d",
                      debugging=FALSE, logfilename=NULL,
                      keepEpc=FALSE, export=FALSE,
                      silent=FALSE, aggressive=FALSE,
                      keepBinary=FALSE,
                      binaryPlace = "./", fileToChange = "epc",
                      skipSpinup = TRUE, modifyOriginal = FALSE, prettyOut = FALSE,
                      postProcString = NULL,
                      doBackup=TRUE,
                      backupDir="bck",
                      fixAlloc=FALSE
                      ){ #
########################################################################
###########################Set local variables and places###############
########################################################################
    if(doBackup){
        for(epc in settings$epcInput){
            file.copy(epc, file.path(settings$inputLoc, backupDir), overwrite=FALSE)
        }

        for(soi in settings$soilFile){
            file.copy(soi, file.path(settings$inputLoc, backupDir), overwrite=FALSE)
        }
    }



    if(!silent){
        cat("Biome-BGC simulation started\n") # ZOLI
    }
    
    Linuxp <-(Sys.info()[1]=="Linux")
    ##Copy the variables from settings
    inputLoc <- settings$inputLoc
    outputLoc <- settings$outputLoc
    outputNames <- settings$outputNames
    executable <- settings$executable
    iniInput <- settings$iniInput
    epc <- settings$epcInput

    if(is.null(calibrationPar)){
        calibrationPar <- settings$calibrationPar
    }
    binaryPlace <- normalizePath(binaryPlace)
    whereAmI<-getwd()


    ## Set the working directory to the inputLoc temporarly.
    setwd(inputLoc)

      
    if(debugging){#If debugging option turned on
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

    
    

     if(aggressive == TRUE){
         cleanupMuso(location = outputLoc,deep = TRUE)
     }

    
    ##change the file determined by filetype if and only if there are given parameters
 
    if(!is.null(parameters)){
        if(is.list(parameters)){
            for(i in seq_along(parameters)){
                tryCatch(changeMuso(settings, parameters[[i]],
                                    calibrationPar[[i]],
                                    fileToChange[[i]], fixAlloc), error = function(e){
                    stop("Something went wrong with the file change. Parameters, calibrationpar, fileToChange have to be list with the same dimension")
                      })
            }

        }
        else {
            changeMuso(settings, parameters,
                       calibrationPar,
                       fileToChange, fixAlloc)
        }
    }
    

    ##We change the working directory becase of the model, but we want to avoid sideeffects, so we save the current location and after that we will change everything to it.
    
   if(!skipSpinup) {

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
    } else {spincrash <- FALSE}
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
               "d"=(Reva <- tryCatch(getdailyout(settings), #(:INSIDE: getOutput.R )
                                    error = function (e){
                                        setwd((whereAmI))
                                        stop("Cannot read binary output, please check if the output type is set 2 in the ini files!")})),
               "m"=(Reva <- tryCatch(getmonthlyout(settings), #(:INSIDE: getOutput.R )
                                    error = function (e){
                                        setwd((whereAmI))
                                        stop("Cannot read binary output, please check if the output type is set 2 in the ini files!")})),
               "y"=(Reva <- tryCatch(getyearlyout(settings), #(:INSIDE: getOutput.R )
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

    
    if(skipSpinup){
       logfiles <- tryCatch(getLogs(outputLoc,outputNames,type="normal"),
                                error = function (e){
                                    setwd(whereAmI)
                                    stop("Cannot find log files, something went wrong")})
    } else {
        logfiles <- tryCatch(getLogs(outputLoc,outputNames,type="both"),
                                error = function (e){
                                    setwd(whereAmI)
                                    stop("Cannot find log files, something went wrong")})
    }
    ## list.files(outputLoc)[grep("log$",list.files(outputLoc))]#creating a vector for logfilenames

###############################################    
#############LOG SECTION#######################
###############################################

    if(skipSpinup){
        errorsign <- readErrors(outputLoc=outputLoc,logfiles=logfiles,type="normal")
    } else {

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
        
        
        
    }
        
    
    

    if(keepEpc){#if keepepc option turned on

        if(length(unique(dirname(epc)))>1){
            stop("Why are you playing with my nervs? Seriously? You hold your epc-s in different folders?")
        } else {
            if(skipSpinup){
                stampAndDir(stampDir=EPCS, wrongDir=WRONGEPC, names=epc[2], type="general", errorsign=errorsign, logfiles=logfiles)
            }
            stampAndDir(stampDir=EPCS, wrongDir=WRONGEPC, names=epc, type="general", errorsign=errorsign, logfiles=logfiles)

        }
    }
    


    if(debugging){ #debugging is boolean
        logfiles <- file.path(outputLoc,logfiles)
        
                       stampAndDir(stampDir=dirName, wrongDir=dirERROR, names=logfiles, type="general",errorsign=errorsign,logfiles=logfiles)}
  
    
    #cleanupMuso(location=outputLoc,deep = FALSE)
    if(errorsign==1){
        stop("Modell Failure")
    }

    

    
    if(timee=="d"){
        if(!prettyOut){
            colnames(Reva) <- unlist(settings$outputVars[[1]])
        } else{
            Reva <- cbind.data.frame(
                musoDate(startYear = settings$startYear,
                         numYears = settings$numYears,
                         combined = FALSE, prettyOut = TRUE),
                Reva)
            colnames(Reva) <- as.character(c("date","day","month","year",unlist(settings$outputVars[[1]])) )
            
        }
    } else {
        if(timee=="y")
            colnames(Reva) <- unlist(settings$outputVars[[2]])
    }

    if(!is.null(postProcString)){
        Reva <- postProcMuso(Reva,postProcString)
    }

    
    if(!prettyOut){
        rownames(Reva) <- musoDate(settings$startYear, numYears = settings$numYears)
   }

    
    if(export!=FALSE){
        setwd(whereAmI)

        write.csv(Reva,export)
        
    } else{
        setwd(whereAmI)
        return(Reva)
    }
}

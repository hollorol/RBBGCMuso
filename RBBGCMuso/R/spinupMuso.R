#' Runs the Biome-BGCMuSo model in spinup phase (execution of normal phase is possible with normalMuso) with debugging features. 
#'
#' This function runs the Biome-BGCMuSo model in spinup phase.
#' 
#' @author Roland HOLLOS
#' @param settings RBBGCMuso uses variables that define the entire simulation environment. Those environment variables include the name of the INI files, the name of the meteorology files, the path to the model executable and its file name, the entire output list, the entire output variable matrix, the dependency rules for the EPC parameters etc. Using the runMuso function RBBGCMuso can automatically create those environment variables by inspecting the files in the working directory (this happens through the setupMuso function). It means that by default model setup is performed automatically in the background and the user has nothing to do. With this settings parameter we can force runMuso to skip automatic environment setup as we provide the environment settings to runMuso. In a typical situation the user can skip this option.
#' @param debugging If debugging is set to TRUE, after model execution the function copies the Biome-BGCMuSo log file into a LOG directory to stores it for further processing. If debugging is set to STAMPLOG instead of TRUE, it concatenates a number before the logfile, which is one plus the maximum of those present in the LOG directory. In each case the log files will be saved.
#' @param keepEpc If keepEpc is set to TRUE, the function keeps the EPC file and stamps it, and then copies it to the EPCS directory. If debugging is set to TRUE, it copies the wrong EPC files to the wrong epc directory.
#' @param silent If you set the silent parameter to TRUE, all of the model's output normally written to the screen will be suppressed. This option can be useful to increase the speed of the model execution.
#' @param aggressive It deletes all previous model-outputs from previous model runs.
#' @param parameters |||| In the parameters variable you have set the row indices of the variables that you wish to change. In this parameter you can provide an exact value for them in a vector form like c(1,2,3,4)
#' @param logfilename If you would like to set a specific name for the logfiles you can set this via the logfilename parameter
#' @return No return, outputs are written to file 
#' @usage spinupMuso(settings, parameters=NULL, debugging=FALSE,
#' logfilename=NULL, keepEpc=FALSE, silent=FALSE, aggressive=FALSE)
#' @export

spinupMuso <- function(settings=NULL, parameters=NULL, debugging=FALSE, logfilename=NULL, keepEpc=FALSE, silent=FALSE, aggressive=FALSE, fileToChange="epc"){

##########################################################################
###########################Set local variables########################
########################################################################

    if(is.null(settings)){
        settings <- setupMuso() #(:INSIDE: setupMuso.R)
        
    }
    # The software works on Linux or Windows, Mac is not implemented yet, so with this simple dichotomy we can determine wich system is running
    Linuxp <-(Sys.info()[1]=="Linux")
    ##Copy the variables from settings for the sake of easy
    inputLoc <- settings$inputLoc
    outputLoc <- settings$outputLoc
    outputNames <- settings$outputNames
    executable <- settings$executable
    iniInput <- settings$iniInput
    epc <- settings$epcInput
    calibrationPar <- settings$calibrationPar

    ## We want to minimize the number of sideeffects so we store the state to restore in the end.
    whereAmI<-getwd()


#############################################################
############################spinup run############################
########################################################## 

    ## obsolete feature, but there can be cases in wich this option is helpful
    if(aggressive==TRUE){
        cleanupMuso(location=outputLoc,deep=TRUE)} #(:INSIDE: cleanup.R)

    ## If parameters given, use changemulline, else leave this steps
    
     if(!is.null(parameters)){
        switch(fileToChange,
               "epc" = tryCatch(changemulline(filePaths = epc[1],calibrationPar,parameters), #(:INSIDE: changeMuso.R)
                              error = function (e) {stop("Cannot change the epc file")}),
               "ini" = tryCatch(changemulline(filePaths = iniInput[1],calibrationPar,parameters), #(:INSIDE: changeMuso.R)
                              error = function (e) {stop("Cannot change the ini file")}),
               "both" = (stop("This option is not implemented yet, please choose epc or ini"))
               )
    }
    
    ## Set the working directory to the inputLoc temporary.
    setwd(inputLoc)

    
    ##Run the spinup modell
    
    if(silent){    #silent mode
    
        if(Linuxp){
        #On Linux machines
            tryCatch(system(paste(executable, iniInput[1], "> /dev/null")),
                error = function(e) stop("Cannot run the model - check the executable!")
            )
            return()
        }

        #On Windows machines
        tryCatch(system(paste(executable, iniInput[1]), show.output.on.console = FALSE),
            error = function(e) stop("Cannot run the model - check the executable!")
        )
        return()
    }

###############################################
#############LOG SECTION#######################
###############################################
    
    logspinup <- getLogs(outputLoc,outputNames,type="spinup") #(:INSIDE: assistantFunctions.R)
 
    if(length(logspinup)==0){
        if(keepEpc){
            stampnum<-stamp(EPCS)
            lapply(epc,function (x) file.copy(from = x ,to=paste(EPCS,"/",(stampnum+1),"-", basename(x),sep="")))
            lapply(epc, function (x) file.copy(from = paste(EPCS,"/",(stampnum+1),"-",basename(x),sep=""), to=WRONGEPC))
            setwd(whereAmI)
            stop("Modell Failure")
        }
        setwd(whereAmI)
        stop("Modell Failure") #in that case the model didn't even create a logfile
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

    dirName<-normalizePath(paste(inputLoc,"/LOG",sep=""))
    dirERROR<-paste0(inputLoc,"/ERROR")
    
    if(!dir.exists(dirName)){
        dir.create(dirName)}

    if(!dir.exists(dirERROR)){
        dir.create(dirERROR)}

    if(spincrash){
        errorsign <- 1
    } else {
        errorsign <- 0}

    if(debugging==TRUE){
        stampAndDir(outputLoc=outputLoc,stampDir=dirName, names=logspinup, type="output") #(:INSIDE: assistantFunctions.R)
    }

    if(errorsign==1){
        stop("Modell Failure")
    }

    
}

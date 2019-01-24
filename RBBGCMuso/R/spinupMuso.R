#' Runs the Biome-BGCMuSo model in spinup phase (execution of normal phase is possible with normalMuso) with debugging features. 
#'
#' This function runs the Biome-BGCMuSo model in spinup phase.
#' 
#' @author Roland HOLLOS
#' @param settings In order to use spinupMuso, first you have to run the setupMuso function to set up the model environment. The result of setupMuso contains all necessary system variables for the model execution. It sets the whole modelling environment for the user.
#' @param debugging If debugging is set to TRUE, after model execution the function copies the Biome-BGCMuSo log file into a LOG directory to stores it for further processing. If debugging is set to STAMPLOG instead of TRUE, it concatenates a number before the logfile, which is one plus the maximum of those present in the LOG directory. In each case the log files will be saved.
#' @param keepEpc If keepEpc is set to TRUE, the function keeps the EPC file and stamps it, and then copies it to the EPCS directory. If debugging is set to TRUE or FALSE, it copies the wrong EPC files to the wrong epc directory.
#' @param silent If you set the silent parameter to TRUE, all of the model's output normally written to the screen will be suppressed. This option can be useful to increase the speed of the model execution.
#' @param aggressive It deletes all previous model-outputs from previous model runs.
#' @param parameters In the parameters variable you have set the row indices of the variables that you wish to change. In this parameter you can give an exact value for them in a vector form like c(1,2,3,4)
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

    ## obsolete feature, but there can be cases in wich this option is helpfull
    if(aggressive==TRUE){
        cleanupMuso(location=outputLoc,deep=TRUE)} #(:INSIDE: cleanup.R)

    ## If parameters given, use changemulline, else leave this steps
    
     if(!is.null(parameters)){
        switch(fileToChange,
               "epc" = tryCatch(changemulline(filename = epc[1],calibrationPar,parameters), #(:INSIDE: changeMuso.R)
                              error = function (e) {stop("Cannot change the epc file")}),
               "ini" = tryCatch(changemulline(filename = iniInput[1],calibrationPar,parameters), #(:INSIDE: changeMuso.R)
                              error = function (e) {stop("Cannot change the ini file")}),
               "both" = (stop("This option is not implemented yet, please choose epc or ini"))
               )
    }
    
    ## Set the working directory to the inputLoc temporary.
    setwd(inputLoc)

    
    ##Run the spinup modell
    
    if(silent){#silenc mode
        if(Linuxp){
            #In this case, in linux machines
            tryCatch(system(paste(executable,iniInput[1],"> /dev/null",sep=" ")),
                   error= function (e){stop("Cannot run the modell-check the executable!")})
        } else {
            #In windows machines there is a show.output.on.console option
            tryCatch(system(paste(executable,iniInput[1],sep=" "),show.output.on.console = FALSE),
                     error= function (e){stop("Cannot run the modell-check the executable!")})
        }} else {
        system(paste(executable,iniInput[1],sep=" "))
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

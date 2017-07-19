#' Run the BBGCMuso modell only in spinup phase, and debugging. 
#'
#' This function runs the BBGC-MuSo model's in the spinup phase.
#' 
#' @author Roland Hollos
#' @param settings You have to run the setupMuso function before spinupMuso. It is its output which contains all of the necessary system variables. It sets the whole running environment.
#' @param debugging If it is TRUE, it copies the log file to a Log directory to store it, if it is stamplog it contatenate a number before the logfile, which is one more than the maximum of the represented ones in the LOG directory. If it is true or stamplog it collects the "wrong" logfiles
#' @param keepEpc If TRUE, it keeps the epc file and stamp it, after these copies it to the EPCS directory. If debugging True or false, it copies the wrong epc files to the wrong epc directory.
#' @param silent If you set it TRUE all off the modells output to the screen will be suppressed. It can be usefull, because it increases the model-speed.
#' @param aggressive It deletes every possible modell-outputs from the previous modell runs.
#' @param parameters In the settings variable you have set the row indexes of the variables, you wish to change. In this parameter you can give an exact value for them in a vector like: c(1,2,3,4)
#' @param logfilename If you want to set a specific name for your logfiles you can set this via logfile parameter
#' @return No return, outputs are written to file 
#' @usage spinupMuso(settings, parameters=NULL, debugging=FALSE,
#' logfilename=NULL, keepEpc=FALSE, silent=FALSE, aggressive=FALSE)
#' @export

spinupMuso <- function(settings, parameters=NULL, debugging=FALSE, logfilename=NULL, keepEpc=FALSE, silent=FALSE, aggressive=FALSE){

##########################################################################
###########################Set local variables########################
########################################################################
    
    Linuxp <-(Sys.info()[1]=="Linux")
    ##Copy the variables from settings
    inputloc <- settings$inputloc
    outputloc <- settings$outputloc
    executable <- settings$executable
    ininput <- settings$ininput
    epc <- settings$epcinput
    calibrationpar <- settings$calibrationpar
    whereAmI<-getwd()


#############################################################
############################spinup run############################
########################################################## 
    
##Sometimes a bug occure due to logfiles and controlfiles in the input loc directory
    
    if(silent!=TRUE){
        if(length(grep("(dayout$)|(log$)",list.files(inputloc)))>0){    
            cat(" \n \n WARMING: there is a log or dayout file nearby the ini files, that may cause problemes. \n \n If you want to avoid that possible problemes, please copy the log or dayout files into a save place, and after do a cleanupMuso(), or delete these manually, or run the rungetMuso(), with the agressive=TRUE parameter \n \n")}}
    
    ##With the aggressive option every unneeded file will deleted
    if(aggressive==TRUE){
        cleanupMuso(location=outputloc)}

    
    ##change the epc file if and only if there are given parameters
    if(!is.null(parameters)){
        changemulline(filename=epc[1], calibrationpar, parameters)}

    ##We change the working directory becase of the model, but we want to avoid sideeffects, so we save the current location and after that we will change everything to it.
    
    ## Set the working directory to the inputloc temporary.
    setwd(inputloc)
    ##Run the spinup



    if(silent){#silenc mode
        if(Linuxp){
            #In this case, in linux machines
            system(paste(executable,ininput[1],"> /dev/null",sep=" "))
        } else {
            #In windows machines there is a show.output.on.console option
            system(paste(executable,ininput[1],sep=" "),show.output.on.console = FALSE)}        
    } else {
        system(paste(executable,ininput[1],sep=" "))}

###############################################
#############LOG SECTION#######################
###############################################
    logspinup<-list.files(outputloc)[grep("log$",list.files(outputloc))]
    spincrash<-tail(readLines(paste(outputloc,logspinup,sep="/"),-1),1)==0
    logfiles <- list.files(outputloc)[grep("log$",list.files(outputloc))]

    dirName<-paste(inputloc,"/LOG",sep="")
    dirERROR<-paste(inputloc,"/ERROR",sep="")
    
    if(!dir.exists(dirName)){
        dir.create(dirName)}

    if(!dir.exists(dirERROR)){
        dir.create(dirERROR)}

    if(spincrash){
        errorsign <- 1
    } else {
        errorsign <- 0}



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
        if(inputloc==outputloc){
            lapply( logfiles, function (x) file.rename(from=paste(outputloc,x, sep=""), to=paste(dirName, "/",(stampnum+1),"-",x,sep="")))
            
        } else {
            lapply( logfiles, function (x) file.rename(from=paste(outputloc,x, sep="/"), to=paste(dirName, "/",(stampnum+1),"-",x,sep="")))
        }
        
        if(errorsign==1){
            lapply( logfiles, function (x) file.copy(from=paste(dirName, "/",(stampnum+1),"-",x,sep=""), to=dirERROR  ))}

    } else { if(debugging){
                 if(is.null(logfilename)){

                     if(inputloc==outputloc){
                          lapply( logfiles, function (x) file.rename(from=paste(outputloc,x, sep=""), to=paste(dirName,"/", x, sep="")))
                     } else {
                         lapply( logfiles, function (x) file.rename(from=paste(outputloc,x, sep="/"), to=paste(dirName,"/", x, sep="")))     
                     }

                     if(errorsign==1){
                         lapply( logfiles, function (x) file.rename(from=paste(dirName,"/", x, sep=""), to=dirERROR))
                     }

                 } else {

                     if(inputloc==outputloc){#These are very ugly solutions for a string problem: inputloc: "./", if outputloc equalent of inputloc, it ends with "/", the string manipulation can not handle this. The better solution is easy, but I dont have enough time(Roland Hollo's) 
                         lapply( logfiles, function (x) file.rename(from=paste(outputloc,x, sep=""), to=paste(dirName, "/",logfilename,"-",x,sep="")))                        
                     } else {
                         lapply( logfiles, function (x) file.rename(from=paste(outputloc,x, sep="/"), to=paste(dirName, "/",logfilename,"-",x,sep="")))    
                     }
                     
                     if(errorsign==1){
                         lapply( logfiles, function (x) file.rename(from=paste(dirName, "/",logfilename,"-",x,sep=""), to=dirERROR))
                     }
                 }    
                 
             }}
    
    cleanupMuso(location=outputloc)

    
    if(errorsign==1){
        return("Modell Failure")
    }

    
}


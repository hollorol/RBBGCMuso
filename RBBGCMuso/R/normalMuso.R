#' normalMuso
#'
#' This function changes the epc file and after that runs the BBGC-MuSo model in normal phase and reads in its outputfile in a well-structured way.
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
#' @return The simunation output matrix, where the columns are the choosen variables and each row is a day/month/year data. 
#' @usage normalMuso(settings,parameters=NULL, timee="d", debugging=FALSE, logfilename=NULL,
#' keepEpc=FALSE, export=FALSE, silent=FALSE, aggressive=FALSE, leapYear=FALSE)
#' @import utils
#' @export

normalMuso<- function(settings=NULL,parameters=NULL,timee="d",debugging=FALSE,logfilename=NULL,keepEpc=FALSE, export=FALSE,silent=FALSE,aggressive=FALSE,leapYear=FALSE, binaryPlace=NULL,fileToChange="epc", keepBinary=FALSE){


##########################################################################
###########################Set local variables########################
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
    whereAmI<-getwd()

    if(is.null(binaryPlace)){
        binaryPlace <- outputLoc
    }


    


        if(!is.null(parameters)){

        switch(fileToChange,
               "epc"=(changemulline(filename=epc[2],calibrationPar,parameters)), #(:TODO: trycatch /p4/)
               "ini"=(changemulline(filename=iniInput[2],calibrationPar,parameters)),
               "both"=(stop("This option is not implemented yet, please choose epc or ini"))
               )
    }



    setwd(inputLoc)
                                        #normal run

    ## if(silent){
    ##     if(Linuxp){
    ##         system(paste(executable,iniInput[2],"> /dev/null",sep=" "))
    ##     } else {
    ##         system(paste(executable,iniInput[2],sep=" "),show.output.on.console = FALSE)
    ##     }
        
    ## } else {
    ##     system(paste(executable,iniInput[2],sep=" "))
    ## }


    
    ## system(paste(executable,iniInput[2],sep=" "))
    
    ## switch(timee,
    ##        "d"=(Reva<-getdailyout(settings)),
    ##        "m"=(Reva<-getmonthlyout(settings)),
    ##        "y"=(Reva<-getyearlyout(settings))
    ##        )

    
        if(silent){
            if(Linuxp){
                tryCatch(system(paste(executable,iniInput[2],"> /dev/null",sep=" ")), 
                         error =function (e) {stop("Cannot run the modell-check the executable!")})
            } else {
                tryCatch(system(paste(executable,iniInput[2],sep=" "),show.output.on.console = FALSE),
                        error =function (e) {stop("Cannot run the modell-check the executable!")} )
            }
            
        } else {
            tryCatch(system(paste(executable,iniInput[2],sep=" ")),
                     error =function (e) {stop("Cannot run the modell-check the executable!")})
        }


        ##read the output
         
        switch(timee,
               "d"=(Reva <- tryCatch(getdailyout(settings),
               error = function (e) {stop("Cannot read binary output, please check if the output type is set 2 in the ini files!")})),
               "m"=(Reva <- tryCatch(getmonthlyout(settings),
               error = function (e) {stop("Cannot read binary output, please check if the output type is set 2 in the ini files!")})),
               "y"=(Reva <- tryCatch(getyearlyout(settings),
               error = function (e) {stop("Cannot read binary output, please check if the output type is set 2 in the ini files!")}))
               )
        
        if(keepBinary){
            possibleNames <- getOutFiles(outputLoc = outputLoc,outputNames = outputNames)
            stampAndDir(outputLoc = outputLoc,names = possibleNames,stampDir=binaryPlace,type="output")
        }
    


    logfiles <- getLogs(outputLoc,outputNames,type = "normal")

#############LOG SECTION#######################
    errorsign <- readErrors(outputLoc = outputLoc,logfiles = logfiles,type="normal")
    dirName<-paste(inputloc,"/LOG",sep="")
    dirERROR<-paste(inputloc,"/ERROR",sep="")
    ERROR_EPC<-paste(inputloc,"/ERROR_EPC",sep="")

    if(!dir.exists(dirName)){
        dir.create(dirName)
    }

    if(!dir.exists(dirERROR)){
        dir.create(dirERROR)
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

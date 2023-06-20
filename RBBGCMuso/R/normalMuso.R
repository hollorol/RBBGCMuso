#' normalMuso
#'
#' This function optionally changes the EPC file and runs the Biome-BGCMuSo model in normal phase and reads its output file in a well-structured way with debugging features. (Execution of spinup phase is possible with spinupMuso.) Prerequisite of normalMuso is the existence of the endpoint file (which is the result of the spinup phase and contains initial conditions for the simulation). 
#' 
#' @author Roland HOLLOS
#' @param settings RBBGCMuso uses variables that define the entire simulation environment. Those environment variables include the name of the INI files, the name of the meteorology files, the path to the model executable and its file name, the entire output list, the entire output variable matrix, the dependency rules for the EPC parameters etc. Using the runMuso function RBBGCMuso can automatically create those environment variables by inspecting the files in the working directory (this happens through the setupMuso function). It means that by default model setup is performed automatically in the background and the user has nothing to do. With this settings parameter we can force runMuso to skip automatic environment setup as we provide the environment settings to runMuso. In a typical situation the user can skip this option.
#' @param timee The required timesteps in the model output. It can be "d", if it is daily, "m", if it is monthly, "y" if it is yearly. It is recommended to use daily data, as the yearly and monthly data is not well-tested yet. 
#' @param debugging If debugging is set to TRUE, after model execution the function copies the Biome-BGCMuSo log file into a LOG directory and stores it for further processing. If debugging is set to STAMPLOG instead of TRUE, it concatenates a number before the logfile, which is one plus the maximum of those present in the LOG directory. In each case the log files will be saved. 
#' @param keepEpc If keepEpc is set to TRUE, the function keeps the EPC file and stamps it, and then copies it to the EPCS directory. If debugging is set to TRUE, it copies the wrong EPC files to the wrong epc directory.
#' @param export If it is set to YES or you define a filename here, the function converts the output to the specific file format. For example, if you set export to "example.csv", it converts the output to "csv". If you set it to "example.xls" it converts the output to example.xls with the xlsx package. If the Excel converter package is not installed it gives back a warning message and converts the results to csv.
#' @param silent If you set the silent parameter to TRUE, all of the model's output normally written to the screen will be suppressed. This option can be useful to increase the speed of the model execution.
#' @param aggressive It deletes all previous model-outputs from previous model runs.
#' @param parameters Using normalMuso it is possible to change some of the EPC parameters prior to model execution. This can be achieved with this option. In the parameters variable you have set the row indices of the variables that you wish to change. In this parameters you can give an exact value for them in a vector form like c(1,2,3,4).
#' @param logfilename If you would like to set a specific name for your logfiles you can set this via the logfile parameter.
#' @param leapYear Should the function do a leapyear correction on the output data? If TRUE, then the result for 31 December will be doubled in leap years which means that the results for the leap year will cover all 366 days. See the model's User's Guide for notes on leap years. 
#' @param keepBinary By default RBBGCMuso keeps the working environment as clean as possible, thus deletes all the regular output files. The results are directly written to the standard output (e.g. to the screen), but you can redirect it and save them to a variable. Alternatively, you can export your results to the desired destination in a desired format. Through the keepBinary parameter you can set RBBGCMuso to keep the binary output files. If you would like to set the location of the binary output, please take a look at the binaryPlace argument.
#' @param binaryPlace The directory for the binary output files (see the keepBinary parameter).
#' @param fileToChange You can change any line of the EPC or the INI file prior to model execution. All you need to do is to specify with this variable which file you want to change. Two options possible: "EPC" or "INI" 
#' @return The simulation output matrix, where the columns are the chosen variables and each row is a daily/monthly/annual data. 
#' @usage normalMuso(settings,parameters=NULL, timee="d", debugging=FALSE, logfilename=NULL,
#' keepEpc=FALSE, export=FALSE, silent=FALSE, aggressive=FALSE, leapYear=FALSE)
#' @import utils
#' @export

normalMuso<- function(settings=NULL,parameters=NULL,timee="d",debugging=FALSE,logfilename=NULL,keepEpc=FALSE, export=FALSE,silent=FALSE,aggressive=FALSE,leapYear=FALSE, binaryPlace=NULL,fileToChange="epc", keepBinary=FALSE){


##########################################################################
###########################Set local variables########################
########################################################################

    if(is.null(settings)){
        settings <- setupMuso() #( :INSIDE: setupMuso.R)
    }
        # The software works on Linux or Windows, Mac is not implemented yet, so with this simple dichotomy we can determine wich syste is running
    Linuxp <-(Sys.info()[1]=="Linux")
    ##Copy the variables from settings
    inputLoc <- settings$inputLoc
    outputLoc <- settings$outputLoc
    outputNames <- settings$outputNames
    executable <- settings$executable
    iniInput <- settings$iniInput
    epc <- settings$epcInput
    calibrationPar <- settings$calibrationPar
    
    ## We want to minimize the number of sideeffects so we store the state to restore in the end.
    whereAmI<-getwd()

    
    ## Optionally the user may want to store the original binary file. At default we set it to the output location. 
    
    if(is.null(binaryPlace)){
        binaryPlace <- outputLoc
    }

    ## Now we create a directories for the debugging files if these are not exists, and if debugging or keepEpc options are set to true. 

        if(debugging){ #debugging is boolean, so we dont write debugging == TRUE for the sake of faster model run
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
    
    if(keepEpc) {#keepEpc is boolean
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
    


    
    
    if(!is.null(parameters)){
        switch(fileToChange,
               "epc" = tryCatch(changemulline(epc[1],calibrationPar,parameters), #(:DONE: trycatch :INSIDE: changeMuso.R)
                                error = function (e) {stop("Cannot change the epc file")}),
               "ini" = tryCatch(changemulline(iniInput[1],calibrationPar,parameters), #(:DONE: trycatch :INSIDE: changeMuso.R)
                                error = function (e) {stop("Cannot change the ini file")}),
               "both" = (stop("This option is not implemented yet, please choose epc or ini"))
               )
    }



   
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
            possibleNames <- getOutFiles(outputLoc = outputLoc,outputNames = outputNames) #(:INSIDE: assistantFunctions.R)
            stampAndDir(outputLoc = outputLoc,names = possibleNames,stampDir=binaryPlace,type="output") #(:INSIDE: assistantFunctions.R)
        }
    


    logfiles <- getLogs(outputLoc,outputNames,type = "normal") #(:INSIDE: assistantFunctions.R)
    

#############LOG SECTION#######################
    errorsign <- readErrors(outputLoc = outputLoc,logfiles = logfiles,type="normal") #(:INSIDE: assistantFunctions.R)

    if(keepEpc){#if keepepc option turned on

        if(length(unique(dirname(epc)))>1){
            stop("Why are you playing with my nervs? Seriously? You hold your epc-s in different folders?")
        } else {

            stampAndDir(stampDir=EPCS, wrongDir=WRONGEPC, names=epc[2], type="general", errorsign=errorsign, logfiles=logfiles)

        }
    }
        

    
        if(debugging){ #debugging is boolean
                       logfiles <- file.path(outputLoc,logfiles)
                       stampAndDir(stampDir=dirName, wrongDir=dirERROR, names=logfiles, type="general",errorsign=errorsign,logfiles=logfiles)}
    cleanupMuso()
    if(errorsign==1){
        return("Modell Failure")
    }

    

    
    return(Reva)

}

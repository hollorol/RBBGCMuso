#' calibMuso 
#'
#' This function changes the epc file and after that runs the BBGC-MuSo model and reads in its outputfile in a well-structured way.
#' 
#' @author Roland Hollos
#' @param settings You have to run the setupMuso function before calibMuso. It is its output which contains all of the necessary system variables. It sets the whole running environment
#' @param timee The required timesteps in the model output. It can be "d", if it is daily, "m", if it's monthly, "y", it it is yearly. I recommend to use daily data, the yearly and monthly data is not well-tested yet. 
#' @param debugging If it is TRUE, it copies the log file to a Log directory to store it, if it is stamplog it contatenate a number before the logfile, which is one more than the maximum of the represented ones in the LOG directory. If it is true or stamplog it collects the "wrong" logfiles
#' @param keepEpc If TRUE, it keeps the epc file and stamp it. After these it copies it to the EPCS directory. If debugging True or false, it copies the wrong epc files to the wrong epc directory.
#' @param export if it is yes or you give a filename here, it converts the ouxtput to the specific extension. For example, if you set export to "example.csv", it converts the output to "csv", if you set it to "example.xls" it converts to example.xls with the xlsx package. If it is not installed it gives back a warning message and converts it to csv.
#' @param silent If you set it TRUE all off the modells output to the screen will be suppressed. It can be usefull, because it increases the model-speed.
#' @param aggressive It deletes every possible modell-outputs from the previous model runs.
#' @param parameters In the settings variable you have set the row indexes of the variables, you wish to change. In this parameter you can give an exact value for them in a vector like: c(1,2,3,4)
#' @param logfilename If you want to set a specific name for your logfiles you can set this via logfile parameter
#' @param leapYear  Should the function do a leapyear correction on the outputdata? If TRUE, then the 31.12 day will be doubled.
#' @param keepBinary In default RBBGCMuso to keep  working area as clean as possible, deletes all the regular output files. The results are directly printed to the standard output, but you can redirect it, and save it to a variable, or you can export your results to the desired destination in a desired format. Whith this variable you can enable to keep the binary output files. If you want to set the location of the binary output, please take a look at the binaryPlace argument.
#' @param binaryPlace The place of the binary output files.
#' @param fileToChange You can change any line of the epc or the ini file, you just have to specify with this variable which file you van a change. Two options possible: "epc", "ini"
#' @param skipSpinup If TRUE, calibMuso wont do spinup simulation
#' @param prettyOut date ad Date type, separate year, month, day vectors
#' @return No return, outputs are written to file 
#' @usage calibMuso(settings,parameters=NULL, timee="d", debugging=FALSE, logfilename=NULL,
#' keepEpc=FALSE, export=FALSE, silent=FALSE, aggressive=FALSE, leapYear=FALSE)
#' @import utils
#' @export

calibMuso <- function(settings = setupMuso(), calibrationPar = NULL,
                      parameters = NULL, outVars = NULL, timee = "d",
                      debugging = FALSE, logfilename = NULL,
                      keepEpc = FALSE, export = FALSE,
                      silent = FALSE, aggressive = FALSE,
                      keepBinary = FALSE,
                      binaryPlace = "./", fileToChange = "epc",
                      skipSpinup = TRUE, modifyOriginal = FALSE, prettyOut = FALSE,
                      postProcString = NULL,
                      doBackup = TRUE,
                      backupDir ="bck",
                      fixAlloc = FALSE
                      ){ 
                        
    ######################################################################
    ################### Set local variables and places ###################
    ######################################################################
    ## Bro functions ('helper' but they are bros for doing it), maybe they'll be a part of a more complex debugger function in the future
    createDirIfNotExist <- function(path) {
        if (!dir.exists(path)) dir.create(path)
    }
    
    stopWithError <- function(errorMsg, whereAmI) {
        setwd(whereAmI)
        stop(errorMsg)
    }


    if(doBackup){
        for(epc in settings$epcInput){
            file.copy(epc, file.path(settings$inputLoc, backupDir), overwrite = FALSE)
        }

        for(soi in settings$soilFile){
            file.copy(soi, file.path(settings$inputLoc, backupDir), overwrite = FALSE)
        }
    }
    
    bck <- file.path(settings$inputLoc, "bck", basename(settings[[paste0(fileToChange, "Input")]][2]))

    if (!silent) cat("Biome-BGC simulation started\n") # ZOLI
    

    Linuxp <- Sys.info()[1] == "Linux"
    ## Copy the variables from settings
    inputLoc <- settings$inputLoc
    outputLoc <- settings$outputLoc
    outputNames <- settings$outputNames
    executable <- settings$executable
    iniInput <- settings$iniInput
    epc <- settings$epcInput
    if(is.null(calibrationPar)) calibrationPar <- settings$calibrationPar  #for simple ifs I'll use this formatting
    binaryPlace <- normalizePath(binaryPlace)
    whereAmI <- getwd()

    ## Set the working directory to the inputLoc temporarily.
    setwd(inputLoc)
    

    ## If debugging option is turned on 
    if (debugging) { # If log or ERROR directory does not exist create it!
        createDirIfNotExists(file.path(inputLoc, "LOG"))
        createDirIfNotExists(file.path(inputLoc, "ERROR"))
    }

    if (keepEpc) {
        WRONGEPC <- file.path(inputLoc, "WRONGEPC")
        EPCS <- file.path(inputLoc, "EPCS")
        createDirIfNotExists(WRONGEPC)
        createDirIfNotExists(EPCS)
    }

    ##################################################
    ################### SPINUP RUN ###################
    ################################################## 
    
    if(aggressive) cleanupMuso(location = outputLoc, deep = TRUE)

    ## Change the epc file if and only if there are given parameters
    if(!is.null(parameters)){
        changemulline(filePaths = epc[2],
                      calibrationPar = calibrationPar,                      
                      contents = parameters,
                      src = if(file.exists(bck)) bck else NULL)
        if(fixAlloc) fixAlloc(settings)
    }

    # do we need this comment this place:
    ## We change the working directory because of the model, but we want to avoid side-effects, so we save the current location and after that we will change everything to it.

    
    ## Bro function for spinup and normal run
    runModel <- function() {
        if (silent) {
            command <- paste(executable, iniInput[1], if (Linuxp) "> /dev/null" else "")
            tryCatch(system(command, show.output.on.console = !Linuxp), 
                     error = function(e) stopWithError("Cannot run the model - check the executable!", whereAmI))
        } else {
            system(paste(executable, iniInput[1]))
        }
    }


   if(!skipSpinup) {
        runModel() # Perform the spinup run.
        logspinup <- getLogs(outputLoc, outputNames, type = "spinup")
        if (length(logspinup) == 0) {
            if (keepEpc) {
                stampnum <- stamp(EPCS)
                lapply(epc, function(x) file.copy(from = x, to = paste(EPCS, "/", (stampnum + 1), "-", basename(x), sep = "")))
                lapply(epc, function(x) file.copy(from = paste(EPCS, "/", (stampnum + 1), "-", basename(x), sep = ""), to = WRONGEPC))
            }
            stopWithError("Model Failure", whereAmI)
        }
    }


    ##################################################
    ################### NORMAL RUN ###################
    ##################################################

    ## If spinup run didn't crash, we continue to normal run
    if (!spincrash) {
        setwd(inputLoc)
        runModel()

        ## Read the output
        Reva <- switch(timee,
            "d" = tryCatch(getdailyout(settings), 
                error = function(e) stopWithError("Cannot read binary output, check output type in ini files!", whereAmI)),
            "m" = tryCatch(getmonthlyout(settings), 
                error = function(e) stopWithError("Cannot read binary output, check output type in ini files!", whereAmI)),
            "y" = tryCatch(getyearlyout(settings), 
                error = function(e) stopWithError("Cannot read binary output, check output type in ini files!", whereAmI))
        )
    
        if (keepBinary) {
            possibleNames <- tryCatch(getOutFiles(outputLoc = outputLoc, outputNames = outputNames), 
                                      error = function(e) stopWithError("Cannot find output files", whereAmI))
            stampAndDir(outputLoc = outputLoc, names = possibleNames, stampDir = binaryPlace, type = "output")
        }
    }

    ####################################################    
    ################### LOG SECTION ####################
    ####################################################

    logfiles <- tryCatch(getLogs(outputLoc, outputNames, type = ifelse(skipSpinup, "normal", "both")),
                         error = function(e) stopWithError("Cannot find log files", whereAmI))


    errorsign <- if(skipSpinup){
        readErrors(outputLoc = outputLoc, logfiles = logfiles, type = "normal")
    } else {
        ## Obtain both spinup and normal run errors
        perror <- readErrors(outputLoc = outputLoc, logfiles = logfiles)   # vector of spinup and normal run error
        
        ## If errorsign is 1 there is an error, if it is 0 everything's ok
        perror[is.na(perror)] <- 0
        if(length(perror) > sum(perror) || length(perror) == 1 || spincrash){
            1
        } else {
            0
        }
    }
        
    
    if(keepEpc){ # if keepepc option is turned on

        if(length(unique(dirname(epc))) > 1){
            stop("Why are you playing with my nerves? Seriously? You hold your epc-s in different folders? Please don't do that <333")
        } else {
                epc_names <- if (skipSpinup) epc[2] else epc
                stampAndDir(stampDir = EPCS, wrongDir = WRONGEPC, names = epc[2], 
                            type = "general", errorsign = errorsign, logfiles = logfiles)
        }
    }
    


    if(debugging){ # debugging is boolean
        logfiles <- file.path(outputLoc, logfiles)
        stampAndDir(stampDir = dirName, wrongDir = dirERROR, names = logfiles, 
                    type = "general", errorsign = errorsign, logfiles = logfiles)
    }
  

    if(errorsign == 1) stop("Modell Failure")

    
    Reva <- switch(
    timee,
    "d" = {
        if (prettyOut) {
            Reva <- cbind.data.frame(
                musoDate(startYear = settings$startYear, numYears = settings$numYears, combined = FALSE, 
                         prettyOut = TRUE),
                Reva
            )
            colnames(Reva) <- c("date", "day", "month", "year", unlist(settings$outputVars[[1]]))
        } else {
            colnames(Reva) <- unlist(settings$outputVars[[1]])
        }
        Reva
    },
    "y" = {
        colnames(Reva) <- unlist(settings$outputVars[[2]])
        Reva
    },
    Reva  # Default case if 'timee' is not "d" or "y"
    )


    if(!is.null(postProcString)) Reva <- postProcMuso(Reva, postProcString)
    if(!prettyOut) rownames(Reva) <- musoDate(settings$startYear, numYears = settings$numYears)
    
    if(export){
        setwd(whereAmI)
        write.csv(Reva, export)
    } else {
        setwd(whereAmI)
        return(Reva)
    }
}

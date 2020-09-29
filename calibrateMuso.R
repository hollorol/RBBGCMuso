#' calibrateMuso
#'
#' This funtion uses the Monte Carlo technique to uniformly sample the parameter space from user defined parameters of the Biome-BGCMuSo model. The sampling algorithm ensures that the parameters are constrained by the model logic which means that parameter dependencies are fully taken into account (parameter dependency means that e.g leaf C:N ratio must be smaller than C:N ratio of litter; more complicated rules apply to the allocation parameters where the allocation fractions to different plant compartments must sum up 1). This function implements a mathematically correct solution to provide uniform distriution for all selected parameters. 
#' @author Roland HOLLOS
#' @export
calibrateMuso <- function(measuredData, parameters = NULL, startDate = NULL,
                     endDate = NULL, formatString = "%Y-%m-%d",
                     dataVar, outLoc = "./calib",
                     preTag = "cal-", settings =  setupMuso(),
                     outVars = NULL, iterations = 30,
                     skipSpinup = TRUE, plotName = "calib.jpg",
                     modifyOriginal=TRUE, likelihood, uncertainity = NULL,
                     naVal = NULL, postProcString = NULL,
                     tread_prefix="thread", numcores = (parallel::detectCores()-1), pb = txtProgressBar(min=0, max=iterations, style=3),
                     pbUpdate = setTxtProgressBar){

    #   ____                _         _   _                        _     
    #  / ___|_ __ ___  __ _| |_ ___  | |_| |__  _ __ ___  __ _  __| |___ 
    # | |   | '__/ _ \/ _` | __/ _ \ | __| '_ \| '__/ _ \/ _` |/ _` / __|
    # | |___| | |  __/ (_| | ||  __/ | |_| | | | | |  __/ (_| | (_| \__ \
    #  \____|_|  \___|\__,_|\__\___|  \__|_| |_|_|  \___|\__,_|\__,_|___/
                                                                       
    
    copyToThreadDirs(thread_prefix, numcores = numcores, runDir = settings$inputLoc)

    #  ____                _   _                        _     
    # |  _ \ _   _ _ __   | |_| |__  _ __ ___  __ _  __| |___ 
    # | |_) | | | | '_ \  | __| '_ \| '__/ _ \/ _` |/ _` / __|
    # |  _ <| |_| | | | | | |_| | | | | |  __/ (_| | (_| \__ \
    # |_| \_\\__,_|_| |_|  \__|_| |_|_|  \___|\__,_|\__,_|___/
                                                            
    threadCount <- distributeCores(iterations, numCores) 

    fut <- lapply(1:numCores, function(i) {
         future({
             musoSingleThread(measuredData, parameters, startDate,
                              endDate, formatString,
                              dataVar, outLoc,
                              preTag, settings,
                              outVars, iterations = threadCount[i],
                              skipSpinup, plotName,
                              modifyOriginal, likelihood, uncertainity,
                              naVal, postProcString, i)
         })
    })

    # __        ___           _       _                                         
    # \ \      / / |__   __ _| |_ ___| |__    _ __  _ __ ___   ___ ___  ___ ___ 
    #  \ \ /\ / /| '_ \ / _` | __/ __| '_ \  | '_ \| '__/ _ \ / __/ _ \/ __/ __|
    #   \ V  V / | | | | (_| | || (__| | | | | |_) | | | (_) | (_|  __/\__ \__ \
    #    \_/\_/  |_| |_|\__,_|\__\___|_| |_| | .__/|_|  \___/ \___\___||___/___/
    #                                        |_|                                

    getProgress <- function(){
        threadfiles <- list.files(settings$inputLoc, pattern="progress.txt", recursive = TRUE)
        if(length(threadfiles)==0){
            return(0)
        } else {
            sum(sapply(threadfiles, function(x){
                           partRes <- readLines(x)
                           if(length(partRes)==0){
                               return(0)
                           } else {
                               return(as.numeric(partRes))
                           }

         }))

        }
    }

    progress <- 0
    while(progress < 400){
        Sys.sleep(1)
        progress <- getProgress()
        pbUpdate(pb,as.numeric(progress))
    }
    close(pb)

    #   ____                _     _            
    #  / ___|___  _ __ ___ | |__ (_)_ __   ___ 
    # | |   / _ \| '_ ` _ \| '_ \| | '_ \ / _ \
    # | |__| (_) | | | | | | |_) | | | | |  __/
    #  \____\___/|_| |_| |_|_.__/|_|_| |_|\___|
                                             

    #   ____ _    _   _ _____ 
    #  / ___| |  | | | | ____|
    # | |  _| |  | | | |  _|  
    # | |_| | |__| |_| | |___ 
    #  \____|_____\___/|_____|
                            

    # musoGlue("preservedCalib.csv",w=w, lg = lg)
}

copyToThreadDirs <- function(prefix="thread", numcores=parallel::detectCores()-1, runDir="."){
    browser()
    dir.create(file.path(runDir,prefix), showWarnings=TRUE)
    fileNames <- grep("^thread.*", list.files(runDir), value=TRUE, invert=TRUE)
    invisible(sapply(1:numcores,function(corenum){
                threadDir <- file.path(runDir,prefix,paste0(prefix,"_",corenum))
                dir.create(threadDir, showWarnings=FALSE)
                file.copy(from=fileNames,to=threadDir, overwrite=FALSE)
    }))
}

musoSingleThread <- function(measuredData, parameters = NULL, startDate = NULL,
                     endDate = NULL, formatString = "%Y-%m-%d",
                     dataVar, outLoc = "./calib",
                     preTag = "cal-", settings =  setupMuso(),
                     outVars = NULL, iterations = 30,
                     skipSpinup = TRUE, plotName = "calib.jpg",
                     modifyOriginal=TRUE, likelihood, uncertainity = NULL,
                     naVal = NULL, postProcString = NULL, threadNumber) {

    setwd(paste0("thread/thread-",threadNumber))
    # Exanding likelihood
    likelihoodFull <- as.list(rep(NA,length(dataVar)))
    names(likelihoodFull) <- names(dataVar)
    if(!missing(likelihood)) {
        lapply(names(likelihood),function(x){
                   likelihoodFull[[x]] <<- likelihood[[x]]
                     })
    }
   defaultLikelihood <- which(is.na(likelihood))
   if(length(defaultLikelihood)>0){
        likelihoodFull[[defaultLikelihood]] <- (function(x, y){
                                                       exp(-sqrt(mean((x-y)^2)))
                                                })
   }

    mdata <- measuredData
    if(is.null(parameters)){
        parameters <- tryCatch(read.csv("parameters.csv", stringsAsFactor=FALSE), error = function (e) {
            stop("You need to specify a path for the parameters.csv, or a matrix.")
        })
    } else {
        if((!is.list(parameters)) & (!is.matrix(parameters))){
            parameters <- tryCatch(read.csv(parameters, stringsAsFactor=FALSE), error = function (e){
                stop("Cannot find neither parameters file neither the parameters matrix")
            })
        }}

    outLoc <- normalizePath(outLoc)
    outLocPlain <- basename(outLoc)
    currDir <- getwd()

    if(!dir.exists(outLoc)){
        dir.create(outLoc)
        warning(paste(outLoc," is not exists, so it was created"))
    }

    outLoc <- normalizePath(outLoc)
    parameterNames <- parameters[,1]
    pretag <- file.path(outLoc,preTag)
    ##reading the original epc file at the specified
    ## row numbers
    print("optiMuso is randomizing the epc parameters now...",quote = FALSE)
    if(iterations < 3000){
        randVals <- musoRand(parameters = parameters,constrains = NULL, iterations = 3000)
        randVals[[2]]<- randVals[[2]][sample(1:3000,iterations),]
    } else {
        randVals <- musoRand(parameters = parameters,constrains = NULL, iterations = iterations)
    }

    origEpc <- readValuesFromFile(settings$epc[2],randVals[[1]])
    partialResult <- matrix(ncol=length(randVals[[1]])+2*length(dataVar))
    colN <- randVals[[1]]
    colN[match(parameters[,2],randVals[[1]])] <- parameters[,1]
    colnames(partialResult) <- c(colN,sprintf("%s_likelihood",names(dataVar)),
                                      sprintf("%s_rmse",names(dataVar)))
    numParameters <- length(colN)
    partialResult[1:numParameters] <- origEpc
    ## Prepare the preservedCalib matrix for the faster
    ##  run.

    pretag <- file.path(outLoc,preTag)

     musoCodeToIndex  <- sapply(dataVar,function(musoCode){
           settings$dailyOutputTable[settings$dailyOutputTable$code == musoCode,"index"]
        })
    resultRange <- (numParameters + 1):(ncol(partialResult))
    ## Creating function for generating separate
    ## csv files for each run

    progBar <- txtProgressBar(1,iterations,style=3)
    settings$iniInput[2] %>%
        (function(x) paste0(dirname(x),"/",tools::file_path_sans_ext(basename(x)),"-tmp.",tools::file_ext(x))) %>%
        unlink
    randValues <- randVals[[2]]

    settings$calibrationPar <- randVals[[1]]

    if(!is.null(naVal)){
        measuredData <- as.data.frame(measuredData)
        measuredData[measuredData == naVal] <- NA
    }

    alignIndexes <- alignMuso(settings,measuredData)
    if(!is.null(uncertainity)){
        uncert <- measuredData[alignIndexes$meas,uncertainity]
    } else {
        uncert <- NULL
    }
    # browser()
    origModellOut <- calibMuso(settings=settings, silent=TRUE, skipSpinup = skipSpinup, postProcString=postProcString, modifyOriginal=modifyOriginal)
    partialResult[,resultRange] <- calcLikelihoodsAndRMSE(dataVar=dataVar, 
                                                          mod=origModellOut,
                                                          mes=measuredData,
                                                          likelihoods=likelihood,
                                                           alignIndexes=alignIndexes,
                                                           musoCodeToIndex = musoCodeToIndex,uncert=uncert)
    write.csv(x=origModellOut, file=paste0(pretag, 1, ".csv"))
    print("Running the model with the random epc values...", quote = FALSE)

    # if(!is.null(postProcString)){
    #     colNumb <- length(settings$dailyVarCodes) + 1
    # }
    
    write.csv(x=partialResult, file="preservedCalib.csv",row.names=FALSE)
    for(i in 2:(iterations+1)){
        # browser()
        tmp <- tryCatch(calibMuso(settings = settings,
                                  parameters = randValues[(i-1),],
                                  silent= TRUE,
                                  skipSpinup = skipSpinup, modifyOriginal=modifyOriginal, postProcString = postProcString), error = function (e) NULL)
        if(is.null(tmp)){
           partialResult[,resultRange] <- NA
        } else {
            partialResult[,resultRange] <- calcLikelihoodsAndRMSE(dataVar=dataVar, 
                                                          mod=tmp,
                                                          mes=measuredData,
                                                          likelihoods=likelihood,
                                                           alignIndexes=alignIndexes,
                                                           musoCodeToIndex = musoCodeToIndex, uncert = uncert)
        }


        partialResult[1:numParameters] <- randValues[(i-1),]
        write.table(x=partialResult, file="preservedCalib.csv", append=TRUE, row.names=FALSE,
                    sep=",", col.names=FALSE)
        write.csv(x=tmp, file=paste0(pretag, (i+1),".csv"))
        writeLines(as.character(i),"progress.txt")
    }

}

distributeCores <- function(iterations, numCores){
    perProcess<- iterations %/% numCores
    numSimu <- rep(perProcess,numCores)
    gainers <- sample(1:numCores, iterations %% numCores)
    numSimu[gainers] <- numSimu[gainers] + 1
    numSimu
}

prepareFromAgroMo <- function(fName){
    obs <- read.table(fName, stringsAsFactors=FALSE, sep = ";", header=T)
    obs <- reshape(obs, timevar="var_id", idvar = "date", direction = "wide")
    dateCols <- apply(do.call(rbind,(strsplit(obs$date, split = "-"))),2,as.numeric)
    colnames(dateCols) <- c("year", "month", "day")
    cbind.data.frame(dateCols, obs)
}


calcLikelihoodsAndRMSE <- function(dataVar, mod, mes, likelihoods, alignIndexes, musoCodeToIndex, uncert){

    likelihoodRMSE <- sapply(names(dataVar),function(key){
               # browser()
               modelled <- mod[alignIndexes$mod,musoCodeToIndex[key]]
               selected <- grep(sprintf("%s$", key), colnames(mes))
               measured <- mes[alignIndexes$meas,selected]
               modelled <- modelled[!is.na(measured)] 
               # uncert   <-   uncert[!is.na(measured)]
               measured <- measured[!is.na(measured)] 
               res <- c(likelihoods[[key]](modelled, measured, uncert),
                        sqrt(mean((modelled-measured)^2))
               )
               res
        })
    names(likelihoodRMSE) <- c(sprintf("%s_likelihood",dataVar), sprintf("%s_rmse",dataVar))

    return(c(likelihoodRMSE[1,],likelihoodRMSE[2,]))
}

agroLikelihood <- function(modVector,measured){
    mu <- measured[,grep("^mean", colnames(measured))]
    stdev <- measured[,grep("^sd", colnames(measured))]
    ndata <- nrow(measured)
    sum(sapply(1:ndata, function(x){
                  dnorm(modVector, mu[x], stdev[x], log = TRUE)
               }))
}

# prepareFromAgroMo("/home/hollorol/agromo/calibration/martonvasar/MV_highN.obs")

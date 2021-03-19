

copyToThreadDirs2 <- function(iniSource, thread_prefix = "thread", numCores, execPath="./",

                              executable = ifelse(Sys.info()[1]=="Linux", file.path(execPath, "muso"),
                                                                            file.path(execPath,"muso.exe"))){
    sapply(iniSource, function(x){
        flatMuso(x, execPath,
                 directory=file.path("tmp", paste0(thread_prefix,"_1"),tools::file_path_sans_ext(basename(x)),""), d =TRUE)
        file.copy(executable,
                  file.path("tmp", paste0(thread_prefix,"_1"),tools::file_path_sans_ext(basename(x))))
        tryCatch(file.copy(file.path(execPath,"cygwin.dll"),
                   file.path("tmp", paste0(thread_prefix,"_1"),tools::file_path_sans_ext(basename(x)))),function(e){"If you are in Windows..."})
    })

    sapply(2:numCores,function(thread){
               dir.create(sprintf("tmp/%s_%s",thread_prefix,thread), showWarnings=FALSE)
               file.copy(list.files(sprintf("tmp/%s_1",thread_prefix),full.names = TRUE),sprintf("tmp/%s_%s/",thread_prefix,thread),
                         recursive=TRUE, overwrite = TRUE)
    })

}


#' multiSiteCalib
#'
#' This funtion uses the Monte Carlo technique to uniformly sample the parameter space from user defined parameters of the Biome-BGCMuSo model. The sampling algorithm ensures that the parameters are constrained by the model logic which means that parameter dependencies are fully taken into account (parameter dependency means that e.g leaf C:N ratio must be smaller than C:N ratio of litter; more complicated rules apply to the allocation parameters where the allocation fractions to different plant compartments must sum up 1). This function implements a mathematically correct solution to provide uniform distriution for all selected parameters. 
#' @author Roland HOLLOS
#' @importFrom future future
#' @export
multiSiteCalib <- function(measurements,
                           calTable,
                           parameters,
                           dataVar, 
                           iterations = 100,
                           likelihood,
                           execPath,
                           thread_prefix="thread",
                           numCores = (parallel::detectCores()-1),
                           pb = txtProgressBar(min=0, max=iterations, style=3),
                           pbUpdate = setTxtProgressBar){

    future::plan(future::multisession)
    # file.remove(list.files(path = "tmp", pattern="progress.txt", recursive = TRUE, full.names=TRUE))
    # file.remove(list.files(path = "tmp", pattern="preservedCalib.csv", recursive = TRUE, full.names=TRUE))
    unlink("tmp",recursive=TRUE)

    #   ____                _         _   _                        _     
    #  / ___|_ __ ___  __ _| |_ ___  | |_| |__  _ __ ___  __ _  __| |___ 
    # | |   | '__/ _ \/ _` | __/ _ \ | __| '_ \| '__/ _ \/ _` |/ _` / __|
    # | |___| | |  __/ (_| | ||  __/ | |_| | | | | |  __/ (_| | (_| \__ \
    #  \____|_|  \___|\__,_|\__\___|  \__|_| |_|_|  \___|\__,_|\__,_|___/
                                                                       
    
    copyToThreadDirs2(iniSource=calTable$site_id, numCores=numCores, execPath=execPath)

    #  ____                _   _                        _     
    # |  _ \ _   _ _ __   | |_| |__  _ __ ___  __ _  __| |___ 
    # | |_) | | | | '_ \  | __| '_ \| '__/ _ \/ _` |/ _` / __|
    # |  _ <| |_| | | | | | |_| | | | | |  __/ (_| | (_| \__ \
    # |_| \_\\__,_|_| |_|  \__|_| |_|_|  \___|\__,_|\__,_|___/
                                                            
    threadCount <- distributeCores(iterations, numCores) 
    fut <- lapply(1:numCores, function(i) {
                      # browser()
         future({
                      tryCatch(

                        multiSiteThread(measuredData = measurements, parameters = parameters, calTable=calTable,
                                         dataVar = dataVar, iterations = threadCount[i],
                                         likelihood = likelihood, threadNumber= i)
                      , error = function(e){
                                            writeLines(as.character(iterations),"progress.txt")
                                        })
         })
    })

    #                _       _                                               
    # __      ____ _| |_ ___| |__    _ __  _ __ ___   __ _ _ __ ___  ___ ___ 
    # \ \ /\ / / _` | __/ __| '_ \  | '_ \| '__/ _ \ / _` | '__/ _ \/ __/ __|
    #  \ V  V / (_| | || (__| | | | | |_) | | | (_) | (_| | | |  __/\__ \__ \
    #   \_/\_/ \__,_|\__\___|_| |_| | .__/|_|  \___/ \__, |_|  \___||___/___/
    #                               |_|              |___/                   

    getProgress <- function(){
        # threadfiles <- list.files(settings$inputLoc, pattern="progress.txt", recursive = TRUE)
        threadfiles <- list.files(pattern="progress.txt", recursive = TRUE)
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
    while(progress < iterations){
        Sys.sleep(1)
        progress <- tryCatch(getProgress(), error=function(e){progress})
        if(is.null(pb)){
            pbUpdate(as.numeric(progress))
        } else {
            pbUpdate(pb,as.numeric(progress))
        }
    }
    if(!is.null(pb)){
        close(pb)
    }

    #   ____                _     _            
    #  / ___|___  _ __ ___ | |__ (_)_ __   ___ 
    # | |   / _ \| '_ ` _ \| '_ \| | '_ \ / _ \
    # | |__| (_) | | | | | | |_) | | | | |  __/
    #  \____\___/|_| |_| |_|_.__/|_|_| |_|\___|

    resultFiles <- list.files(pattern="preservedCalib.*csv$",recursive=TRUE)
    res0 <- read.csv(grep("thread_1/",resultFiles, value=TRUE),stringsAsFactors=FALSE)
    resultFilesSans0 <- grep("thread_1/", resultFiles, value=TRUE, invert=TRUE)
    # results <- do.call(rbind,lapply(resultFilesSans0, function(f){read.csv(f, stringsAsFactors=FALSE)}))
    resultsSans0 <- lapply(resultFilesSans0, function(f){read.csv(f, stringsAsFactors=FALSE, header=FALSE)})
    resultsSans0 <- do.call(rbind,resultsSans0)
    colnames(resultsSans0) <- colnames(res0)
    results <- (rbind(res0,resultsSans0))
    write.csv(results,"result.csv")
    calibrationPar <- future::value(fut[[1]], stdout = FALSE, signal=FALSE)[["calibrationPar"]]
    origModOut <- future::value(fut[[1]], stdout = FALSE, signal=FALSE)[["origModOut"]]
    # Just single objective version TODO:Multiobjective
    bestCase <- which.max(results[,ncol(results)-1])
    parameters <- results[bestCase,1:(ncol(results)-2)] # the last two column is the (log) likelihood and the rmse
    #TODO: Have to put that before multiSiteThread, we should not have to calculate it at every iterations 

    firstDir <- list.dirs("tmp/thread_1",full.names=TRUE,recursive =FALSE)[1]
    epcFile <- list.files(firstDir, pattern = "\\.epc",full.names=TRUE)
    settingsProto <- setupMuso(inputLoc = firstDir,
                               iniInput =rep(list.files(firstDir, pattern = "\\.ini",full.names=TRUE),2))
    alignIndexes <- commonIndexes(settingsProto, measurements)
    musoCodeToIndex  <- sapply(dataVar,function(musoCode){
      settingsProto$dailyOutputTable[settingsProto$dailyOutputTable$code == musoCode,"index"]
    })
    setwd("tmp/thread_1")
    aposteriori<- spatialRun(settingsProto, calibrationPar, parameters, calTable)
    file.copy(list.files(list.dirs(full.names=TRUE, recursive=FALSE)[1],pattern=".*\\.epc", full.names=TRUE),
              "../../multiSiteOptim.epc", overwrite=TRUE)
    setwd("../../")
    #TODO: Have to put that before multiSiteThread, we should not have to calculate it at every iterations 
    nameGroupTable <- calTable 
    nameGroupTable[,1] <- tools::file_path_sans_ext(basename(nameGroupTable[,1]))
    res <- list()
    res[["calibrationPar"]] <- calibrationPar
    res[["parameters"]] <- parameters
    res[["comparison"]] <- compareCalibratedWithOriginal(key="grainDM", modOld=origModOut, modNew=aposteriori, mes=measurements,
                                                                 likelihoods=likelihood,
                                                                 alignIndexes=alignIndexes,
                                                                 musoCodeToIndex = musoCodeToIndex,
                                                                 nameGroupTable = nameGroupTable, mean)
    res[["likelihood"]] <- results[bestCase,ncol(results)-1]
    comp <- res$comparison
    res[["originalMAE"]] <-mean(abs((comp[,1]-comp[,3])))
    res[["MAE"]] <- mean(abs((comp[,2]-comp[,3])))
    res[["RMSE"]] <- results[bestCase,ncol(results)]
    res[["originalRMSE"]] <- sqrt(mean((comp[,1]-comp[,3])^2))
    png("calibRes.png")
    opar <- par(mar=c(5,5,4,2)+0.1, xpd=FALSE)
        with(data=res$comparison, {
             plot(measured,original,
                  ylim=c(min(c(measured,original,calibrated)),
                         max(c(measured,original,calibrated))),
                  xlim=c(min(c(measured,original,calibrated)),
                         max(c(measured,original,calibrated))),
                  xlab=expression("measured "~(kg[C]~m^-2)),
                  ylab=expression("simulated "~(kg[C]~m^-2)),
                  cex.lab=1.3,
                  col="red",
                  pch=19,
                  pty="s"
             )
             points(measured,calibrated, pch=19, col="blue")
             abline(0,1)
             legend(x="top",
                    pch=c(1,19),
                    inset=c(0,-0.1),
                    legend=c("original","calibrated"),
                    ncol=2,
                    box.lty=0,
                    xpd=TRUE
             )
        })
    dev.off()
    return(res)
}

multiSiteThread <- function(measuredData, parameters = NULL, startDate = NULL,
                     endDate = NULL, formatString = "%Y-%m-%d", calTable,
                     dataVar, outLoc = "./calib",
                     outVars = NULL, iterations = 300,
                     skipSpinup = TRUE, plotName = "calib.jpg",
                     modifyOriginal=TRUE, likelihood, uncertainity = NULL,
                     naVal = NULL, postProcString = NULL, threadNumber) {

    originalRun <- list()
    nameGroupTable <- calTable 
    nameGroupTable[,1] <- tools::file_path_sans_ext(basename(nameGroupTable[,1]))
    setwd(paste0("tmp/thread_",threadNumber))
    firstDir <- list.dirs(full.names=FALSE,recursive =FALSE)[1]
    epcFile <- list.files(firstDir, pattern = "\\.epc",full.names=TRUE)
    settingsProto <- setupMuso(inputLoc = firstDir,
                               iniInput =rep(list.files(firstDir, pattern = "\\.ini",full.names=TRUE),2))

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

    print("optiMuso is randomizing the epc parameters now...",quote = FALSE)
    if(iterations < 3000){
        randVals <- musoRand(parameters = parameters,constrains = NULL, iterations = 3000)
        randVals[[2]]<- randVals[[2]][sample(1:3000,iterations),]
    } else {
        randVals <- musoRand(parameters = parameters,constrains = NULL, iterations = iterations)
    }

    origEpc <- readValuesFromFile(epcFile, randVals[[1]])
    partialResult <- matrix(ncol=length(randVals[[1]])+2*length(dataVar))
    colN <- randVals[[1]]
    colN[match(parameters[,2],randVals[[1]])] <- parameters[,1]
    colN[match(parameters[,2], randVals[[1]])[!is.na(match(parameters[,2],randVals[[1]]))]] <- parameters[,1]
    colnames(partialResult) <- c(colN,sprintf("%s_likelihood",names(dataVar)),
                                      sprintf("%s_rmse",names(dataVar)))
    numParameters <- length(colN)
    partialResult[1:numParameters] <- origEpc
    ## Prepare the preservedCalib matrix for the faster
    ##  run.
    musoCodeToIndex  <- sapply(dataVar,function(musoCode){
      settingsProto$dailyOutputTable[settingsProto$dailyOutputTable$code == musoCode,"index"]
    })

    resultRange <- (numParameters + 1):(ncol(partialResult))
    randValues <- randVals[[2]]

    settingsProto$calibrationPar <- randVals[[1]]

    if(!is.null(naVal)){
        measuredData <- as.data.frame(measuredData)
        measuredData[measuredData == naVal] <- NA
    }
    resIterate <- 1:nrow(calTable)
    names(resIterate) <- tools::file_path_sans_ext(basename(calTable[,1]))
    alignIndexes <- commonIndexes(settingsProto, measuredData)
    if(threadNumber == 1){
        originalRun[["calibrationPar"]] <- randVals[[1]]

        origModOut <- lapply(resIterate, function(i){
            dirName <- tools::file_path_sans_ext(basename(calTable[i,1]))
            setwd(dirName)
            settings <- settingsProto 
            settings$outputLoc <- settings$inputLoc <- "./"
            settings$iniInput <- settings$inputFiles <- rep(paste0(dirName,".ini"),2)
            settings$outputNames <- rep(dirName,2)
            settings$executable <- ifelse(Sys.info()[1]=="Linux","./muso","./muso.exe") # set default exe option at start wold be better
            res <- tryCatch(calibMuso(settings=settings,parameters =origEpc, silent = TRUE, skipSpinup = TRUE), error=function(e){NA})
            setwd("../")
            res
        })
        originalRun[["origModOut"]] <- origModOut

        partialResult[,resultRange] <- calcLikelihoodsForGroups(dataVar=dataVar, 
                                                                mod=origModOut,
                                                                mes=measuredData,
                                                                likelihoods=likelihood,
                                                                alignIndexes=alignIndexes,
                                                                musoCodeToIndex = musoCodeToIndex,nameGroupTable = nameGroupTable, mean)

        write.csv(x=randVals[[1]],"../randIndexes.csv")
        write.csv(x=partialResult, file="preservedCalib.csv",row.names=FALSE)
    }

    print("Running the model with the random epc values...", quote = FALSE)
    for(i in 2:(iterations+1)){
        # browser()
        tmp <- lapply(resIterate, function(siteI){
            dirName <- tools::file_path_sans_ext(basename(calTable[siteI,1]))
            setwd(dirName)
            settings <- settingsProto 
            settings$outputLoc <- settings$inputLoc <- "./"
            settings$iniInput <- settings$inputFiles <- rep(paste0(dirName,".ini"),2)
            settings$outputNames <- rep(dirName,2)
            settings$executable <- ifelse(Sys.info()[1]=="Linux","./muso","./muso.exe") # set default exe option at start wold be better

            res <- tryCatch(calibMuso(settings=settings,parameters=randValues[(i-1),], silent = TRUE, skipSpinup = TRUE), error=function(e){NA})
            setwd("../")
            res
        })

        if(is.null(tmp)){
           partialResult[,resultRange] <- NA
        } else {
            partialResult[,resultRange] <- calcLikelihoodsForGroups(dataVar=dataVar, 
                                                                mod=tmp,
                                                                mes=measuredData,
                                                                likelihoods=likelihood,
                                                                alignIndexes=alignIndexes,
                                                                musoCodeToIndex = musoCodeToIndex,nameGroupTable = nameGroupTable, mean)

                
                
                
                

        partialResult[1:numParameters] <- randValues[(i-1),]
        write.table(x=partialResult, file="preservedCalib.csv", append=TRUE, row.names=FALSE,
                    sep=",", col.names=FALSE)
        # write.csv(x=tmp, file=paste0(pretag, (i+1),".csv"))
        writeLines(as.character(i-1),"progress.txt") #UNCOMMENT IMPORTANT
    }
    }
    if(threadNumber == 1){
        return(originalRun)
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

calcLikelihoodsForGroups <- function(dataVar, mod, mes, likelihoods, alignIndexes, musoCodeToIndex, nameGroupTable, groupFun){
    likelihoodRMSE <- sapply(names(dataVar),function(key){
              modelled <- as.vector(unlist(sapply(sort(names(alignIndexes)),
                                           function(domain_id){
                                            apply(do.call(cbind,
                                                          lapply(nameGroupTable[,1][nameGroupTable[,2] == domain_id],
                                                                 function(site){mod[[site]][alignIndexes[[domain_id]]$model,musoCodeToIndex[key]]
                                        })),1,groupFun)




                                        })))


               measuredGroups <- split(mes,mes$domain_id)
               measured <- do.call(rbind.data.frame, lapply(names(measuredGroups), function(domain_id){
                                                    measuredGroups[[domain_id]][alignIndexes[[domain_id]]$meas,]
                                        }))
               measured <- measured[measured$var_id == key,]

               # measured <- measured[!is.na(measured)] 
               res <- c(likelihoods[[key]](modelled, measured),
                        sqrt(mean((modelled-measured$mean)^2))
               )
               print(abs(mean(modelled)-mean(measured$mean)))
               # browser()
               res
        })
    names(likelihoodRMSE) <- c(sprintf("%s_likelihood",dataVar), sprintf("%s_rmse",dataVar))
    return(c(likelihoodRMSE[1,],likelihoodRMSE[2,]))
}

commonIndexes <- function (settings,measuredData) {
        # Have to fix for other starting points also
        modelDates <- seq(from= as.Date(sprintf("%s-01-01",settings$startYear)), 
            by="days",
            to=as.Date(sprintf("%s-12-31",settings$startYear+settings$numYears-1)))
        modelDates <- grep("-02-29",modelDates,invert=TRUE, value=TRUE)

        lapply(split(measuredData,measuredData$domain_id),function(x){
            measuredDates <- x$date
            modIndex <- match(as.Date(measuredDates), as.Date(modelDates))
            measIndex <- which(!is.na(modIndex))
            modIndex <- modIndex[!is.na(modIndex)]
            cbind.data.frame(model=modIndex,meas=measIndex)
        })
} 

agroLikelihood <- function(modVector,measured){
    mu <- measured[,grep("mean", colnames(measured))]
    stdev <- measured[,grep("^sd", colnames(measured))]
    ndata <- nrow(measured)
    sum(sapply(1:ndata, function(x){
                  dnorm(modVector, mu[x], stdev[x], log = TRUE)
               }), na.rm=TRUE)
}


#' compareCalibratedWithOriginal
#'
#' This functions compareses the likelihood and the RMSE values of the simulations and the measurements
#' @param key
compareCalibratedWithOriginal <- function(key, modOld, modNew, mes,
                             likelihoods, alignIndexes, musoCodeToIndex, nameGroupTable,
                             groupFun){

    original <- as.vector(unlist(sapply(sort(names(alignIndexes)),
                    function(domain_id){
                        apply(do.call(cbind,
                            lapply(nameGroupTable$site_id[nameGroupTable$domain_id == domain_id],
                                   function(site){
                                       modOld[[site]][alignIndexes[[domain_id]]$model,musoCodeToIndex[key]]
                            })),1,groupFun)
                    })))
    calibrated <- as.vector(unlist(sapply(sort(names(alignIndexes)),
                    function(domain_id){
                        apply(do.call(cbind,
                            lapply(nameGroupTable$site_id[nameGroupTable$domain_id == domain_id],
                                   function(site){
                                       modNew[[site]][alignIndexes[[domain_id]]$model,musoCodeToIndex[key]]
                            })),1,groupFun)
                    })))
    measuredGroups <- split(mes,mes$domain_id)
    measured <- do.call(rbind.data.frame, lapply(names(measuredGroups), function(domain_id){
                                                    measuredGroups[[domain_id]][alignIndexes[[domain_id]]$meas,]
                                        }))
    measured <- measured[measured$var_id == key,]
    return(data.frame(original = original, calibrated = calibrated,measured=measured$mean))
}

# plotDiff(key="grainDM", 
#                                                                 modOld=origModOut,
#                                                                 modNew=tmp,
#                                                                 # mes=measuredData,
#                                                                 mes=mes,
#                                                                 likelihoods=likelihood,
#                                                                 alignIndexes=alignIndexes,
#                                                                 musoCodeToIndex = musoCodeToIndex,nameGroupTable = nameGroupTable, mean)
#
#
#  comp <- compareCalibratedWithOriginal(key="grainDM", 
#                                                                 modOld=origModOut,
#                                                                 modNew=tmp,
#                                                                 # mes=measuredData,
#                                                                 mes=mes,
#                                                                 likelihoods=likelihood,
#                                                                 alignIndexes=alignIndexes,
#                                                                 musoCodeToIndex = musoCodeToIndex,nameGroupTable = nameGroupTable, mean)
#
#
#
#
#
# plotDiff <-  function(key, modOld, modNew, mes,
#                              likelihoods, alignIndexes, musoCodeToIndex, nameGroupTable,
#                              groupFun){
#     compme <- compareCalibratedWithOriginal(key,modOld,modNew,mes,
#                                             likelihoods,alignIndexes,musoCodeToIndex,nameGroupTable,groupFun)
#
#     with(data=compme, {
#          plot(measured,original,ylim=c(min(measured),max(measured)))
#          points(measured,calibrated,pch=20)
#          abline(0,1)
# })}
#
# likelihood <- list(grainDM=function(x,y){exp(-1./2.*sqrt(mean((x-y$mean)^2.)))})
# likelihood <- list(grainDM=agroLikelihood)

spatialRun <- function(settingsProto,calibrationPar, parameters, calTable){
    resIterate <- 1:nrow(calTable)
    names(resIterate) <- tools::file_path_sans_ext(basename(calTable[,1]))
    modOut <- lapply(resIterate, function(i){
            dirName <- tools::file_path_sans_ext(basename(calTable[i,1]))
            setwd(dirName)
            settings <- settingsProto 
            settings$outputLoc <- settings$inputLoc <- "./"
            settings$iniInput <- settings$inputFiles <- rep(paste0(dirName,".ini"),2)
            settings$outputNames <- rep(dirName,2)
            settings$calibrationPar <- calibrationPar
            settings$executable <- ifelse(Sys.info()[1]=="Linux","./muso","./muso.exe") # set default exe option at start wold be better
            res <- tryCatch(calibMuso(settings=settings,parameters =parameters, silent = TRUE, skipSpinup = TRUE), error=function(e){NA})
            setwd("../")
            res
        })
    modOut
}

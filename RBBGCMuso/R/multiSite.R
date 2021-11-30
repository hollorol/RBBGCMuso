`%between%` <- function(x, y){
    (x <= y[2]) & (x >= y[1])
}

annualAggregate <- function(x, aggFun){
    tapply(x, rep(1:(length(x)/365), each=365), aggFun)
}

SELECT <- function(x, selectPart){
    if(!is.function(selectPart)){
        index <- as.numeric(selectPart)
        tapply(x,rep(1:(length(x)/365),each=365), function(y){
            y[index]
        })
    } else {
        tapply(x,rep(1:(length(x)/365),each=365), selectPart)
    }
}
    
bVectToInt<- function(bin_vector){
    bin_vector <- rev(as.integer(bin_vector))
    packBits(as.raw(c(bin_vector,numeric(32-length(bin_vector)))),"integer")
}

constMatToDec <- function(constRes){
    tab <- table(apply(constRes,2,function(x){paste(x,collapse=" ")}))
    bitvect <- strsplit(names(tab[which.max(tab)]),split=" ")[[1]]
    bVectToInt(bitvect)
}

compose <- function(expr){
   splt <- strsplit(expr,split="\\|")[[1]]
   lhs <- splt[1]
   rhs <- splt[2]
   penv <- parent.frame()
   lhsv <- eval(parse(text=lhs),envir=penv)
   penv[["lhsv"]] <- lhsv
   place <- regexpr("\\.[^0-9a-zA-Z]",rhs)

   if(place != -1){
       finalExpression <- paste0(substr(rhs, 1, place -1),"lhsv",
                                 substr(rhs, place + 1, nchar(rhs)))
   } else {
        finalExpression <- paste0(rhs,"(lhsv)")
   }
   eval(parse(text=finalExpression),envir=penv)
}

compoVect <- function(mod, constrTable, fileToWrite = "const_results.data"){
    with(as.data.frame(mod), {
             nexpr  <- nrow(constrTable)
             filtered  <- numeric(nexpr)
             vali <- numeric(nexpr) 
             for(i in 1:nexpr){
                 val <- compose(constrTable[i,1])
                 filtered[i] <- (val <= constrTable[i,3]) &&
                             (val >= constrTable[i,2])
                 vali[i] <- val
             }

             write(paste(vali,collapse=","), fileToWrite, append=TRUE)
             filtered
    })
}

modCont <- function(expr, datf, interval, dumping_factor){
    tryCatch({
        if((with(datf,eval(parse(text=expr))) %between% interval)){
            return(NA)
        } else{
            return(dumping_factor)
        }
    },
        error = function(e){
            stop(sprintf("Cannot find the variable names in the dataframe, detail:\n%s",
                         e))
        })
}

copyToThreadDirs2 <- function(iniSource, thread_prefix = "thread", numCores, execPath="./",

                              executable = ifelse(Sys.info()[1]=="Linux", file.path(execPath, "muso"),
                                                                            file.path(execPath,"muso.exe"))){
    sapply(iniSource, function(x){
        flatMuso(x, execPath,
                 directory=file.path("tmp", paste0(thread_prefix,"_1"),tools::file_path_sans_ext(basename(x)),""), d =TRUE)
        file.copy(executable,
                  file.path("tmp", paste0(thread_prefix,"_1"),tools::file_path_sans_ext(basename(x))))
        tryCatch(file.copy(file.path(execPath,"cygwin1.dll"),
                   file.path("tmp", paste0(thread_prefix,"_1"),tools::file_path_sans_ext(basename(x)))),
                 error = function(e){"If you are in Windows..."})
    })
    sapply(2:numCores,function(thread){
               dir.create(sprintf("tmp/%s_%s",thread_prefix,thread), showWarnings=FALSE)
               file.copy(list.files(sprintf("tmp/%s_1",thread_prefix),full.names = TRUE),sprintf("tmp/%s_%s/",thread_prefix,thread),
                         recursive=TRUE, overwrite = TRUE)
    })

}


#' multiSiteCalib
#'
#' This funtion uses the Monte Carlo technique to uniformly sample the parameter space from user defined parameters of the Biome-BGCMuSo model. The sampling algorithm ensures that the parameters are constrained by the model logic which means that parameter dependencies are fully taken into account (parameter dependency means that e.g leaf C:N ratio must be smaller than C:N ratio of litter; more complicated rules apply to the allocation parameters where the allocation fractions to different plant compartments must sum up 1). This function implements a mathematically correct solution to provide uniform distriution of the random parameters on convex polytopes. 
#' @author Roland HOLLOS
#' @importFrom future future
#' @importFrom rpart rpart rpart.control
#' @importFrom rpart.plot rpart.plot
#' @param measuremets The table which contains the measurements
#' @param calTable A dataframe which contantains the ini file locations and the domains they belongs to
#' @param parameters A dataframe with the name, the minimum, and the maximum value for the parameters used in MonteCarlo experiment 
#' @param dataVar A named vector where the elements are the MuSo variable codes and the names are the same as provided in measurements and likelihood
#' @param iterations The number of MonteCarlo experiments to be executed
#' @param burnin Currently not used, altought it is the length of burnin period of the MCMC sampling used to generate random parameters
#' @param likelihood A list of likelihood functions which names are linked to dataVar
#' @param execPath If you are running the calibration from different location than the MuSo executable, you have to provide the path
#' @param thread_prefix The prefix of thread directory names in the tmp directory created during the calibrational process
#' @param numCores The number of processes used during the calibration. At default it uses one less than the number of threads available
#' @param pb The progress bar function. If you use (web-)GUI you can provide a different function
#' @param pbUpdate The update function for pb (progress bar)
#' @param copyThread A boolean, recreate tmp directory for calibration or not (case of repeating the calibration)
#' @param contsraints A dataframe containing the constraints logic the minimum and a maximum value for the calibration.
#' @param th A trashold value for multisite calibration. What percentage of the site should satisfy the constraints. 
#' @param treeControl A list which controls (maximal complexity, maximal depth) the details of the decession tree making.
#' @export
multiSiteCalib <- function(measurements,
                           calTable,
                           parameters,
                           dataVar, 
                           iterations = 100,
                           burnin =ifelse(iterations < 3000, 3000, NULL),
                           likelihood,
                           execPath,
                           thread_prefix="thread",
                           numCores = (parallel::detectCores()-1),
                           pb = txtProgressBar(min=0, max=iterations, style=3),
                           pbUpdate = setTxtProgressBar,
                           copyThread = TRUE,
                           constraints=NULL, th = 10, treeControl=rpart.control()
                           ){
    future::plan(future::multisession)
    # file.remove(list.files(path = "tmp", pattern="progress.txt", recursive = TRUE, full.names=TRUE))
    # file.remove(list.files(path = "tmp", pattern="preservedCalib.csv", recursive = TRUE, full.names=TRUE))

    #   ____                _         _   _                        _     
    #  / ___|_ __ ___  __ _| |_ ___  | |_| |__  _ __ ___  __ _  __| |___ 
    # | |   | '__/ _ \/ _` | __/ _ \ | __| '_ \| '__/ _ \/ _` |/ _` / __|
    # | |___| | |  __/ (_| | ||  __/ | |_| | | | | |  __/ (_| | (_| \__ \
    #  \____|_|  \___|\__,_|\__\___|  \__|_| |_|_|  \___|\__,_|\__,_|___/
    if(copyThread){
        unlink("tmp",recursive=TRUE)
        copyToThreadDirs2(iniSource=calTable$site_id, numCores=numCores, execPath=execPath)
    } else {
        print("copy skipped")
        file.remove(file.path(list.dirs("tmp",recursive=FALSE),"progress.txt"))
        file.remove(file.path(list.dirs("tmp", recursive=FALSE), "const_results.data"))
    }

    #  ____                _   _                        _     
    # |  _ \ _   _ _ __   | |_| |__  _ __ ___  __ _  __| |___ 
    # | |_) | | | | '_ \  | __| '_ \| '__/ _ \/ _` |/ _` / __|
    # |  _ <| |_| | | | | | |_| | | | | |  __/ (_| | (_| \__ \
    # |_| \_\\__,_|_| |_|  \__|_| |_|_|  \___|\__,_|\__,_|___/
                                                            
    threadCount <- distributeCores(iterations, numCores) 
    fut <- lapply(1:numCores, function(i) {
         future({
                      tryCatch(

                               {
                                  result <- multiSiteThread(measuredData = measurements, parameters = parameters, calTable=calTable, 
                                                   dataVar = dataVar, iterations = threadCount[i],
                                                   likelihood = likelihood, threadNumber= i, constraints=constraints, th=th)
                                   setwd("../../")
                                   return(result)
                               }

                      , error = function(e){
                                            saveRDS(e,"error.RDS")
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
    if(!is.null(constraints)){
        constRes <- file.path(list.dirs("tmp", recursive=FALSE), "const_results.data")
        constRes <- lapply(constRes, function(f){read.csv(f, stringsAsFactors=FALSE, header=FALSE)})
        constRes <- do.call(rbind,constRes)
        write.csv(constRes, "constRes.csv")
    }
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
    if(!is.null(constraints)){
        notForTree <- c(seq(from = (length(calibrationPar)+1), length.out=3))
        notForTree <- c(notForTree,which(sapply(seq_along(calibrationPar),function(i){sd(results[,i])==0})))
        treeData <- results[,-notForTree]
        treeData["failType"] <- as.factor(results$failType)
        if(ncol(treeData) > 4){
            rp <- rpart(failType ~ .,data=treeData,control=treeControl)
            svg("treeplot.svg")
            tryCatch(rpart.plot(rp), error = function(e){
                print(e)
            })
            dev.off()
        }
    }
   origModOut <- future::value(fut[[1]], stdout = FALSE, signal=FALSE)[["origModOut"]]
    # Just single objective version TODO:Multiobjective
    results <- results[results[,"Const"] == 1,]
    if(nrow(results)==0){
        stop("No simulation suitable for constraints\n Please see treeplot.png for explanation, if you have more than four parameters.")
    }
    bestCase <- which.max(results[,length(calibrationPar) + 1])
    parameters <- results[bestCase,1:length(calibrationPar)] # the last two column is the (log) likelihood and the rmse
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
    file.copy(list.files(list.dirs(full.names=TRUE, recursive=FALSE)[1], pattern=".*\\.epc", full.names=TRUE),
              "../../multiSiteOptim.epc", overwrite=TRUE)
    setwd("../../")
    #TODO: Have to put that before multiSiteThread, we should not have to calculate it at every iterations 
    nameGroupTable <- calTable 
    nameGroupTable[,1] <- tools::file_path_sans_ext(basename(nameGroupTable[,1]))
    res <- list()
    res[["calibrationPar"]] <- calibrationPar
    res[["parameters"]] <- parameters
    res[["comparison"]] <- compareCalibratedWithOriginal(key = "grainDM", modOld=origModOut, modNew=aposteriori, mes=measurements,
                                                                 likelihoods = likelihood,
                                                                 alignIndexes = alignIndexes,
                                                                 musoCodeToIndex = musoCodeToIndex,
                                                                 nameGroupTable = nameGroupTable, mean)
    res[["likelihood"]] <- results[bestCase,ncol(results)-2]
    comp <- res$comparison
    res[["originalMAE"]] <- mean(abs((comp[,1]-comp[,3])))
    res[["MAE"]] <- mean(abs((comp[,2]-comp[,3])))
    res[["RMSE"]] <- results[bestCase,ncol(results)-2]
    res[["originalRMSE"]] <- sqrt(mean((comp[,1]-comp[,3])^2))
    res[["originalR2"]] <- summary(lm(measured ~ original,data=res$comparison))$r.squared
    res[["R2"]] <- summary(lm(measured ~ calibrated, data=res$comparison))$r.squared
    saveRDS(res,"results.RDS")
    png("calibRes.png")
    opar <- par(mar=c(5,5,4,2)+0.1, xpd=FALSE)
        with(data=res$comparison, {
             plot(measured,original,
                  ylim=c(min(c(measured,original,calibrated)),
                         max(c(measured,original,calibrated))),
                  xlim=c(min(c(measured,original,calibrated)),
                         max(c(measured,original,calibrated))),
                  xlab=expression("measured "~(kg[DM]~m^-2)),
                  ylab=expression("simulated "~(kg[DM]~m^-2)),
                  cex.lab=1.3,
                  col="red",
                  pch=19,
                  pty="s"
             )
             points(measured,calibrated, pch=19, col="blue")
             abline(0,1)
             legend(x="top",
                    pch=c(19,19),
                    col=c("red","blue"),
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

#' multiSiteThread
#'
#' This is an 
#' @author Roland HOLLOS


multiSiteThread <- function(measuredData, parameters = NULL, startDate = NULL,
                     endDate = NULL, formatString = "%Y-%m-%d", calTable,
                     dataVar, outLoc = "./calib",
                     outVars = NULL, iterations = 300,
                     skipSpinup = TRUE, plotName = "calib.jpg",
                     modifyOriginal=TRUE, likelihood, uncertainity = NULL, burnin=NULL,
                     naVal = NULL, postProcString = NULL, threadNumber, constraints=NULL,th=10) {

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
    randVals <- musoRand(parameters = parameters,constrains = NULL, iterations = iterations)

    origEpc <- readValuesFromFile(epcFile, randVals[[1]])
    partialResult <- matrix(ncol=length(randVals[[1]])+2*length(dataVar) + 2)
    colN <- randVals[[1]]
    colN[match(parameters[,2],randVals[[1]])] <- parameters[,1]
    colN[match(parameters[,2], randVals[[1]])[!is.na(match(parameters[,2],randVals[[1]]))]] <- parameters[,1]
    colnames(partialResult) <- c(colN,sprintf("%s_likelihood",names(dataVar)),
                                      sprintf("%s_rmse",names(dataVar)),"Const", "failType")
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
                                                                musoCodeToIndex = musoCodeToIndex,nameGroupTable = nameGroupTable, groupFun=mean, constraints=constraints,th=th)

        write.csv(x=randVals[[1]],"../randIndexes.csv")
        write.csv(x=partialResult, file="preservedCalib.csv",row.names=FALSE)
    }

    print("Running the model with the random epc values...", quote = FALSE)
    for(i in 2:(iterations+1)){
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
                                                                musoCodeToIndex = musoCodeToIndex,nameGroupTable = nameGroupTable, groupFun=mean, constraints = constraints, th=th)

                
                
                
                

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

    return(0)
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

calcLikelihoodsForGroups <- function(dataVar, mod, mes,
                                     likelihoods, alignIndexes, musoCodeToIndex,
                                     nameGroupTable, groupFun, constraints,
                                     th = 10){

    if(!is.null(constraints)){
                         constRes<- sapply(mod,function(m){
                            compoVect(m,constraints)
                         })

                        failType <- constMatToDec(constRes)
    }

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

               res <- c(likelihoods[[key]](modelled, measured),
                        sqrt(mean((modelled-measured$mean)^2))
               )
               print(abs(mean(modelled)-mean(measured$mean)))
               res
        })

    likelihoodRMSE <- c(likelihoodRMSE[1,], likelihoodRMSE[2,],
             ifelse((100 * sum(apply(constRes, 2, prod)) / ncol(constRes)) >= th,
                    1,0), failType)
    names(likelihoodRMSE) <- c(sprintf("%s_likelihood",dataVar), sprintf("%s_rmse",dataVar), "Const", "failType")
    return(likelihoodRMSE)
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

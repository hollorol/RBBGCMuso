#' calibrateMuso
#'
#' This funtion uses the Monte Carlo technique to uniformly sample the parameter space from user defined parameters of the Biome-BGCMuSo model. The sampling algorithm ensures that the parameters are constrained by the model logic which means that parameter dependencies are fully taken into account (parameter dependency means that e.g leaf C:N ratio must be smaller than C:N ratio of litter; more complicated rules apply to the allocation parameters where the allocation fractions to different plant compartments must sum up 1). This function implements a mathematically correct solution to provide uniform distriution for all selected parameters. 
#' @author Roland HOLLOS
#' @importFrom future future
#' @export
calibrateMuso <- function(measuredData, parameters = "parameters.csv", startDate = NULL,
                     endDate = NULL, formatString = "%Y-%m-%d",
                     dataVar, outLoc = "./calib",
                     preTag = "cal-", settings =  setupMuso(),
                     outVars = NULL, iterations = 100,
                     skipSpinup = TRUE, plotName = "calib.jpg",
                     modifyOriginal=TRUE, likelihood, uncertainity = NULL,
                     naVal = NULL, postProcString = NULL,
                     sourceFile=NULL, # bases for musoRand if dependecy group is not fully defined by parameters.csv 
                     thread_prefix="thread", numCores = max(c(parallel::detectCores()-1,1)), pb = txtProgressBar(min=0, max=iterations, style=3),
                     constraints=NULL,
                     maxLikelihoodEpc=TRUE,
                     fileToChange = "epc",
                     pbUpdate = setTxtProgressBar, outputLoc="./", method="GLUE",lg = FALSE, w=NULL, ...){
    
    parameters <- read.csv(parameters, stringsAsFactor=FALSE)
    future::plan(future::multisession, workers = numCores)
    file.remove(list.files(path = settings$inputLoc, pattern="progress.txt", recursive = TRUE, full.names=TRUE))
    file.remove(list.files(path = settings$inputLoc, pattern="preservedCalib.csv", recursive = TRUE, full.names=TRUE))
    unlink(file.path(settings$inputLoc,"thread"),recursive=TRUE)


    if(fileToChange == "soil"){
        #targetFile <- settings$soilFile[2]
        sourceFilePath <- settings$soilFile[2]
    }
    else if (fileToChange == "epc"){
        #targetFile <- settings$epc[2]
        sourceFilePath <- settings$epc[2]
    }
    #   ____                _         _   _                        _     
    #  / ___|_ __ ___  __ _| |_ ___  | |_| |__  _ __ ___  __ _  __| |___ 
    # | |   | '__/ _ \/ _` | __/ _ \ | __| '_ \| '__/ _ \/ _` |/ _` / __|
    # | |___| | |  __/ (_| | ||  __/ | |_| | | | | |  __/ (_| | (_| \__ \
    #  \____|_|  \___|\__,_|\__\___|  \__|_| |_|_|  \___|\__,_|\__,_|___/
                                                                       
    

    copyToThreadDirs(thread_prefix, numcores = numCores, runDir = settings$inputLoc)

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
                               musoSingleThread(measuredData, parameters, startDate,
                                        sourceFile=sourceFilePath, 
                                        endDate, formatString,
                                        dataVar, outLoc,
                                        preTag, settings,
                                        outVars, iterations = threadCount[i],
                                        skipSpinup, plotName,
                                        modifyOriginal, likelihood, uncertainity,
                                        naVal, postProcString, constraints=constraints, threadNumber = i, fileToChange = fileToChange)
                      , error = function(e){
                           #browser()
                                            writeLines(as.character(e),"error.txt")
                                            writeLines(as.character(iterations),"progress.txt")
                                        })

                    #   musoSingleThread(measuredData, parameters, startDate,
                    #                    endDate, formatString,
                    #                    dataVar, outLoc,
                    #                    preTag, settings,
                    #                    outVars, iterations = threadCount[i],
                    #                     skipSpinup, plotName,
                    #                     modifyOriginal, likelihood, uncertainity,
                    #                     naVal, postProcString, i)
         })
    })

    # __        ___     _       _                                         
    # \ \      / / __ _| |_ ___| |__    _ __  _ __ ___   ___ ___  ___ ___ 
    #  \ \ /\ / / / _` | __/ __| '_ \  | '_ \| '__/ _ \ / __/ _ \/ __/ __|
    #   \ V  V / | (_| | || (__| | | | | |_) | | | (_) | (_|  __/\__ \__ \
    #    \_/\_/   \__,_|\__\___|_| |_| | .__/|_|  \___/ \___\___||___/___/
    #                                  |_|                                

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
    if(numCores==1){
        results <- res0
    } else {
        resultFilesSans0 <- grep("thread_1/", resultFiles, value=TRUE, invert=TRUE)
        # results <- do.call(rbind,lapply(resultFilesSans0, function(f){read.csv(f, stringsAsFactors=FALSE)}))
        resultsSans0 <- lapply(resultFilesSans0, function(f){read.csv(f, stringsAsFactors=FALSE, header=FALSE)})
        resultsSans0 <- do.call(rbind,resultsSans0)
        colnames(resultsSans0) <- colnames(res0)
        results <- (rbind(res0,resultsSans0))
    }

    switch(method,
           "GLUE"={
                print("Writing the results to the output directory...")
                write.csv(x=results, file=file.path(outputLoc,"calibResults.csv"), row.names=FALSE)
                print("Writing the parameters to the output directory...")
                write.csv(x=parameters, file=file.path(outputLoc,"sampledParameters.csv"), row.names=FALSE)
                musoGlue(results, parameters=parameters,settings=settings, w=w, lg=lg)
                liks <- results[,sprintf("%s_likelihood",names(likelihood))]    
                epcIndexes <- future::value(fut[[1]], stdout = FALSE, signal=FALSE)
                if(is.null(dim(liks)) || dim(liks)[2] == 1 ){
                    ml_place <- which.max(liks)
                } else {
                    ml_place <- which.max(as.matrix(liks) %*% as.matrix(w))
                }
                epcVals <- results[ml_place,1:length(epcIndexes)]
                if(fileToChange == "soil"){
                    epcPlace <- file.path(dirname(settings$inputFiles),settings$soil)[2]
                    outFile <- "maxLikelihood_soil.soi"
                } else {
                    epcPlace <- file.path(dirname(settings$inputFiles),settings$epc)[2]
                    outFile <- "maxLikelihood_epc.epc"
                }
                #epcPlace <- file.path(dirname(settings$inputFiles),settings$epc)[2]
                changemulline(filePaths= epcPlace, epcIndexes,
                              epcVals, src =epcPlace,# settings$epcInput[2],
                              outFiles = file.path(outputLoc, outFile))
                names(epcVals) <- epcIndexes
           },
           "agromo"={
                liks <- results[,sprintf("%s_likelihood",names(likelihood))]    
                epcIndexes <- future::value(fut[[1]], stdout = FALSE, signal=FALSE)
                epcVals <- results[which.max(liks),1:length(epcIndexes)]
                epcPlace <- file.path(dirname(settings$inputFiles),settings$epc)[2]
                changemulline(filePaths= epcPlace, epcIndexes,
                              epcVals, src =epcPlace,# settings$epcInput[2],
                              outFiles = file.path(outputLoc, "maxLikelihood_epc.epc"))
                names(epcVals) <- epcIndexes
                xdate <- as.Date(measuredData$date) 
                meanM <- measuredData[,sprintf("mean.%s", names(likelihood))]
                minsd <-  meanM - measuredData[,sprintf("sd.%s", names(likelihood)[1])]
                maxsd <- meanM + measuredData[,sprintf("sd.%s", names(likelihood)[1])]
                minM <- measuredData[,sprintf("min.%s", names(likelihood)[1])]
                maxM <- measuredData[,sprintf("max.%s", names(likelihood)[1])]
                plot(xdate, minM, type="l", xlab=NA, ylim=c(min(minM)*0.8, max(maxM)*1.1), ylab = names(likelihood)[1])
                lines(xdate, maxM)
                polygon(c(xdate,rev(xdate)),c(minM,rev(maxM)), col="gray",border=NA)
                lines(xdate, minsd)
                lines(xdate, maxsd)
                polygon(c(xdate,rev(xdate)),c(minsd,rev(maxsd)), col="gray30",border=NA)
                points(xdate,meanM)

                varIndex <- match(as.character(dataVar),settings$dailyVarCodes)
                apriori <- calibMuso(settings)
                modDates <- as.Date(row.names(apriori), format="%d.%m.%Y")
                lines(modDates, apriori[,varIndex],col="brown")
                calibrated <- calibMuso(settings, calibrationPar = as.numeric(names(epcVals)), parameters=epcVals)
                lines(modDates, calibrated[,varIndex],col="blue")

           },
           stop(sprintf("method: %s not found, please choose from {GLUE, agromo}. See more about this in the documentation of the function!", method))
    )
}

copyToThreadDirs <- function(prefix="thread", numcores=parallel::detectCores()-1, runDir="."){
    dir.create(file.path(runDir,prefix), showWarnings=TRUE)
    fileNames <- grep(".*thread$", list.files(runDir,full.names=TRUE), value=TRUE, invert=TRUE)
    invisible(sapply(1:numcores,function(corenum){
                threadDir <- file.path(runDir,prefix,paste0(prefix,"_",corenum),"")
                dir.create(threadDir, showWarnings=FALSE)
                file.copy(from=fileNames,to=threadDir, overwrite=FALSE, recursive=TRUE)
    }))
}

musoSingleThread <- function(measuredData, parameters = NULL, startDate = NULL,
                     sourceFile=NULL,
                     endDate = NULL, formatString = "%Y-%m-%d",
                     dataVar, outLoc = "./calib",
                     preTag = "cal-", settings =  setupMuso(),
                     outVars = NULL, iterations = 300,
                     skipSpinup = TRUE, plotName = "calib.jpg",
                     modifyOriginal=TRUE, likelihood, uncertainity = NULL, 
                     naVal = NULL, postProcString = NULL, constraints=NULL, threadNumber, fileToChange = fileToChange) {

    setwd(paste0(settings$inputLoc, "/thread/thread_", threadNumber))
    if(fileToChange == "soil"){
        targetFile <- settings$soilFile[2]
        sourceFilePath <- settings$soilFile[2]
    }
    else if (fileToChange == "epc"){
        targetFile <- settings$epc[2]
        sourceFilePath <- settings$epc[2]
    }
    iniFiles <- file.path(settings$iniInput) 
    # iniFiles <- list.files(pattern=".*ini")
    # if(length(iniFiles)==1){
    #     iniFiles <- rep(iniFiles, 2)
    # }
    settings <- setupMuso(iniInput = iniFiles)
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
    # browser()

    print("optiMuso is randomizing the epc parameters now...",quote = FALSE)
    if(iterations < 3000){
        randVals <- musoRand(parameters = parameters,constraints = constraints, iterations = 3000,sourceFile=sourceFile, fileType = fileToChange)
        randVals[[2]]<- randVals[[2]][sample(1:3000,iterations),] # TODO: last not random
    } else {
        randVals <- musoRand(parameters = parameters,constraints = constraints, iterations = iterations,sourceFile=sourceFile, fileType = fileToChange)
    }


    origEpc <- readValuesFromFile(targetFile,randVals[[1]])
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

    pretag <- file.path(outLoc,preTag)

     musoCodeToIndex  <- sapply(dataVar,function(musoCode){
           settings$dailyOutputTable[settings$dailyOutputTable$code == musoCode,"index"]
        })
    resultRange <- (numParameters + 1):(ncol(partialResult))
    ## Creating function for generating separate
    ## csv files for each run

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
    if(threadNumber == 1){
        origModellOut <- calibMuso(settings=settings, silent=TRUE, skipSpinup = skipSpinup, postProcString=postProcString, modifyOriginal=modifyOriginal)
        partialResult[,resultRange] <- calcLikelihoodsAndRMSE(dataVar=dataVar, 
                                                              mod=origModellOut,
                                                              mes=measuredData,
                                                              likelihoods=likelihood,
                                                               alignIndexes=alignIndexes,
                                                               musoCodeToIndex = musoCodeToIndex,uncert=uncert)
        write.csv(x=origModellOut, file=paste0(pretag, 1, ".csv"))
    write.csv(x=partialResult, file="preservedCalib.csv",row.names=FALSE)
    }
    print("Running the model with the random epc values...", quote = FALSE)

    # if(!is.null(postProcString)){
    #     colNumb <- length(settings$dailyVarCodes) + 1
    # }
    
    singleVarCalib <- FALSE
    if(is.null(dim(randValues))){
        singleVarCalib <- TRUE
    }    

    for(i in 2:(iterations+1)){
        
        if(!singleVarCalib){
            parameters <- randValues[(i-1),]
        } else {
            parameters <- randValues[i]
        }
        
        tmp <- tryCatch(calibMuso(settings = settings,
                                  parameters = parameters,
                                  silent= TRUE,
                                  skipSpinup = skipSpinup, modifyOriginal=modifyOriginal, postProcString = postProcString, fileToChange = fileToChange), error = function (e) NULL)
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

        partialResult[1:numParameters] <- parameters
        write.table(x=partialResult, file="preservedCalib.csv", append=TRUE, row.names=FALSE,
                    sep=",", col.names=FALSE)
        write.csv(x=tmp, file=paste0(pretag, (i+1),".csv"))
        writeLines(as.character(i-1),"progress.txt")
    }

    if(threadNumber == 1){
        return(randVals[[1]])
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

     # Ensure 'mes' is a data frame
     mes <- as.data.frame(mes)

     # Iterate through the *names* provided in the dataVar argument
     likelihoodRMSE_list <- sapply(names(dataVar), function(key){

        # --- Find Modelled Column ---
        modelColIndex <- NA # Default to invalid index

        # 1. Try using the pre-calculated index (derived from code in dataVar)
        #    Check if the key exists in the names derived from dataVar codes
        #    and if the corresponding index is valid for the 'mod' data frame
        if (key %in% names(musoCodeToIndex)) {
             idx_from_code <- musoCodeToIndex[[key]]
             if (length(idx_from_code) == 1 && !is.na(idx_from_code) &&
                 idx_from_code > 0 && idx_from_code <= ncol(mod)) {
                 modelColIndex <- idx_from_code
             }
        }

        # 2. If index from code wasn't found or valid, try matching column by name
        if (is.na(modelColIndex)) {
            idx_from_name <- match(key, colnames(mod))
            # Check if match found a valid index
            if (length(idx_from_name) == 1 && !is.na(idx_from_name) && idx_from_name > 0) {
                 modelColIndex <- idx_from_name
             
            }
        }

        # 3. Check if we successfully found a column index for the model output
        if (is.na(modelColIndex)) {
            warning(paste("Could not find model output column for key:", key,
                          "(tried code lookup and name matching). Skipping likelihood calculation for this variable."),
                    call. = FALSE) # Avoid printing the call stack for clarity
            # Return NA for both likelihood and RMSE for this variable
            return(c(likelihood = NA, rmse = NA))
        }

        # Extract modelled data using the determined index
        # Ensure we handle potential errors if alignIndexes$mod is out of bounds
        modelled <- tryCatch({
             mod[alignIndexes$mod, modelColIndex]
             }, error = function(e) {
                  warning(paste("Error accessing modelled data for key:", key, "at index", modelColIndex, "-", e$message), call. = FALSE)
                  rep(NA, length(alignIndexes$mod)) # Return NA vector of correct expected length
             })
        # End Find Modelled Column


        # Find Measured Column(s)
        # Search for columns in 'mes' ending with the key
        selected_indices <- grep(sprintf("%s$", key), colnames(mes))

        if (length(selected_indices) == 0) {
             warning(paste("Could not find measurement column ending with key:", key,
                           "in measuredData. Skipping likelihood calculation for this variable."),
                     call. = FALSE)
             return(c(likelihood = NA, rmse = NA))
        }

        # Select the actual column(s) based on index/indices
        # Handle case where multiple columns match (_mean, _sd) - prefer single exact/mean match
        measured_col_data <- mes[alignIndexes$meas, selected_indices, drop = FALSE] # Use drop=FALSE to keep data frame structure

        # Determine the primary measurement column for RMSE ('m') and the data for likelihood ('measured_for_like')
        # Prioritize exact match, then mean, then first match
        measured_for_like <- measured_col_data # By default, use all matched columns for likelihood func
        m_col_index <- NULL
        exact_match_idx <- which(colnames(measured_col_data) == key)
        mean_match_idx <- grep(sprintf("^mean\\.%s$|^%s_mean$", key, key), colnames(measured_col_data))

        if(length(exact_match_idx) == 1) {
            m_col_index <- exact_match_idx
        } else if (length(mean_match_idx) == 1) {
            m_col_index <- mean_match_idx
        } else {
             # If no exact or mean match, use the first selected column for 'm'
             m_col_index <- 1
             if(ncol(measured_col_data) > 1) {
                  warning(paste("Multiple measurement columns found for key:", key,
                                "- using '", colnames(measured_col_data)[m_col_index], "' for RMSE calculation."),
                          call. = FALSE)
             }
        }
         m <- measured_col_data[, m_col_index]
        # End Find Measured Column(s)


        # Alignment and NA Handling
        # Align modelled and measured data, removing rows where *either* is NA
        valid_indices <- !is.na(modelled) & !is.na(m)
        modelled_aligned <- modelled[valid_indices]
        m_aligned <- m[valid_indices]
        measured_for_like_aligned <- measured_for_like[valid_indices, , drop = FALSE] # Keep aligned subset

        # Check if any comparable data remains
        if (length(modelled_aligned) == 0) {
           warning(paste("No valid overlapping non-NA data points found for key:", key, "after alignment."),
                   call. = FALSE)
           return(c(likelihood = NA, rmse = NA))
        }
        # End Alignment and NA Handling


        # Calculate Likelihood and RMSE 
        # Get the appropriate likelihood function for this key
        currentLikelihoodFunc <- likelihoods[[key]]
        if (is.null(currentLikelihoodFunc) || !is.function(currentLikelihoodFunc)) {
             warning(paste("Likelihood function not found or invalid for key:", key), call. = FALSE)
             # Returning NA for now.
             likelihood_val <- NA
        } else {
             # Calculate likelihood 
             likelihood_val <- tryCatch({
                  currentLikelihoodFunc(modelled_aligned, m_aligned)
             }, error = function(e) {
                  warning(paste("Error calculating likelihood for key:", key, "-", e$message), call. = FALSE)
                  NA
             })
        }

        # Calculate RMSE using the primary measurement column ('m_aligned')
        rmse_val <- sqrt(mean((modelled_aligned - m_aligned)^2, na.rm = TRUE)) # na.rm is fallback

        res <- c(likelihood = likelihood_val, rmse = rmse_val)
        # End Calculate Likelihood and RMSE

        return(res) # Return named vector for this key

    }, simplify = FALSE) # Use simplify=FALSE initially to handle potential errors gracefully

    # Combine results into the final matrix/vector format expected
    # Handle cases where some variables failed (returned NA)
    final_likelihoods <- sapply(likelihoodRMSE_list, function(x) x['likelihood'])
    final_rmses <- sapply(likelihoodRMSE_list, function(x) x['rmse'])

    # Construct the final named vector/matrix as expected by the calling function
    likelihood_names <- sprintf("%s_likelihood", names(dataVar))
    rmse_names <- sprintf("%s_rmse", names(dataVar))
    final_results_vector <- c(final_likelihoods, final_rmses)
    names(final_results_vector) <- c(likelihood_names, rmse_names)

    # Return results in the format that musoSingleThread expects for partialResult[, resultRange]
    # Ensure the order matches: likelihoods first, then RMSEs
    return(final_results_vector)
}

agroLikelihood <- function(modVector,measured){
    mu <- measured[,grep("mean", colnames(measured))]
    stdev <- measured[,grep("^sd", colnames(measured))]
    ndata <- nrow(measured)
    sum(sapply(1:ndata, function(x){
                  dnorm(modVector, mu[x], stdev[x], log = TRUE)
               }), na.rm=TRUE)
}



maxLikelihoodAgromo <- function (results, imgPath, varName, ...) {
    
}



#' musoOptimCalib
#'
#' This function allows further post process for calibrateMuso results based on random forest surrogate model which will then be optimized.
#' Currenlty only DE (differential evolution) is available but in the future custom optimizing method could be used as an input
#' Note: For multiobjective calibration it won't work, its calibResult file looks a little different but will be supported in the future
#' 
#' @param calibList The calibration results created by the calibrationMuso, or a path to the csv file with the results. 
#' @param parameters THe parameters.csv used for setting the min and max values
#' @param numTrees Number of trees for the random forest
#' @param mtry The number of variables randomly sampled as candidates at each split.  
#' @param dataTrain The percent of the data to be randomly selected for training. 
#'                  Base is 0.8 so 80 percent of the data will be used for training.
#' @param method The method to be used for optimization. Currently only DEoptim is available.
#' @param maxIterations The maximum number of iterations for the optimization.
#' @param NP The population size for the DEoptim algorithm. Generally setting this 10 times larger than your parameter vector is necessary for proper results
#' @param saveAllNP saves all the population results as the calibration runs and writes it into a csv later
#' @param parallel option to perform the optimization method in a parallel way for better speed
#' @param cluster for better perfomance in case you call this function in a loop this option allows the use of already created clusters. If given the musoOptimCalib function won't create and destroy clusters at each call.
#' @param numCores Number of cores to be used if parallel is TRUE
#' @export
musoOptimCalib <- function(
                    outputLoc = "./",
                    calibList = "calibResults.csv", 
                    parameters = "parameters_soil.csv",
                    numTrees = 1000, 
                    mtry = 3, 
                    dataTrain = 0.8,
                    method = "DEoptim",
                    maxIterations = 50,
                    NP = 100,
                    saveAllNP = FALSE,
                    parallel = FALSE,
                    cluster = NULL,
                    numCores = parallel::detectCores()-1
                    ){

    # check whether the package given for the function for the optimization is installed
    if(!requireNamespace("DEoptim", quietly = TRUE)){
        stop("Currently the DEoptim package is required for this function to work and is not a strict requirement of the RBBGCMuso package. Please install it using install.packages('DEoptim') if you wish to use the musoOptimCalib function")
    }

    if (parallel && numCores < 1) {
        stop("numCores must be at least 1 when parallel = TRUE")
    }

    # try to be as robust about the editing of calibResults.
    calibData <- read.table(calibList, header=TRUE, sep=",", stringsAsFactors=FALSE) 
    parameters <- read.csv(parameters, stringsAsFactors=FALSE)
    # remove all columns ending with _rmse
    calibData <- calibData[,!grepl("_rmse$", colnames(calibData))]

    # getting the min and max values for the parameters used in the calibration (and their names) from the parameters.csv
    minValues <- parameters$min
    maxValues <- parameters$max
    paramNames <- parameters[,1] 

    # get the name of the likelihood column, ending with _likelihood
    likelihoodCol <- grep("_likelihood$", colnames(calibData), value=TRUE)
    # creating the formula string for the random forest model where we will use as.formula so it won't complain
    formula_string <- sprintf("%s ~ .", likelihoodCol)

    # creating filtered data 
    model_data <- calibData[, c(paramNames, likelihoodCol)]
    # randomly select the data for training
    dataTrainIndex <- sample(1:nrow(model_data), size = round(nrow(model_data) * dataTrain), replace = FALSE)
    dataTrained <- model_data[dataTrainIndex,]
    dataTesting <- model_data[-dataTrainIndex,]

    # random forest model
    randomForest <- ranger::ranger(
        formula = as.formula(formula_string),
        data = dataTrained,
        num.trees = numTrees,
        mtry = mtry
    )

    # plot the random forest result for the user for inspection
    plot(predict(randomForest,dataTesting)$predictions,dataTesting[,likelihoodCol],
    main = "Random Forest Predictions vs Observed Likelihood",
    xlab = "Predicted Likelihood",
    ylab = "Observed Likelihood",
    col = "blue",
    pch = 19)

    # add a line for the perfect prediction
    abline(a=0, b=1, col="red", lwd=2)

    correlation <- cor(predict(randomForest,dataTesting)$predictions, dataTesting[,likelihoodCol], use="complete.obs")
    rmse <- sqrt(mean((predict(randomForest,dataTesting)$predictions - dataTesting[,likelihoodCol])^2, na.rm=TRUE))
    cat("Random Forest Model Correlation and RMSE:\n")
    cat(sprintf("Correlation: %.8f, RMSE: %.8f\n", correlation, rmse))

    optimizationFunc <- function(x){
        m <- as.data.frame(setNames(as.list(x), paramNames))

        predictions <- predict(randomForest, data = m)$predictions
        return(-predictions) # return negative because DEoptim minimizes the function
    }

    # get the max likelihood value and the related parameters
    maxLikelihood <- max(calibData[,likelihoodCol], na.rm=TRUE)
    maxLikelihoodParams <- calibData[which.max(calibData[,likelihoodCol]), paramNames]

    # creating cluster for the parallel run so the DE optimization can properly do paralallelisation
    if (parallel) {
        if (is.null(cluster)) {
            # Create a new cluster if none provided
            cluster <- parallel::makeCluster(numCores, type = "SOCK")
            on.exit(parallel::stopCluster(cluster), add = TRUE)
            parallel::clusterExport(cluster, "randomForest", envir = environment())
            parallel::clusterEvalQ(cluster, library(ranger))
        } else {
            # Use provided cluster, export necessary objects
            parallel::clusterExport(cluster, "randomForest",envir = environment())
            parallel::clusterEvalQ(cluster, library(ranger))
        }
    }

    cat(sprintf("Starting optimization with %s method, %d iterations and population size of %d.\n\n", method, maxIterations, NP))
    cat(sprintf("For comparison: Max Likelihood from the calibration data: %.6f \n", maxLikelihood))
    cat(sprintf("Parameters for max likelihood: \n %s", paste(maxLikelihoodParams, collapse=", \n")))
    cat("\n\n")

    # optimization using DEoptim
    optimResult <- DEoptim::DEoptim(
        optimizationFunc,
        lower = minValues,
        upper = maxValues,
        control = list(
            itermax = maxIterations,
            NP = NP,
            trace = TRUE,
            storepopfrom = if(saveAllNP) 1 else itermax+1, # itermax + 1 will only save the best
            parallelType = if(parallel) "parallel" else "none",
            cluster = if(parallel) cluster else NULL
        )
    )

    # get the best parameters from the optimization result
    bestParams <- optimResult$optim$bestmem
    bestLikelihood <- -optimResult$optim$bestval
    bestParams <- as.data.frame(setNames(as.list(bestParams), paramNames))
    

    cat("Best Parameters:\n")
    print(bestParams)
    cat(sprintf("Optim Likelihood: %.8f \n", bestLikelihood))
    bestParams <- cbind(bestParams, likelihood = bestLikelihood)
    # set the likelihood column name to the same as in the calibData
    colnames(bestParams)[ncol(bestParams)] <- likelihoodCol

    # save the best parameters to a csv file
    # if optRanges.csv exists, read it and create the optimizedCalibParameters.csv including optRanges data
    if(file.exists(file.path(outputLoc,"optRanges.csv"))){
    cat("optRanges.csv found, creating optimizedCalibParameters.csv including optRanges data...\n")
        optRanges <- read.csv(file.path(outputLoc,"optRanges.csv"), row.names = 1, check.names = FALSE)
        optRanges$optimized <- NA
        #parName <- colnames(bestParams)
        optRanges[c(paramNames,likelihoodCol),"optimized"] <- as.numeric(bestParams[1,])

        # bring in the maxlikelihood params and value as well
        #optRanges$maxLikelihood <- NA
        #opt... or not
        write.csv(optRanges,file = file.path(outputLoc, "optimizedCalibParameters.csv"), row.names = TRUE)
        
        cat(sprintf("Optimized parameters saved to %s/optimizedCalibParameters.csv .\n", file.path(outputLoc)))
    }
    else{
    cat("optRanges.csv not found, saving only the optimized parameters to optimizedCalibParameters.csv...\n")
    write.csv(bestParams, file = file.path(outputLoc, "optimizedCalibParameters.csv"), row.names = FALSE)
    cat("Optimized parameters saved to optimizedCalibParameters.csv in the output directory.\n")

    }

    # saving the best members at each iteration to a csv file
    iterBest <- as.data.frame(optimResult$member$bestmemit)
    colnames(iterBest) <- paramNames
    iterBest$likelihood <- -optimResult$member$bestvalit
    write.csv(iterBest, file = file.path(outputLoc, "optimization_iterations_bestMembers.csv"), row.names = FALSE,quote = FALSE)


    ### SOME PLOTTING ###
    # combining all the populations if saveAllNP was true and creating plots for the parameter ranges
    AllPopulations <- optimResult$member$storepop
        if (is.null(AllPopulations) || length(AllPopulations) == 0) {
        AllPopulations <- list(optimResult$member$pop)
        }  else {
        AllPopulations <- c(AllPopulations, list(optimResult$member$pop))
        }
    names(AllPopulations) <- paste0("iter", seq_len(length(AllPopulations)))

    # Calculate relative ranges for each parameter across iterations
    rel_ranges <- t(sapply(AllPopulations, function(pop) {
        sapply(seq_along(paramNames), function(i) {
            (max(pop[,i]) - min(pop[,i])) / (maxValues[i] - minValues[i])
        })
    }))
    colnames(rel_ranges) <- paramNames
    # Prepare iterBest for plotting (exclude likelihood column)
    iter_best_plot <- iterBest[, paramNames, drop = FALSE]
    iter_best_plot$iteration <- seq_len(nrow(iter_best_plot))

    # Start PDF device for multi-page output
    pdf(file.path(outputLoc, "relative_ranges.pdf"), width = 8, height = 6)

    # 1. Relative ranges plot
    rel_ranges_df <- as.data.frame(rel_ranges)
    rel_ranges_df$iteration <- seq_len(nrow(rel_ranges_df))
    rel_ranges_df <- pivot_longer(rel_ranges_df, cols = -iteration, 
                                names_to = "parameter", values_to = "relative_range")

    p1 <- ggplot(rel_ranges_df, aes(x = iteration, y = relative_range, color = parameter)) +
        geom_line(linewidth = 1) +
        scale_y_continuous(limits = c(0, 1), name = "Relative Range (Range / Initial Range)") +
        scale_x_continuous(name = "Iteration") +
        scale_color_viridis_d(option = "viridis", name = "Parameter") +
        theme_minimal() +
        theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5)) +
        ggtitle("Parameter Range Evolution in DEoptim")
    print(p1)

    # 2. Per-parameter value vs. iteration plot
    iter_best_long <- pivot_longer(iter_best_plot, cols = -iteration, 
                                names_to = "parameter", values_to = "value")
    p2 <- ggplot(iter_best_long, aes(x = iteration, y = value, color = parameter)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        facet_wrap(~ parameter, scales = "free_y", ncol = 1) +
        scale_x_continuous(name = "Iteration", breaks = scales::pretty_breaks(n = 5)) +
        scale_y_continuous(name = "Parameter Value") +
        scale_color_viridis_d(option = "viridis", name = "Parameter") +
        theme_minimal() +
        theme(legend.position = "right",
            strip.text = element_text(size = 10),
            plot.title = element_text(hjust = 0.5)) +
        ggtitle("Best Parameter Values vs. Iteration") +
        guides(color = guide_legend(override.aes = list(size = 3)))
    print(p2)

    # 3. Histograms: first 50% vs. last 50% iterations
    half_point <- nrow(iter_best_plot) %/% 2
    iter_best_long$period <- ifelse(iter_best_long$iteration <= half_point, 
                                    paste0("First ", half_point, " Iterations"),
                                    paste0("Last ", nrow(iter_best_plot) - half_point, " Iterations"))
    p3 <- ggplot(iter_best_long, aes(x = value, fill = period)) +
        geom_histogram(aes(y = after_stat(count)), bins = 15, color = "black", alpha = 0.7, position = "dodge") +
        facet_wrap(~ parameter, scales = "free", ncol = 1) +
        scale_x_continuous(name = "Parameter Value") +
        scale_y_continuous(name = "Frequency", breaks = scales::pretty_breaks(n = 5)) +
        scale_fill_manual(values = c("coral", "skyblue"), name = "Period") +
        theme_minimal() +
        theme(legend.position = "right",
            strip.text = element_text(size = 10),
            plot.title = element_text(hjust = 0.5)) +
        ggtitle("Histograms: First 50% vs. Last 50% Iterations") +
        guides(fill = guide_legend(override.aes = list(alpha = 1)))
    print(p3)

    # Close PDF device
    dev.off()

    cat(sprintf("Relative ranges plot saved to %s/relative_ranges.pdf\n", outputLoc))


    # visualization of the optimization result, saving them as a pdf file
    pdf(file.path(outputLoc, "optimization_dotplots.pdf"))
    pari <- par(mfrow=c(1,2)) 

    top5points <- calibData[,likelihoodCol] > quantile(calibData[,likelihoodCol], 0.95, na.rm=TRUE)
    calibDataTop5 <- calibData[top5points,]
    
    # Calculate GLUE intervals from the top 5% of calibration data
    optRanges <- t(apply(calibDataTop5[, paramNames], 2, function(x) quantile(x, c(0.05, 0.5, 0.95), na.rm = TRUE)))


    for(i in seq_along(paramNames)){
        param <- paramNames[i]
        
        # General dotty plot
        plot(calibData[,param], calibData[,likelihoodCol], pch=19, cex=.1, ylab="likelihood",
             main = param, xlab="", 
             xlim = c(parameters$min[i], parameters$max[i]))
        # abline(v = optRanges[param, 1], col = "blue")
        # abline(v = optRanges[param, 2], col = "green")
        # abline(v = optRanges[param, 3], col = "red")
        abline(v = bestParams[1,param], col = "orange", lwd = 1.5)

        # Top 5% dotty plot
        plot(calibDataTop5[,param], calibDataTop5[,likelihoodCol], pch=19, cex=.1, ylab="likelihood",
             main = paste0(param," (top 5%)"), xlab="",
             xlim = c(parameters$min[i], parameters$max[i]))
        abline(v = optRanges[param, 1], col = "blue")
        abline(v = optRanges[param, 2], col = "green")
        abline(v = optRanges[param, 3], col = "red")
        abline(v = bestParams[1,param], col = "orange", lwd = 1.5)
        
        # Add optimum value text
        # text(x = par("usr")[1] + 0.05 * diff(par("usr")[1:2]), 
        #      y = par("usr")[4] - 0.05 * diff(par("usr")[3:4]), 
        #      labels = paste("Optimum:", format(bestParams[1,param], digits = 4)),
        #      adj = c(0, 1), col = "red")
    }

    par(pari)
    dev.off()
    cat(sprintf("Dot plots saved to %s/optimization_dotplots.pdf\n", outputLoc))

}



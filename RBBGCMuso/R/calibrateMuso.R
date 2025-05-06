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
                 # Optional: Print a message indicating name matching was used
                 # print(paste("Info: Found model column for '", key, "' by name match.", sep=""))
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
        # --- End Find Modelled Column ---


        # --- Find Measured Column(s) ---
        # Search for columns in 'mes' ending with the key
        selected_indices <- grep(sprintf("%s$", key), colnames(mes))

        if (length(selected_indices) == 0) {
             warning(paste("Could not find measurement column ending with key:", key,
                           "in measuredData. Skipping likelihood calculation for this variable."),
                     call. = FALSE)
             return(c(likelihood = NA, rmse = NA))
        }

        # Select the actual column(s) based on index/indices
        # Handle case where multiple columns match (e.g., _mean, _sd) - prefer single exact/mean match
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
        # --- End Find Measured Column(s) ---


        # --- Alignment and NA Handling ---
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
        # --- End Alignment and NA Handling ---


        # --- Calculate Likelihood and RMSE ---
        # Get the appropriate likelihood function for this key
        currentLikelihoodFunc <- likelihoods[[key]]
        if (is.null(currentLikelihoodFunc) || !is.function(currentLikelihoodFunc)) {
             warning(paste("Likelihood function not found or invalid for key:", key), call. = FALSE)
             # Decide default behavior: NA or default likelihood? Returning NA for now.
             likelihood_val <- NA
        } else {
             # Calculate likelihood - pass the potentially multi-column aligned measurement data
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
        # --- End Calculate Likelihood and RMSE ---

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

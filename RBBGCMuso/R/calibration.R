#' optiMuso
#'
#' This function calculates the -users specified- likelihood for random model input.
#'
#' @author Roland HOLLOS
#' @param measuredDataFile a
#' @param parameters b
#' @param sep c
#' @param startDate d
#' @param endDate e
#' @param formatString a
#' @param filterCol a
#' @param filterVal b
#' @param selVar c
#' @param outLoc c
#' @param pretag a
#' @param calPar a
#' @param skipSpinup a
#' @param iterations c
#' @param constrains d
#' @param likelihood d
#' @param settings e
#' @param leapYear b
#' @param plotName u
#' @importFrom ggplot2 ggplot aes_string geom_point ggsave
#' @importFrom magrittr '%>%'
#' @importFrom gridExtra grid.arrange
#' @export
optiMuso <- function(measuredData, parameters = NULL, startDate = NULL,
                     endDate = NULL, formatString = "%Y-%m-%d",
                     dataVar, outLoc = "./calib",
                     preTag = "cal-", settings =  setupMuso(),
                     outVars = NULL, iterations = 30,
                     skipSpinup = TRUE, plotName = "calib.jpg",
                     modifyOriginal=TRUE, likelihood, uncertainity = NULL,
                     naVal = NULL, postProcString = NULL, w=NULL, lg=FALSE, parallel = TRUE) {
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
        setTxtProgressBar(progBar,i)
    }
    
    musoGlue("preservedCalib.csv",w=w, lg = lg)

}




alignMuso <- function (settings,measuredData) {
        # Have to fix for other starting points also
        modelDates <- seq(from= as.Date(sprintf("%s-01-01",settings$startYear)), 
            by="days",
            to=as.Date(sprintf("%s-12-31",settings$startYear+settings$numYears-1)))
        modelDates <- grep("-02-29",modelDates,invert=TRUE, value=TRUE)

        measuredDates <- apply(measuredData,1,function(xrow){
                                   sprintf("%s-%s-%s",xrow[1],xrow[2],xrow[3])
        })

        modIndex <- match(as.Date(measuredDates), as.Date(modelDates))
        measIndex <- which(!is.na(modIndex))
        modIndex <- modIndex[!is.na(modIndex)]
        cbind.data.frame(model=modIndex,meas=measIndex)
} 

# calcLikelihoodsAndRMSE <- function(dataVar, mod, mes, likelihoods, alignIndexes, musoCodeToIndex, uncert){
#
#     likelihoodRMSE <- sapply(names(dataVar),function(key){
#                # browser()
#                modelled <- mod[alignIndexes$mod,musoCodeToIndex[key]]
#                measured <- mes[alignIndexes$meas,key]
#                modelled <- modelled[!is.na(measured)] 
#                # uncert   <-   uncert[!is.na(measured)]
#                measured <- measured[!is.na(measured)] 
#                res <- c(likelihoods[[key]](modelled, measured, uncert),
#                         sqrt(mean((modelled-measured)^2))
#                )
#                res
#         })
#     names(likelihoodRMSE) <- c(sprintf("%s_likelihood",dataVar), sprintf("%s_rmse",dataVar))
#
#     return(c(likelihoodRMSE[1,],likelihoodRMSE[2,]))
# }

#' musoGlue
#'
#' This function calculates the -users specified- likelihood for random model input.
#'
#' @author Roland HOLLOS
#' @param plotName u
#' @export
musoGlue <- function(presCalFile, w, delta = 0.17, settings=setupMuso(), parameters=read.csv("parameters.csv",
                                                                        stringsAsFactors=FALSE), lg=FALSE){
    if(is.data.frame(presCalFile)){
        preservedCalib <- presCalFile
    } else {
        preservedCalib <- read.csv(presCalFile)
    }
    paramIndex <- parameters[(match(colnames(preservedCalib),parameters[,1])),2]
    paramIndex <- paramIndex[!is.na(paramIndex)]
    paramIndex <- c(paramIndex,
                        as.numeric(gsub("X","",
                                          grep("X[0-9]{1,}",
                                               colnames(preservedCalib),value=TRUE))))
    preservedCalib <- preservedCalib[-1,] #original

    likeIndexes <- grep("likelihood",colnames(preservedCalib))
    if(!is.null(w)){
        forCombine<- sapply(names(w),function(n){
                                grep(sprintf("%s_likelihood",n),colnames(preservedCalib))
        })
        preservedCalib[["combined"]] <- apply(as.data.frame(Map(function(x,y){
                                                           toNormalize  <- preservedCalib[,y]
                                                           toNormalize <- toNormalize / sqrt(sum(x^2))
                                                           toNormalize * x

                                            },w,forCombine)), 1, sum)
    } else {
        preservedCalib[["combined"]] <- preservedCalib[,grep("likelihood",colnames(preservedCalib),value=TRUE)]
    }

    parameterIndexes <- 1:(min(likeIndexes)-1)
    preservedCalib <- preservedCalib[!is.na(preservedCalib$combined),]
    unfilteredLikelihood <-  preservedCalib$combined
    top5points <- preservedCalib$combined>quantile(preservedCalib$combined,0.95)
    preservedCalibtop5 <- tryCatch(preservedCalib[top5points,], error=function(e){
       warning("Too few simulation to calculate the top five percent, please increase the iteration number")
                            preservedCalib
    })
    optRanges <-t(apply(preservedCalibtop5,2,function(x) quantile(x,c(0.05,0.5,0.95))))  
    pdf("dotplot.pdf")
    if(lg){
        plot(Reduce(min, -(unfilteredLikelihood), accumulate=TRUE),type="l", ylab="-log(likelihood)",xlab="iterations")
    } else {
        plot(Reduce(min, -log(unfilteredLikelihood), accumulate=TRUE),type="l", ylab="-log(likelihood)",xlab="iterations")
    }
    pari <- par(mfrow=c(1,2)) 
    for(i in seq_along(colnames(preservedCalib)[parameterIndexes])){
        plot(preservedCalib[,i],preservedCalib[,"combined"],pch=19,cex=.1, ylab="likelihood",
             main = colnames(preservedCalib)[i], xlab="")
        plot(preservedCalibtop5[,i],preservedCalibtop5[,"combined"],pch=19,cex=.1, ylab="likelihood",
             main = paste0(colnames(preservedCalibtop5)[i]," (behav.)"), xlab="")
        abline(v=optRanges[i,1],col="blue")
        abline(v=optRanges[i,2],col="green")
        abline(v=optRanges[i,3],col="red")
        
    }

    par(pari)
    dev.off()
    maxParValues <- unlist(preservedCalibtop5[which.max(preservedCalibtop5$combined),])[1:length(paramIndex)]
    maxParIndexes <- paramIndex
    maxLikelihoodParameters <- data.frame(parameter_index=maxParIndexes,parameter_value=maxParValues)
    write.csv(cbind.data.frame(parameters=maxParIndexes, calibrationPar=maxParValues),
              "maxlikelihood_parameters.csv")
    cat("\n\n- A file containing the parameters with the maximum likelihood (maxlikelihood_parameters.csv) has been created.\n")
    write.csv(optRanges,"optRanges.csv")
    cat("- GLUE interval values have been written into optRanges.csv\n")
    # browser()
    # There are some serious problems with this implementation. The uncertainity bouns are not for the parameters, but for the output values. The median is pointwise median for all simulation.
    # And the 95 and 5 percentile also.
    # dataVec <- preservedCalibtop5$combined
    # closestToMedian <- function (dataVec) {
    #     match(sort(dataVec)[min(which(sort(dataVec)>=median(dataVec)))], dataVec)
    # }
    #
    # while(is.null(optimalEpc)){
    #     match(quantile(preservedCalibtop5$combined,0.5), preservedCalibtop5$combined)
    #     optInterval <-t(apply(preservedCalibtop5,2,function(x) quantile(x,c(0.5-delta,0.5+delta))))  
    #     optParamRange <- cbind.data.frame(rownames(optInterval)[parameterIndexes],as.numeric(paramIndex),optInterval[parameterIndexes,])
    #     optimalEpc <- tryCatch(musoRand(optParamRange,iterations = 2), error=function(e){NULL})
    #     delta <- delta*1.05
    #     if(delta > 0.5){
    #         delta <- 0.5
    #     }
    #     if((delta == 0.5) && is.null(optimalEpc)){
    #         stop("cannot find optimal value in the given range")
    #     }
    # }
    # print("getOptim")
    # optimalEpc[[2]] <- optimalEpc[[2]][1,]
    # write.csv(as.data.frame(optimalEpc),"epcOptim.csv")
    # print(head(optRanges,n=-2))
    # calibMuso(calibrationPar=optimalEpc[[1]],parameters=optimalEpc[[2]])
    # file.copy(settings$epcInput[2],"epcOptim.epc")
}

generateOptEpc <- function(optRanges,delta, maxLikelihood=FALSE){
    if(missing(delta)){

    }
    
}

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
                     leapYearHandling = TRUE,
                     dataVar, outLoc = "./calib",
                     preTag = "cal-",
                     settings =  NULL,
                     outVars = NULL,
                     iterations = 30,
                     skipSpinup = TRUE,
                     constrains = NULL,
                     plotName = "calib.jpg",
                     modifyOriginal=TRUE,
		     likelihood = function(x, y){
			 exp(-sqrt(mean((x-y)^2)))
		     },
                     continious,
		     modelVar = 3009,
             naVal = NULL,
                     postProcString = NULL)
{
    mdata <- measuredData
    dataCol <- grep(dataVar, colnames(measuredData))

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

    if(is.null(settings)){
        settings <- setupMuso()
    }

    parameterNames <- parameters[,1]
    pretag <- file.path(outLoc,preTag)
    npar <- length(settings$calibrationPar)

    ##reading the original epc file at the specified
    ## row numbers
    print("optiMuso is randomizing the epc parameters now...",quote = FALSE)
    if(iterations < 3000){
        randVals <- musoRand(parameters = parameters,constrains = constrains, iterations = 3000)
        randVals[[2]]<- randVals[[2]][sample(1:3000,iterations),]
    } else {
        randVals <- musoRand(parameters = parameters,constrains = constrains, iterations = iterations)
    }

    origEpc <- readValuesFromFile(settings$epc[2],parameters[,2])

    ## Prepare the preservedCalib matrix for the faster
    ##  run.

    pretag <- file.path(outLoc,preTag)

    ## Creating function for generating separate
    ## csv files for each run

    progBar <- txtProgressBar(1,iterations,style=3)
    colNumb <- which(settings$dailyVarCodes == modelVar)
    settings$iniInput[2] %>%
        (function(x) paste0(dirname(x),"/",tools::file_path_sans_ext(basename(x)),"-tmp.",tools::file_ext(x))) %>%
        unlink
    randValues <- randVals[[2]]
    settings$calibrationPar <- randVals[[1]]
    if(!is.null(naVal)){
        measuredData <- as.data.frame(measuredData)
        measuredData[measuredData == naVal] <- NA
    }
    list2env(alignData(measuredData,dataCol = dataCol,modellSettings = settings,startDate = startDate,endDate = endDate,leapYear = leapYearHandling, continious = continious),envir=environment())
    ## modIndex and measuredData are created.  


    modellOut <- numeric(iterations + 1) # single variable solution
    rmse <- numeric(iterations + 1)
    origModellOut <- calibMuso(settings=settings,silent=TRUE, skipSpinup = skipSpinup,postProcString=postProcString, modifyOriginal=modifyOriginal)


    write.csv(x=origModellOut, file=paste0(pretag,1,".csv"))
    modellOut[1] <- likelihood(measuredData,origModellOut[modIndex,colNumb])
    rmse[1] <- sqrt(mean((measuredData-origModellOut[modIndex,colNumb])^2))
    print("Running the model with the random epc values...", quote = FALSE)

    if(!is.null(postProcString)){
        colNumb <- length(settings$dailyVarCodes) + 1
    }
    
    partialTemplate <- matrix(ncol=length(randVals[[1]])+2)
    colN <- randVals[[1]]
    colN[match(parameters[,2],randVals[[1]])] <- parameters[,1]

    colnames(partialTemplate) <- c(colN, "rmse","likelihood")
    partialTemplate[1:((length(randVals[[1]]))+2)] <- c(readValuesFromFile(settings$epc[2],randVals[[1]]),modellOut[1], rmse[1])
    write.csv(x=partialTemplate, file="preservedCalib.csv",row.names=FALSE)
    for(i in 2:(iterations+1)){
        tmp <- tryCatch(calibMuso(settings = settings,
                                  parameters = randValues[(i-1),],
                                  silent= TRUE,
                                  skipSpinup = skipSpinup, modifyOriginal=modifyOriginal, postProcString = postProcString)[modIndex,colNumb], error = function (e) NULL        )
        if(is.null(tmp)){
           tmp <- rmse[i] <- modellOut[i] <- NA
        } else {
            modellOut[i]<- likelihood(measuredData,tmp)
            rmse[i] <- sqrt(mean((measuredData-tmp)^2))
        }

        partialTemplate[1:(length(randVals[[1]])+2)] <- c(randValues[(i-1),], rmse[i], modellOut[i])
        write.table(x=partialTemplate, file="preservedCalib.csv",append=TRUE,row.names=FALSE,sep=",",col.names=FALSE)
        write.csv(x=tmp, file=paste0(pretag, (i+1),".csv"))
        setTxtProgressBar(progBar,i)
    }

    # paramLines <- parameters[,2]
    # paramLines <- order(paramLines)
    # randInd <- randVals[[1]][(randVals[[1]] %in% parameters[,2])]
    # randInd <- order(randInd)
    #
    #
    #
    # epcStrip <- rbind(origEpc[order(parameters[,2])],
    #                   randValues[,randVals[[1]] %in% parameters[,2]][,randInd])
    #
    #
    # preservedCalib <- cbind(epcStrip,rmse,
    #                         modellOut)
    # columNames <-  c(parameterNames[paramLines],"rmse", "likelihood")
    # colnames(preservedCalib) <- columNames
    # write.csv(preservedCalib,"preservedCalib.csv")

    preservedCalib<- read.csv("preservedCalib.csv")
    p<-list()
    preservedCalib <- preservedCalib[-1,]
    preservedCalib <- preservedCalib[!is.na(preservedCalib$likelihood),]
    dontInclude <-c((ncol(preservedCalib)-1),ncol(preservedCalib))

    # for(i in seq_along(colnames(preservedCalib)[-dontInclude])){
    #     p[[i]] <- ggplot(as.data.frame(preservedCalib),aes_string(colnames(preservedCalib)[i],"likelihood")) +
    #         geom_point(shape='.',size=1,alpha=0.8)
    # }
    unfilteredLikelihood <-  preservedCalib$likelihood
    preservedCalib <- preservedCalib[preservedCalib$likelihood>quantile(preservedCalib$likelihood,0.95),]
    optRanges <-t(apply(preservedCalib,2,function(x) quantile(x,c(0.05,0.5,0.95))))  

pdf("dotplot.pdf")
    plot(Reduce(min, -log(unfilteredLikelihood), accumulate=TRUE),type="l", ylab="-log(likelihood)",xlab="iterations")
    for(i in seq_along(colnames(preservedCalib)[-dontInclude])){
        plot(preservedCalib[,i],preservedCalib[,"likelihood"],pch=19,cex=.1, ylab="likelihood",
             main = colnames(preservedCalib)[i], xlab="")
        abline(v=optRanges[i,1],col="blue")
        abline(v=optRanges[i,2],col="green")
        abline(v=optRanges[i,3],col="red")
        
    }
dev.off()

    # ggsave(plotName,grid.arrange(grobs = p, ncol = floor(sqrt(ncol(preservedCalib)-1))), dpi = 1000)
    # maxLikelihoodPlace  <- which(preservedCalib[,"likelihood"]==max(preservedCalib[,"likelihood"],na.rm = TRUE))
    # resPlot <- plotMusoWithData(mdata = measuredData, startDate = startDate, endDate = endDate,
    #                             dataVar = dataVar, modelVar = modelVar, settings = settings, continious = continious) +
    #     plotMuso(settings = settings, parameters = randValues[maxLikelihoodPlace,],
    #              postProcString = postProcString, skipSpinup = FALSE, variable = colNumb, layerPlot = TRUE, colour = "green")
    #
    # print(resPlot)
    # tempEpc <- paste0(tools::file_path_sans_ext(basename(settings$epcInput[2])),"-tmp.",tools::file_ext(settings$epcInput[2]))
    # file.rename(tempEpc, "optimizedEpc.epc")
    # return(preservedCalib[maxLikelihoodPlace,])
    write.csv(optRanges,"optRanges.csv")
    # is.num <- sapply(head(optRanges,-2), is.numeric)
    # optRanges[is.num] <- lapply(optRanges[is.num], round, 4)
    return(head(optRanges,n=-2))
}





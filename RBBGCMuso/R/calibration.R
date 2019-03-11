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
optiMuso <- function(measuredData, parameters = NULL, startDate,
                     endDate, formatString = "%Y-%m-%d",
                     leapYearHandling = TRUE,
                     dataVar, outLoc = "./calib",
                     preTag = "cal-",
                     settings =  NULL,
                     outVars = NULL,
                     iterations = 30,
                     skipSpinup = TRUE,
                     constrains = NULL,
                     plotName = "calib.jpg",
		     likelihood = function(x, y){
			 exp(-sqrt(mean((x-y)^2)))
		     },
                     continious,
		     modelVar = 3009,
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
    list2env(alignData(measuredData,dataCol = dataCol,modellSettings = settings,startDate = startDate,endDate = endDate,leapYear = leapYearHandling, continious = continious),envir=environment())
    ## modIndex and measuredData are created.  

    modellOut <- numeric(iterations + 1) # single variable solution
    rmse <- numeric(iterations + 1)
    origModellOut <- calibMuso(settings=settings,silent=TRUE, skipSpinup = skipSpinup)


    write.csv(x=origModellOut, file=paste0(pretag,1,".csv"))
    modellOut[1] <- likelihood(measuredData,origModellOut[modIndex,colNumb])
    print("Running the model with the random epc values...", quote = FALSE)

    if(!is.null(postProcString)){
        colNumb <- length(settings$dailyVarCodes) + 1
    }

    for(i in 2:(iterations+1)){
        tmp <- tryCatch(calibMuso(settings = settings,
                                  parameters = randValues[(i-1),],
                                  silent= TRUE,
                                  skipSpinup = skipSpinup, postProcString = postProcString)[modIndex,colNumb], error = function (e) NA)

        modellOut[i]<- likelihood(measuredData,tmp)
        rmse[i] <- sqrt(mean((measuredData-tmp)^2))
        write.csv(x=tmp, file=paste0(pretag,(i+1),".csv"))
        setTxtProgressBar(progBar,i)
    }
    paramLines <- parameters[,2]
    paramLines <- order(paramLines)
    randInd <- randVals[[1]][(randVals[[1]] %in% parameters[,2])]
    randInd <- order(randInd)



    epcStrip <- rbind(origEpc[order(parameters[,2])],
                      randValues[,randVals[[1]] %in% parameters[,2]][,randInd])


    preservedCalib <- cbind(epcStrip,rmse,
                            modellOut)
    columNames <-  c(parameterNames[paramLines],"rmse", "likelihood")
    colnames(preservedCalib) <- columNames
    write.csv(preservedCalib,"preservedCalib.csv")
    p<-list()
    preservedCalib <- preservedCalib[-1,]
    dontInclude <-c((ncol(preservedCalib)-1),ncol(preservedCalib))
    for(i in seq_along(colnames(preservedCalib)[-dontInclude])){
        p[[i]] <- ggplot(as.data.frame(preservedCalib),aes_string(colnames(preservedCalib)[i],"likelihood")) +
            geom_point(shape='.',size=1,alpha=0.8)
    }

    ggsave(plotName,grid.arrange(grobs = p, ncol = floor(sqrt(ncol(preservedCalib)-1))),dpi = 300)
    maxLikelihoodPlace  <- which(preservedCalib[,"likelihood"]==max(preservedCalib[,"likelihood"],na.rm = TRUE))
    resPlot <- plotMusoWithData(mdata = mdata, startDate = startDate, endDate = endDate,
                                dataVar = dataVar, modelVar = modelVar, settings = settings, continious = continious) +
        plotMuso(settings = settings, parameters = randValues[maxLikelihoodPlace,],
                 postProcString = postProcString, skipSpinup = FALSE, variable = colNumb, layerPlot = TRUE, colour = "green")

    print(resPlot)
    tempEpc <- paste0(tools::file_path_sans_ext(basename(settings$epcInput[2])),"-tmp.",tools::file_ext(settings$epcInput[2]))
    file.rename(tempEpc, "optimizedEpc.epc")
    return(preservedCalib[maxLikelihoodPlace,])
}



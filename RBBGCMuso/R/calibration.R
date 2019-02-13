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
#' @importFrom ggplot ggplot aes_string geom_point ggsave
#' @importFrom magrittr '%>%'
#' @importFrom gridExtra grid.arrange
#' @export 
optiMuso <- function(measuredDataFile, parameters = NULL,
                     sep = ",", startDate,
                     endDate, formatString = "%Y-%m-%d",
                     naString = NULL, leapYear = TRUE,
                     filterCol = NULL, filterVal = 1,
                     selVar, outLoc = "./calib",
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
		     calPar = 3009)
{
 measuredData <- readMeasuredMuso(inFile = measuredDataFile, sep = sep, selVar = selVar,filterCol = filterCol, filterVal = filterVal)

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
 colNumb <- which(settings$dailyVarCodes == calPar)
 settings$iniInput[2] %>%
     (function(x) paste0(dirname(x),"/",tools::file_path_sans_ext(basename(x)),"-tmp.",tools::file_ext(x))) %>%
     unlink
 randValues <- randVals[[2]]
 settings$calibrationPar <- randVals[[1]]
 list2env(alignData(measuredData,dataCol = 8,modellSettings = settings,startDate = startDate,endDate = endDate,leapYear = FALSE),envir=environment())
 
 modellOut <- numeric(iterations + 1) # single variable solution
 origModellOut <- calibMuso(settings=settings,silent=TRUE)
 write.csv(x=origModellOut, file=paste0(pretag,1,".csv"))
 modellOut[1] <- likelihood(measuredData,origModellOut[modIndex,colNumb])
 for(i in 2:(iterations+1)){
     tmp <- tryCatch(calibMuso(settings = settings,
                               parameters = randValues[(i-1),],
                               silent= TRUE,
                               skipSpinup = skipSpinup)[modIndex,colNumb], error = function (e) NA)
     
     modellOut[i]<- likelihood(measuredData,tmp)
     write.csv(x=tmp, file=paste0(pretag,(i+1),".csv"))
     setTxtProgressBar(progBar,i)
 }
 modellOut
 paramLines <- parameters[,2]
 paramLines <- order(paramLines)
 randInd <- randVals[[1]][(randVals[[1]] %in% parameters[,2])]
 randInd <- order(randInd)
 
        

 epcStrip <- rbind(origEpc[order(parameters[,2])],
                   randValues[,randVals[[1]] %in% parameters[,2]][,randInd])
 
 
 preservedCalib <- cbind(epcStrip,
                         modellOut)
 colnames(preservedCalib) <- c(parameterNames[paramLines], "likelihood")
 p<-list()
 
 for(i in seq_along(colnames(preservedCalib)[-ncol(preservedCalib)])){
     p[[i]] <- ggplot(as.data.frame(preservedCalib),aes_string(colnames(preservedCalib)[i],"likelihood"))+geom_point(size=0.9)
 }

 ggsave(plotName,grid.arrange(grobs = p, ncol = floor(sqrt(ncol(preservedCalib)-1))),dpi = 600)
 
 return(preservedCalib[preservedCalib[,"likelihood"]==max(preservedCalib[,"likelihood"]),])
}



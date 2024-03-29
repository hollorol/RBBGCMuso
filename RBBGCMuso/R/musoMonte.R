#' musoMonte
#'
#' This function executes the Monte Carlo experiment with Biome-BGCMuSo (musoRand is called by this function). It samples the selected  model parameters within user defined ranges from conditional multivariate uniform distribution, and then runs the model for each run.
#' @author Roland HOLLOS
#' @param settings A list of environmental variables for the Monte Carlo experiment. These settings are generated by the setupMuso function. By default the settings parameter is generated automatically. 
#' @param parameters This is a dataframe (heterogeneous data-matrix), where the first column is the name of the parameter, the second is a numeric vector of the rownumbers of the given variable in the input EPC file, and the last two columns describe the minimum and the maximum of the parameter (i.e. the parameter ranges), defining the interval for the randomization.
#' @param calibrationPar You might want to change some parameters in your EPC file before you run the modell. You have to select the appropirate model parameters here. You can refer to the parameters by the number of the line in the EPC file where the variables are defined. The indexing of the lines starts at 1, and each line matters (like in any simple text file). You should use a vector for this selection like c(1,5,8)
#' @param inputDir The location of the input directory for the Biome-BGCMuSo model. This directory must contain a viable pack of all input files and the model executable file.  
#' @param iterations Number of the Monte Carlo simulations.
#' @param preTag This defines the name of the output files. This tag will be re-used so that the results will be like preTag-1.csv, preTag-2csv...
#' @param outputType  This parameter can be "oneCsv", "moreCsv", and "netCDF". If "oneCsv" is chosen the function creates one large csv file for all of the runs. If "moreCsv" is chosen, every model output goes to separate files. If netCDF is selected the output will be stored in a netCDF file. The default value of the outputTypes is "moreCsv". Note that netCDF is not implemented yet. 
#' @param fun If you select a variable from the possible outputs (by using the varIndex parameter), you have to provide a function which maps to a subset of real numbers. The most frequent possibilities are: mean, min, max, var, but you can define any function for your needs.
#' @param varIndex This parameter specifies which parameter will be used for the Monte Carlo experiment from the output list of Biome-BGCMuSo (defined by the INI file). You can extract this information from the INI files. At the output parameter specifications, the parameter order will determine this number. For example, if you have set these output parameters: 412, 874, 926, 888, and you want to use 926 for the experiment, you should specify varIndex as 3. 
#' @param debugging If you set this parameter, you can save every logfile, and RBBGCMuso will select those which contains errors. This is useful to study why the model crashes with a given parameter set. 
#' @param keepEpc If you set keepEpc as TRUE, it will save every selected EPC file, and move the wrong ones into the WRONGEPC directory.
#' @importFrom magrittr '%>%'
#' @export

musoMonte <- function(settings=NULL,
                     parameters=NULL,
                     sourceFile=NULL,
                     inputDir = "./",
                     outLoc = "./calib",
                     iterations = 10,
                     preTag = "mont-",
                     outputType = "moreCsv",
                     fun=mean,
                     varIndex = 1,
                     outVars = NULL,
                     silent = TRUE,
                     skipSpinup = TRUE,
                     debugging = FALSE,
                     keepEpc = FALSE,
                     constrains = NULL,
                     skipZero = TRUE,
                     postProcString=NULL,
                     modifyOut=TRUE,
                     ...){


    readValuesFromEpc  <- function(epc, linums){
        epcFile <- readLines(epc)
        rows <- numeric(2)
        values <- sapply(linums, function(x){
            rows[1] <- as.integer(x)
            rows[2] <- as.integer(round(100*x)) %% 10 + 1 
            epcFile <- readLines(epc)
            selRow <- unlist(strsplit(epcFile[rows[1]], split= "[\t ]"))
            selRow <- selRow[selRow!=""]
            return(as.numeric(selRow[rows[2]]))
            
        })
        
        return(values)
    } 
    
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
    
    

    if(is.null(outVars)){
        numVars <- length(settings$outputVars[[1]])
        outVarNames <- settings$outputVars[[1]]
    } else {
        numVars <- length(outVars)
        outVarNames <- sapply(outVars, musoMapping)
    }
    
    if(!is.null(postProcString)){
        outVarNames <- c(outVarNames,gsub("\\s","",unlist(strsplit(procString,"<-"))[1]))
    }

    parameterNames <- gsub("([\\s]|\\-epc)","",parameters[,1],perl=TRUE)
    # settings$calibrationPar <- A[,1] #:LATER:
    pretag <- file.path(outLoc,preTag)
    npar <- length(settings$calibrationPar)
    
    ##reading the original epc file at the specified
    ## row numbers
    if(iterations < 3000){
        randVals <- musoRand(parameters = parameters,fileType="epc", iterations = 3000,sourceFile=sourceFile)
        randVals[[2]]<- randVals[[2]][sample(1:3000,iterations),]
    } else {
        randVals <- musoRand(parameters = parameters,fileType="epc", iterations = iterations)
    }
    
    origEpc <- readValuesFromEpc(settings$epc[2],parameters[,2])
    
    ## Prepare the preservedEpc matrix for the faster
    ##  run.

    pretag <- file.path(outLoc,preTag)
    
    ## Creating function for generating separate
    ## csv files for each run

    progBar <- txtProgressBar(1,iterations,style=3)

    
    moreCsv <- function(){
        settings$iniInput[2] %>%
            (function(x) paste0(dirname(x),"/",tools::file_path_sans_ext(basename(x)),"-tmp.",tools::file_ext(x))) %>%
            unlink
        randValues <- randVals[[2]]
        settings$calibrationPar <- randVals[[1]]
        ## randValues <- randValues[,randVals[[1]] %in% parameters[,2]][,rank(parameters[,2])]
        modellOut <- matrix(ncol = numVars, nrow = iterations + 1)


        origModellOut <- calibMuso(settings=settings,silent=TRUE)
        write.csv(x=origModellOut, file=paste0(pretag,1,".csv"))

        if(!is.list(fun)){
            funct <- rep(list(fun), numVars)
        }
            
        tmp2 <- numeric(numVars)
# browser()
        for(j in 1:numVars){
            tmp2[j]<-funct[[j]](origModellOut[,j])
        }
        modellOut[1,]<- tmp2
        
        for(i in 2:(iterations+1)){
            tmp <- tryCatch(calibMuso(settings = settings,
                             parameters = randValues[(i-1),],
                             silent= TRUE,
                             skipSpinup = skipSpinup,
                             keepEpc = keepEpc,
                             debugging = debugging,
                             modifyOriginal = modifyOut,
                             outVars = outVars,postProcString=postProcString), error = function (e) NA)
            
            if(length(dim(tmp))>=1){
                for(j in 1:numVars){
                    tmp2[j]<-funct[[j]](tmp[,j])
                }
                if(skipZero){
                    if(tmp2[j]==0){
                        tmp2[j] <- NA
                    }
                }
            } else {
                for(j in 1:numVars){
                    tmp2[j]<-NA
                }
            }

            
            
            modellOut[i,]<- tmp2
            write.csv(x=tmp, file=paste0(pretag,(i+1),".csv"))
            setTxtProgressBar(progBar,i)
        }

        paramLines <- parameters[,2]
        paramLines <- order(paramLines)
        randInd <- randVals[[1]][(randVals[[1]] %in% parameters[,2])]
        randInd <- order(randInd)
    

       
        # browser()
        epcStrip <- rbind(origEpc[order(parameters[,2])],
                          randValues[,randVals[[1]] %in% parameters[,2]][,randInd])
        
        
            preservedEpc <- cbind(epcStrip,
                                  modellOut)
        colnames(preservedEpc) <- c(parameterNames[paramLines], sapply(outVarNames, function (x) paste0("mod.", x)))
        return(preservedEpc)
    }
    
    ## Creating function for generating one
    ## csv files for each run
    
    oneCsv <- function () {
      #  stop("This function is not implemented yet")
         settings$iniInput[2] %>%
            (function(x) paste0(dirname(x),"/",tools::file_path_sans_ext(basename(x)),"-tmp.",tools::file_ext(x))) %>%
            unlink
        randValues <- randVals[[2]]
        settings$calibrationPar <- randVals[[1]]
        ## randValues <- randValues[,randVals[[1]] %in% parameters[,2]][,rank(parameters[,2])]
        modellOut <- matrix(ncol = numVars, nrow = iterations + 1)

        origModellOut <- calibMuso(settings=settings,silent=TRUE)
        write.csv(x=origModellOut, file=paste0(pretag,".csv"))

        if(!is.list(fun)){
            funct <- rep(list(fun), numVars)
        }
            
        tmp2 <- numeric(numVars)

        for(j in 1:numVars){
            tmp2[j]<-funct[[j]](origModellOut[,j])
        }
        modellOut[1,]<- tmp2
        
        for(i in 2:(iterations+1)){
            tmp <- tryCatch(calibMuso(settings = settings,
                             parameters = randValues[(i-1),],
                             silent= TRUE,
                             skipSpinup = skipSpinup,
                             keepEpc = keepEpc,
                             debugging = debugging,
                             outVars = outVars), error = function (e) NA)
            
            if(!is.na(tmp)){
                for(j in 1:numVars){
                    tmp2[j]<-funct[[j]](tmp[,j])
                }
            } else {
                for(j in 1:numVars){
                    tmp2[j]<-rep(NA,length(settings$outputVars[[1]]))
                }
            }

            
            
            modellOut[i,]<- tmp2
            write.table(x=tmp, file=paste0(pretag,".csv"), append = TRUE,col.names = FALSE, sep = ",")
            setTxtProgressBar(progBar,i)
        }

        paramLines <- parameters[,2]
        paramLines <- order(paramLines)
        randInd <- randVals[[1]][(randVals[[1]] %in% parameters[,2])]
        randInd <- order(randInd)
        
       
        epcStrip <- rbind(origEpc[order(parameters[,2])],
                          randValues[,randVals[[1]] %in% parameters[,2]][,randInd])
        
        
            preservedEpc <- cbind(epcStrip,
                                  modellOut)
        colnames(preservedEpc) <- c(parameterNames[paramLines], sapply(outVarNames, function (x) paste0("mod.", x)))
        return(preservedEpc)
    }
    
    netCDF <- function () {
        stop("This function is not inplemented yet")
    }
    
    ## Call one function according to the outputType
    switch(outputType,
           "oneCsv" = (a <- oneCsv()),
           "moreCsv" = (a <- moreCsv()),
           "netCDF" = (a <- netCDF()))
    write.csv(a,"preservedEpc.csv")
   
    setwd(currDir)
    return(a)
}

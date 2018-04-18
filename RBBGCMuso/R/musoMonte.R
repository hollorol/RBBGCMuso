#' musoMonte
#'
#' This funcion is fundamental for the BiomBGC-MuSo modell related functions like spinupMuso, normalMuso, rungetMuso, because it sets the modells environment.
#' inputDir = "./",
#' @author Roland Hollos
#' @param settings Do you want to run multiple modell paralelly, if yes, set this variable to TRUE
#' @param parameters stores the place of the modell-executable file. In normal usage, you don't have to be set this, because a RBBgcmuso package contains allways the latest modell executable. In spite of this, if you would like to use this package for modell development or just want to use different models (for example for comparison), you will find it useful
#' @param calibrationPar You may want to change some parameters in your epc file, before you run the modell. You have to select the appropirate modell parameters. You can refence to these with the number of the line in the epc file where the variables are. It indexes from one. You should use a vector for this, like: c(1,5,8)
#' @param outLoc Where should the modell puts its outputs. You should give a location for it via this variable, for example: outputLoc="/place/of/the/outputs/"
#' @param iterations Usually it is the root directory, where you put the iniFiles for the modell
#' @param preTag Via metInput parameter, you can tell the modell where are the meteorological files. As default it reads this from the iniFiles.
#' @param inputName Via CO2 parameter, you can tell the modell where are the CO2 data files. As default it reads this from the iniFiles.
#' @param outputType Via planting parameter, you can tell the modell where are the data files, which contains the planting informations. As default it reads this from the iniFiles.
#' @param fun Via thining parameter, you can tell the modell where are the data files, which contains the thining informations. As default it reads this from the iniFiles.
#' @param varIndex Via mowing parameter, you can tell the modell where are the data files, which contains the mowing informations. As default it reads this from the iniFiles.
#' @param doSensitivity Via grazing parameter, you can tell the modell where are the data files, which contains the grazing informations. As default it reads this from the iniFiles.
#' @param onDisk Via harvesting parameter, you can tell the modell where are the data files, which contains the harvesting informations. As default it reads this from the iniFiles.
#' @export
musoMonte <- function(settings=NULL,
                     parameters,
                      inputDir = "./",
                      outLoc = "./calib",
                      iterations = 10,
                      preTag = "mont-",
                      inputName = paste0(pretag,"epcs.csv"),
                      outputType = "moreCsv",
                      fun=mean,
                      varIndex=8,
                      doSensitivity=FALSE,
                      onDisk=FALSE,
                     ...)
{
    outLocPlain <- basename(outLoc)
    currDir <- getwd()
    inputDir <- normalizePath(inputDir)
  tmp <- file.path(outLoc,"tmp/")

    if(!dir.exists(outLoc)){
    dir.create(outLoc)
    warning(paste(outLoc," is not exists, so it was created"))
  }

  if(dir.exists(tmp)){
    stop("There is a tmp directory inside the output location, please replace it. tmp is an important temporary directory for the function")
    }
dir.create(tmp)
  outLoc <- normalizePath(outLoc)
  tmp <- normalizePath(tmp)

  inputFiles <- file.path(inputDir,grep(basename(outLoc),list.files(inputDir),invert = TRUE,value = TRUE)) 

  
  for(i in inputFiles){
    file.copy(i,tmp)
  }
  setwd(tmp)
  
  if(is.null(settings)){
    settings <- setupMuso()
  }
  
  parameterNames <- parameters[,1]
  parReal <- parameters[,-1]
  Otable <- OtableMaker(parReal)
  A <- as.matrix(Otable[[1]][,c(2,4,5,6)])
  B <- as.matrix(Otable[[2]])
  settings$calibrationPar <- A[,1] 
  pretag <- file.path(outLoc,preTag)
  
  ##reading the original epc file at the specified
  ## row numbers
  
  origEpcFile <- readLines(settings$epcInput[2])
  
  origEpc <- unlist(lapply(settings$calibrationPar, function (x) {
    as.numeric(unlist(strsplit(origEpcFile[x],split="[\t ]"))[1])
  }))
  
  ## Prepare the preservedEpc matrix for the faster
  ##  run.
  preservedEpc <- matrix(nrow = (iterations +1 ), ncol = length(settings$calibrationPar))
  preservedEpc[1,] <- origEpc
  names(origEpc)<-colnames(preservedEpc)
  colnames(preservedEpc) <- Otable[[1]][,1]
  write.table(t(origEpc),row.names = FALSE,"preservedEpc.csv",sep=",")
  ## Save the backupEpc, while change the settings
  ## variable and set the output.
  file.copy(settings$epc[2],"savedEpc",overwrite = TRUE) # do I need this? 
  pretag <- file.path(outLoc,preTag)
  
  ## Creating function for generating separate
  ## csv files for each run
  moreCsv <- function(){
      a <- list()
      for(i in 1:iterations){
          parVar <- musoRandomizer(A,B)[,2]
                                        #preservedEpc[(i+1),] <- parVar
          write.table(x=t(parVar),file="preservedEpc.csv",row.names=FALSE,col.names=FALSE, append=TRUE,sep=",")
          exportName <- paste0(preTag,i,".csv")
          tempData <- calibMuso(settings,debugging = "stamplog",
                               parameters = parVar,
                               keepEpc = TRUE)
          write.csv(x=tempData,file=exportName)
          a[[i]]<-fun(tempData[,varIndex])
      
      }
      return(a)
  }
  
  ## Creating function for generating one
  ## csv files for each run
  
  oneCsv <- function () {
    numDays <- settings$numdata[1]
    if(!onDisk){
      for(i in 1:iterations){
      
        parVar <- apply(parameters,1,function (x) {
            runif(1, as.numeric(x[3]), as.numeric(x[4]))})
          
        preservedEpc[(i+1),] <- parVar
        exportName <- paste0(preTag,".csv")
        write.csv(parvar,"preservedEpc.csv",append=TRUE)
        calibMuso(settings,debugging = "stamplog",
                  parameters = parVar,keepEpc = TRUE) %>%
                  {mutate(.,iD = i)} %>%
                  {write.csv(.,file=exportName,append=TRUE)}
      }
      
    return(preservedEpc)
      } else {
      
    }
  }
  
  netCDF <- function () {
    stop("This function is not inplemented yet")
  }
  
  ## Call one function according to the outputType
  switch(outputType,
         "oneCsv" = (a <- oneCsv()),
         "moreCsv" = (a <- moreCsv()),
         "netCDF" = (a <- netCDF()))
  
  ## Change back the epc file to the original
    for(i in file.path("./",grep(outLocPlain, list.files(inputDir), invert = TRUE, value = TRUE))){
        file.remove(i,recursive=TRUE)
    }
    for(i in list.files()){
        file.copy(i,outLoc,recursive=TRUE,overwrite = TRUE)
    }
    
    unlink(tmp,recursive = TRUE)
    setwd(currDir)
    file.copy("savedEpc",settings$epc[2],overwrite = TRUE)
    return(do.call("c",a))

}

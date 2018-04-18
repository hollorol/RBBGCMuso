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

  currDir <- getwd()
  tmp <- file.path(outLoc,"tmp/")

    if(!dir.exists(outLoc)){
    dir.create(outLoc)
    warning(paste(outLoc," is not exists, so it was created"))
  }

  if(dir.exists(tmp){
    stop("There is a tmp directory inside the output location, please replace it. tmp is an important temporary directory for the function")
    }
  dir.create(tmp)
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
  colnames(preservedEpc) <- Otable[[1]][,1]
  write.csv(preservedEpc,"preservedEpc.csv")
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
          write.csv(x=parVar,file="preservedEpc.csv", append=TRUE)
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
  
  file.copy(savedEpc,settings$epc[2],overwrite = TRUE)
  return(do.call("c",a))

}  

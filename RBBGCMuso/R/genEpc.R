#' randEpc
#'
#' randEpc is a random epc creator based on musoMonte
#' @author Roland HOLLOS
#' @param parameterFile parameters.csv file location
#' @param location output location directory
#' @param sourceEpc the original epc file-the template
#' @param iteration the number of iterations
#' @export

randEpc <- function(parameterFile = "parameters.csv", location = "./epcDir",
                    sourceEpc = "maize.epc", iterations = 1000, constrains = NULL){
    
    if(!dir.exists(location)){
        dir.create(location)
    }
    sourceEpc <- normalizePath(sourceEpc)
    currDir <- getwd()
    parameters <- read.csv(parameterFile)

    if(iterations < 3000){
        randVals <- musoRand(parameters = parameters,constrains = constrains, iterations = 3000)
        randVals[[2]]<- randVals[[2]][sample(1:3000,iterations),]
    } else {
        randVals <- musoRand(parameters = parameters,constrains = constrains, iterations = iterations)
    }
    file.copy(sourceEpc,location,overwrite = TRUE)
    setwd(location)
    for(i in seq(iterations)){
        epcOut <- gsub("\\.",paste0("-",i,"."),basename(sourceEpc))
        changemulline(filePaths = basename(sourceEpc), calibrationPar = randVals[[1]],
                      contents = randVals[[2]][i,],fileOut = epcOut, fileToChange = "epc")        
    }
   setwd(currDir)
}

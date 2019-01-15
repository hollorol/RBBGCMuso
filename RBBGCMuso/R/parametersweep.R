#' paramSweep
#' 
#' This function is for testing the modell response to change a set of input variables. It generates an html file which contains a set of graphics of the ...
#' @author Roland Hollos
#' @param inputDir  The directory which contains the MuSo model's ini files
#' @param parameters  A csv file's path which contains the input parameters. The first row must be the name of the parameters, the second is the index of the parameters(row index in the input file), the third is the minimum value of the parameters, the forth is the maximum value of the parameters. If it is not privided, a filebrowser will pop up.
#' @param outputDir The path of the directory where the html file will be generated. 
#' @param iterations The number of changes in the parameter
#' @param outVar The name of the output variable to plot, of the MuSo code of it.
#' @param htmlOutName The name of the rendered html file 
#' @importFrom rmarkdown render
#' @importFrom digest digest
#' @importFrom tcltk tk_choose.files 
#' @export

paramSweep <- function(inputDir="./",
                       parameters=NULL,
                       outputDir=NULL,
                       iterations=10,
                       outVar="daily_gpp",
                       htmlOutName = "paramsweep.html"){
    currDir <- getwd()
    opSystem <- Sys.info()[[1]]
    if(is.character(outVar)){
        varNames <- as.data.frame(musoMappingFind(outVar))
        if(nrow(varNames)!=1){
            warning("There are more than one output variable in conection with ", outVar, ". The first possibility were choosen.")
            print(varNames)
            outVarIndex <- unlist(varNames[1,1])
            varNames <- as.character(unlist(varNames[1,2]))
        } else {
            outVarIndex <- unlist(varNames[1,1])
            varNames <- as.character(unlist(varNames[1,2]))
        }
    } else {
        varNames <- musoMapping(outVar)
        outVarIndex<-outVar
    }
    
    
    if(is.null(parameters)){
        parameters <- tcltk::tk_choose.files(caption = "Please select a file with the parameters and the ranges")
    }
    
    rmdFile <- "---\ntitle: \"ParameterSweep basic\"\n---\n```{r setup, include=FALSE}\nknitr::opts_chunk$set(echo = TRUE)\n```\n```{r, echo=FALSE}\nsuppressWarnings(library(RBBGCMuso))\n```\n```{r, echo=FALSE}\nparameters <- read.csv(\"parameters.csv\")\n```\n```{r,fig.width=10, fig.height=3, echo=FALSE}\nnumPar\nfor(i in 1:numPar){\n  suppressWarnings(musoQuickEffect(calibrationPar=parameters[i,2],startVal = parameters[i,3], endVal = parameters[i,4],\nnSteps = 9,\noutVar = \"daily_gpp\",\nparName = parameters[i,1]))\n}\n```"
    rmdVec <- unlist(strsplit(rmdFile,"\n"))
    rmdVec[11] <- paste0("parameters <- read.csv(\"",parameters,"\", stringsAsFactor = FALSE)")
    rmdVec[14] <- "numPar <- nrow(parameters)"
    rmdVec[17] <- paste0("nSteps = ", iterations - 1,",")
    rmdVec[18] <- paste0("outVar = \"",varNames,"\",")

    if(!is.null(outputDir)){
        setwd(outputDir)
    }
    
    randName <- paste0(digest(date(),"md5"),"-paramsweep.rmd")
    writeLines(rmdVec,randName)
    render(randName,output_file = htmlOutName)
    unlink(randName)

    if(opSystem == "Linux"){
        system(paste0("xdg-open ",htmlOutName))
    } else {
        if(opSystem == "Windows"){
            system(paste0("start ",htmlOutName))
        } else {
            system(paste0("open ",htmlOutName))
        }
    }
    setwd(currDir)
    
}


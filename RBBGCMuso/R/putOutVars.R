#' putOutVars
#'
#'This function is for adding variables in the inifiles.
#'
#' @author Roland Hollos
#' @param IniFile The name of the normal ini file.
#' @param outputVars List of the output codes
#' @keywords internal 
putOutVars <- function(iniFile,outputVars,modifyOriginal = FALSE){
    ini <- readLines(iniFile)
    numVarsOriginal <- as.numeric(ini[grep("DAILY_OUTPUT",ini)+1])
    if(!modifyOriginal){
        iniOut <- paste0(tools::file_path_sans_ext(basename(iniFile)),"-tmp.",tools::file_ext(iniFile))
    } else {
        iniOut <- iniFile
    }
    
    outNames <- sapply(outputVars,musoMapping)
    partOne <- ini[1:grep("DAILY_OUTPUT",ini)]
    partTwo <- ini[grep("ANNUAL_OUTPUT",ini):(length(ini))]
    numVars <- length(outputVars)
    fileContent <- c(partOne,
                     as.character(numVars),
                     sapply(outputVars,function (x) {
                         paste(as.character(x),musoMapping(x),sep = " ")
                     }),
                     "",
                     partTwo)
    writeLines(fileContent,iniOut)
    return(list(names=outNames,ratio=numVars/numVarsOriginal))
}


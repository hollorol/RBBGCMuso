#' This is the function which is capable change multiple specific lines to other using their row numbers.
#'
#' he function uses the previous changspecline function to operate.
         ##From now changespecline is in the forarcheologist file, because its no longer needed
#' 
#' @author Roland Hollos
#' @keywords internal

changemulline <- function(filename,calibrationPar,contents){
    ##This is the function which is capable change multiple specific lines to other using their row numbers.
    ##The function uses the previous changspecline function to operate.
    ##From now changespecline is in the forarcheologist file, because its no longer needed
    varnum <- length(calibrationPar)
    contents <- as.list(contents)
    if(length(contents)!=varnum)
    {
        stop(" number of the values is not the same as the number of the changed parameters")
    }
    
    readedFile = readLines(filename,-1)

    for(i in 1:varnum){
        readedFile[calibrationPar[i]] <- paste(contents[[i]],collapse = " ")
    }
    
    writeLines(unlist(readedFile),filename)
}

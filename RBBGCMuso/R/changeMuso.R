#' changemulline 
#'
#' The function uses the previous changspecline function to operate.
#' 
#' @author Roland Hollos
#' @export

changemulline <- function(filePaths, calibrationPar, contents, src=NULL, outFiles=filePaths){
    # browser()
    if(is.null(src)){
        src <- filePaths
    }
    
    fileStringVector <- readLines(src)
    Map(function(index, content){
           fileStringVector <<- changeByIndex(index, content, fileStringVector)

    }, calibrationPar, contents)
    writeLines(fileStringVector, outFiles)
    
}

changeNth <- function (string,place,replacement) {
    trimws(gsub(sprintf("^((.*?\\s+){%s})(.*?\\s+)", place), sprintf("\\1%s ", replacement), paste0(string," "), perl=TRUE),
           which="right")
}

changeByIndex <- function (rowIndex, parameter, fileStringVector){
    h <- round((rowIndex*100) %% 10)
    i <- as.integer(rowIndex)
    fileStringVector[i] <- changeNth(fileStringVector[i], h, parameter)        
    fileStringVector
}

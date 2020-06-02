#' This is the function which is capable to change multiple specific lines to others using their row numbers.
#'
#' The function uses the previous changspecline function to operate.
         ##From now changespecline is in the forarcheologist file, because itis  no longer needed
#' 
#' @author Roland Hollos
#' @keywords internal
#' 


changemulline <- function(filePaths, calibrationPar, contents, src){
    # browser()
    if(is.null(src)){
        src <- filePaths
    }
    
    fileStringVector <- readLines(src)
    Map(function(index, content){
           fileStringVector <<- changeByIndex(index, content, fileStringVector)

    }, calibrationPar, contents)
    writeLines(fileStringVector, filePaths)
    
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

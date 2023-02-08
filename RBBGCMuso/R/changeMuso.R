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


#' musoGetValues 
#' 
#' Get values from a musofile by supplying muso indices
#'
#' @param filename The name of the musofile we want the value from (e.g. epc file)
#' @param indices muso indices
#' @usage musoGetValues(filename, indices)
#' @export 

musoGetValues <- function(filename, indices){
    sapply(indices, function(index){
               colIndex <- round((index*100) %% 10) + 1
               rowIndex <- as.integer(index)
               as.numeric(unlist(strsplit(readLines(filename)[rowIndex],split="\\s+"))[colIndex])

})
}

#' musoCompareFiles 
#' 
#' A simple wrapper function based on musoGetValues where you can get multiple values from multiple files 
#' using the supplied indices. It is useful for comparing files.
#'
#' @param  filenames The name of the files where you can get the data from
#' @param indices muso indices
#' @usage musoCompareFiles(filenames, indices) 
#' @export 

musoCompareFiles <- function(filenames, indices){
    sapply(filenames, function(fn){
        musoGetValues(fn,indices)
    })
}

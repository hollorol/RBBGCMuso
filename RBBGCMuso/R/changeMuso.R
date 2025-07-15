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
    
    #fileStringVector <- readLines(src)
    fileStringVector <- readLines(src,warn=FALSE)
    Map(function(index, content){
           fileStringVector <<- changeByIndex(index, content, fileStringVector)

    }, calibrationPar, contents)
    writeLines(fileStringVector,outFiles)
}

prettyChangemulline <- function(filePaths, calibrationPar, contents, src=NULL, outFiles=filePaths){
    # browser()
    if(is.null(src)){
        src <- filePaths
    }
    
    #fileStringVector <- readLines(src)
    fileStringVector <- readLines(src,warn=FALSE,encoding="UTF-8-BOM")
    Map(function(index, content){
           fileStringVector <<- prettyChangeByIndex(index, content, fileStringVector)

    }, calibrationPar, contents)
    #writeLines(fileStringVector, outFiles)
    writeLines(fileStringVector,outFiles,useBytes=TRUE)
}

changeNth <- function (string,place,replacement) {
    trimws(gsub(sprintf("^((.*?\\s+){%s})(.*?\\s+)", place), sprintf("\\1%s ", replacement), paste0(string," "), perl=TRUE),
           which="right")
}

prettyChangeNth <- function(string, place, replacement) {
    # Split the string by any whitespace into fields
    fields <- strsplit(string, "\\s+")[[1]]
    
    # Check if the requested position exists
    if (place + 1 > length(fields)) {
        warning("Column not found.")
        return(string)
    }
    
    # Replace the target field
    fields[place + 1] <- as.character(replacement)
    
    # Identify which fields are numeric (including decimals)
    is_numeric <- grepl("^-?[0-9]*\\.?[0-9]+$", fields)
    
    # Build the new string manually
    new_string <- ""
    for (i in 1:length(fields)) {
        # Add the field
        new_string <- paste0(new_string, fields[i])
        
        # Add separator if not the last field
        if (i < length(fields)) {
            if (is_numeric[i]) {
                # Two tabs after numeric fields
                new_string <- paste0(new_string, "\t\t")
            } else {
                # Single space after text fields
                new_string <- paste0(new_string, " ")
            }
        }
    }
    
    return(new_string)
}

changeByIndex <- function (rowIndex, parameter, fileStringVector){
    h <- round((rowIndex*100) %% 10)
    i <- as.integer(rowIndex)
    fileStringVector[i] <- changeNth(fileStringVector[i], h, parameter)        
    fileStringVector
}

prettyChangeByIndex <- function (rowIndex, parameter, fileStringVector){
    h <- round((rowIndex*100) %% 10)
    i <- as.integer(rowIndex)
    fileStringVector[i] <- prettyChangeNth(fileStringVector[i], h, parameter)        
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


#' changeMuso
#' 
#' A function to change lines mainly in soil, epc or ini files 
#' @param calibrationPar the line number in the chosen file of the parameters to change 
#' @param parameters the values for the chosen parameters listed in calibrationPar
#' @param fileToChange defining which file to change
#' @usage changeMuso(settings, parameters, calibrationPar, fileToChange, fixAlloc)
#' @export 
changeMuso <- function(settings, parameters, calibrationPar, fileToChange, fixAlloc){
        #print(paste("Value of fileToChange:", fileToChange))

        switch(fileToChange,
               epc = {
                  fileToChange <- tools::file_path_as_absolute(settings$epcInput[2])
                
               }, 
               soil = {
                   fileToChange <- tools::file_path_as_absolute(settings$soilFile[2])
                   
                   
               },

               fileToChange <- tools::file_path_as_absolute(fileToChange)
        )

        bck  <- file.path(settings$inputLoc, "bck",
                          basename(fileToChange)) 

        changemulline(filePaths = fileToChange,
                      calibrationPar = calibrationPar,
                      contents = parameters,
                      src = if(file.exists(bck)){
                          bck
                      } else {
                          NULL
                      })
        if(fixAlloc){
            fixAlloc(settings)
        }
                       

}


prettyChangeMuso <- function(settings, parameters, calibrationPar, fileToChange, fixAlloc){
       

        switch(fileToChange,
               epc = {
                  fileToChange <- tools::file_path_as_absolute(settings$epcInput[2])
     
               }, 
               soil = {
                   fileToChange <- tools::file_path_as_absolute(settings$soilFile[2])
                  
               },

               fileToChange <- tools::file_path_as_absolute(fileToChange)
        )

        bck  <- file.path(settings$inputLoc, "bck",
                          basename(fileToChange)) 

        prettyChangemulline(filePaths = fileToChange,
                      calibrationPar = calibrationPar,
                      contents = parameters,
                      src = NULL
                      )
        if(fixAlloc){
            fixAlloc(settings)
        }
                       

}

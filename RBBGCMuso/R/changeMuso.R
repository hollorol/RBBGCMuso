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
    fileStringVector <- readLines(src,warn=FALSE,encoding="UTF-8")
    Map(function(index, content){
           fileStringVector <<- changeByIndex(index, content, fileStringVector)

    }, calibrationPar, contents)
    #writeLines(fileStringVector, outFiles)
    writeLines(fileStringVector,outFiles,useBytes=TRUE)
}

prettyChangemulline <- function(filePaths, calibrationPar, contents, src=NULL, outFiles=filePaths){
    # browser()
    if(is.null(src)){
        src <- filePaths
    }
    
    #fileStringVector <- readLines(src)
    fileStringVector <- readLines(src,warn=FALSE,encoding="UTF-8")
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
    # Find all fields (non-whitespace plus following spaces)
    m <- gregexpr("\\S+\\s*", string, perl = TRUE)
    fields <- regmatches(string, m)[[1]]
    
    # Check that the requested column exists 
    if ((place + 1) > length(fields)) {
        warning("Column not found.")
        return(string)
    }
    
    # Extract the target field (which includes its trailing whitespace)
    target_field <- fields[place + 1]
    # Separate the field value and its trailing whitespace.
    orig_val <- sub("^(\\S+)(\\s*)$", "\\1", target_field)
    trailing_ws <- sub("^(\\S+)(\\s*)$", "\\2", target_field)
    
    # Convert the replacement to a character string.
    new_val <- as.character(replacement)
    
    # If the new value is shorter than the original field, pad it on the right.
    if (nchar(new_val) < nchar(orig_val)) {
        new_val <- sprintf("%-*s", nchar(orig_val), new_val)
    }
    
    # Rebuild the new field by appending the preserved trailing whitespace.
    new_field <- paste0(new_val, trailing_ws)
    
    # Use the match positions from gregexpr to rebuild the string.
    starts <- as.integer(m[[1]])
    lengths <- attr(m[[1]], "match.length")
    target_start <- starts[place + 1]
    target_end <- target_start + lengths[place + 1] - 1
    
    new_string <- paste0(
        substring(string, 1, target_start - 1),
        new_field,
        substring(string, target_end + 1)
    )
    
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

changeMuso <- function(settings, parameters, calibrationPar, fileToChange, fixAlloc){
        #print(paste("Value of fileToChange:", fileToChange))

        switch(fileToChange,
               epc = {
                  fileToChange <- tools::file_path_as_absolute(settings$epcInput[2])
               #print("AAAAA")
               #print(settings$epcInput[2])
               #print(settings$soilFile)
                
               }, 
               soil = {
                   fileToChange <- tools::file_path_as_absolute(settings$soilFile[2])
                   #print("soitest")
                   #print(settings$soilFile[2])
               },

               fileToChange <- tools::file_path_as_absolute(fileType)
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
                       # fileToChange = fileToChange,)

}


prettyChangeMuso <- function(settings, parameters, calibrationPar, fileToChange, fixAlloc){
       

        switch(fileToChange,
               epc = {
                  fileToChange <- tools::file_path_as_absolute(settings$epcInput[2])
     
               }, 
               soil = {
                   fileToChange <- tools::file_path_as_absolute(settings$soilFile[2])
                  
               },

               fileToChange <- tools::file_path_as_absolute(fileType)
        )

        bck  <- file.path(settings$inputLoc, "bck",
                          basename(fileToChange)) 

        prettyChangemulline(filePaths = fileToChange,
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

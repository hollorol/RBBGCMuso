cleanupMuso <- function(location=NULL, simplicity=TRUE,deep=FALSE){
    areThereAnyFolders <- FALSE
    whereAmI <- getwd()
    if(!is.null(location)){
        setwd(location)
    }

    if(deep){
        if(dir.exists("LOG")){
            setwd("LOG")
            file.remove(
                grep("(out$)|(endpoint$)|(log$)",
                     list.files(), value = T))
            areThereAnyFolders <- TRUE
        }
        
        if(dir.exists("../ERROR")){
            setwd("../ERROR")

            file.remove(
                grep("(out$)|(endpoint$)|(log$)",
                     list.files(), value = T))
            areThereAnyFolders <- TRUE
        }


        if(dir.exists("../EPCS")){
            setwd("../EPCS")

            file.remove(
                grep("(out$)|(endpoint$)|(log$)|(epc$)",
                     list.files(), value = T))
            areThereAnyFolder <- TRUE
        }

        if(dir.exists("../WRONGEPC")){
            setwd("../WRONGEPC")
            file.remove(
                grep("(out$)|(endpoint$)|(log$)|(epc$)",
                     list.files(), value = T))
            areThereAnyFolders <- TRUE
        }

        if(areThereAnyFolders){
            setwd("..")    
            file.remove(
                grep("(out$)|(endpoint$)|(log$)",
                     list.files(), value = T))
        } else {
            file.remove(
                grep("(out$)|(endpoint$)|(log$)",
                     list.files(), value = T))}
    }
    
    if(!simplicity){    
        file.remove(
            grep("(out$)|(endpoint$)|(log$)",
                 list.files(), value = T)
        )} else {
             file.remove(grep("log$",list.files(),value = T))
         }

    setwd(whereAmI)
    
}

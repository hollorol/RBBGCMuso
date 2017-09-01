#'cleanupMuso
#'
#' cleanupMuso can erase all of the unnecessary log and output files.
#' 
#' @author Roland Hollos
#' @param location It is the place where your output files are.
#' @param simplicity TRUE or FALSE. If true cleanupMuso will erase only the log files from only the location
#' @param deep If it is TRUE, it will delete every files from the subdirectories also
#' @usage cleanupMuso(location=NULL, simplicity=TRUE,deep=FALSE)
#' @export


cleanupMuso <- function(location=NULL, simplicity=TRUE,deep=FALSE){
    
    
    whereAmI <- getwd()
    if(!is.null(location)){
        setwd(location)
    }

    if(deep){


        
        if(dir.exists(paste(location,"/LOG",sep=""))){
            file.remove(
                list.files("LOG/",pattern="(out$)|(endpoint$)|(log$)", full.names=TRUE)
         )
        }
        
        if(dir.exists("ERROR")){
            file.remove(
                list.files("ERROR/",pattern="(out$)|(endpoint$)|(log$)", full.names=TRUE))
        }

        if(dir.exists("EPCS")){
            file.remove(
                list.files("EPCS/",pattern="(out$)|(endpoint$)|(log$)", full.names=TRUE))
        }

        if(dir.exists("WRONGEPC")){
            file.remove(
                list.files("WRONGEPC/",pattern="(out$)|(endpoint$)|(log$)", full.names=TRUE))
        }

       file.remove(list.files(pattern="(out$)|(endpoint$)|(log$)",full.names=TRUE))}
    
    
    if(!simplicity){    
        file.remove(
            grep("(out$)|(endpoint$)|(log$)",
                 list.files(), value = T)
        )} else {
             file.remove(grep("log$",list.files(),value = T))
         }

    setwd(whereAmI)
    
}

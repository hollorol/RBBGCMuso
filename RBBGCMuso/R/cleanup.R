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
    
    if(is.null(location)){
        location<-"./"
    }

    logDir<-file.path(location,"LOG")
    errDir<-file.path(location,"ERROR")
    epcDir<-file.path(location,"EPCS")
    wroDir<-file.path(location,"WRONGEPC")

    if(deep){

        if(dir.exists(logDir)){
            file.remove(
                list.files(logDir,pattern="(out$)|(endpoint$)|(log$)", full.names=TRUE)
         )
        }
        
        if(dir.exists(errDir)){
            file.remove(
                list.files(errDir,pattern="(out$)|(endpoint$)|(log$)", full.names=TRUE))
        }

        if(dir.exists(epcDir)){
            file.remove(
                list.files(epcDir,pattern="(out$)|(endpoint$)|(log$)", full.names=TRUE))
        }

        if(dir.exists(wroDir)){
            file.remove(
                list.files(wroDir,pattern="(out$)|(endpoint$)|(log$)", full.names=TRUE))
        }

       file.remove(list.files(location, pattern="(out$)|(endpoint$)|(log$)",full.names=TRUE))}
    
    
    if(!simplicity){    
        file.remove(list.files(location, pattern="(out$)|(endpoint$)|(log$)",full.names=TRUE))
    } else {
             file.remove(list.files(location, pattern="log$",full.names=TRUE))}
         
}

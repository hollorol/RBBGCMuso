#' getDailyOutputList
#'
#' bla bla
#' @param settings bla
#' @export


getDailyOutputList <- function(settings=NULL){
    if(is.null(settings)){
        settings <- setupMuso()
    }
    varTable <- getOption("RMuso_varTable")$'6'
    toPrint <- varTable[which(varTable$codes %in% as.numeric(settings$dailyVarCodes)),]
    toPrint <- cbind.data.frame(index=1:nrow(toPrint),toPrint)
    print(toPrint, row.names=FALSE)
}

#' getAnnualOutputList
#'
#' bla bla
#' @param settings bla
#' @export


getAnnualOutputList <- function(settings=NULL){
    if(is.null(settings)){
        settings<- setupMuso()
    }
    varTable <- getOption("RMuso_varTable")$'6'
    toPrint <- varTable[which(varTable$codes %in% as.numeric(settings$annualVarCodes)),]
    toPrint <- cbind.data.frame(index=1:nrow(toPrint),toPrint)
    print(toPrint, row.names=FALSE)
}

#' getDailyOutputList
#'
#' bla bla
#' @param settings bla
#' @export


getDailyOutputList <- function(settings=NULL){
    if(is.null(settings)){
        settings<- setupMuso()
    }
    print(settings$dailyOutputTable, row.names=FALSE)
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
    print(settings$annualOutputTable, row.names=FALSE)
}

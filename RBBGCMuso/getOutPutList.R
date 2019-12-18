#' getDailyOutputList
#'
#' bla bla
#' @param settings bla
#' @export


getDailyOutputList <- function(settings=NULL){
    if(is.null(settings)){
        settings<- setupMuso()
    }
    settings$dailyOutputTable
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
    settings$annualOutputTable
}

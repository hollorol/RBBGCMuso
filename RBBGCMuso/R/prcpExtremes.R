#' prcpExtremes 
#'
#' This function calculates the yearly extremes, and averages for a given meteorology file (mtc43)  
#'
#' @author Erzsebet Kristof 
#' @param metFileName The name of the meteorology file (mtc43)
#' @param skip number of header lines in meteorology file
#' @export 

prcpExtremes <- function(metFileName, skip = 4){

  metTable <- tryCatch(read.table(metFileName, skip = skip), error = function(e){
    stop(sprintf("Cannot read or find meteorology file: %s", metFileName))
  })

  yearlySum <- tapply(metTable$V6,list(metTable$V1),sum)

  timeFrame <- range(metTable[,1])
  cat("Yearly and daily extremes for the time period of", timeFrame[1], "-", timeFrame[2], ": 
      Minimum and maximum of yearly precipitation sums:", min(yearlySum, na.rm=TRUE), "cm and", max(yearlySum, na.rm=TRUE), "cm.
      Minimum and maximum of daily precipitation sums:",  min(metTable$V6, na.rm=TRUE), "cm and", max(metTable$V6, na.rm=TRUE), "cm.\n")
}

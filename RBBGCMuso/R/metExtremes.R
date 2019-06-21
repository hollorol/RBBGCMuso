#' Meteorology
#'
#' This function calculates the daily and yearly extremes for a given meteorology file (mtc43)  
#'
#' @author Erzsebet Kristof 
#' @param metFileName The name of the meteorology file (mtc43)
#' @param skip number of header lines in meteorology file
#' @export 

MetExtremes <- function(metFileName, skip = 4){

  metTable <- tryCatch(read.table(metFileName, skip = skip), error = function(e){
    stop(sprintf("Cannot read or find meteorology file: %s", metFileName))
  })

  yearlyPrcpSum <- tapply(metTable$V6,list(metTable$V1), sum)
  yearlyTempAvg <- tapply(metTable$V5,list(metTable$V1), mean)
  yearlySrdAvg <- tapply(metTable$V7,list(metTable$V1), mean)

  timeFrame <- range(metTable[,1])
  cat("Yearly and daily extremes of meteorological data for the time period of", timeFrame[1], "-", timeFrame[2], ":\n 
      Precipitation data:
      Minimum and maximum of daily sums: ", round(min(metTable$V6, na.rm=TRUE), digits=1), "cm and", round(max(metTable$V6, na.rm=TRUE), digits=1), "cm.
      Minimum and maximum of yearly sums:", round(min(yearlyPrcpSum, na.rm=TRUE), digits=1), "cm and", round(max(yearlyPrcpSum, na.rm=TRUE), digits=1), "cm.\n

      Temperature data:
      Lowest and highest daily temperatures (Tmin and Tmax): ", round(min(metTable$V4, na.rm=TRUE), digits=1), "°C and", round(max(metTable$V3, na.rm=TRUE), digits=1), "°C.
      Minimum and maximum of yearly averages (based on Tday):", round(min(yearlyTempAvg, na.rm=TRUE), digits=1), "°C and", round(max(yearlyTempAvg, na.rm=TRUE), digits=1), "°C.\n

      Solar radiation data:
      Minimum and maximum of daily solar radiation:    ",  round(min(metTable$V8, na.rm=TRUE), digits=1), "W m-2 and", round(max(metTable$V8, na.rm=TRUE), digits=1), "W m-2.     
      Yearly averages of the shortest and longest days:", round(mean(metTable$V8[na.omit(metTable$V9)==min(metTable$V9, na.rm=TRUE)]), digits=1), "W m-2 and", round(mean(metTable$V8[na.omit(metTable$V9)==max(metTable$V9, na.rm=TRUE)]), digits=1), "W m-2.\n
  
      Vapour pressure deficit data:
      Minimum and maximum of daily values:", round(min(metTable$V7, na.rm=TRUE), digits=1), "Pa and", round(max(metTable$V7, na.rm=TRUE), digits=1), "Pa.
      Minimum and maximum of yearly averages:", round(min(yearlySrdAvg, na.rm=TRUE), digits=1), "Pa and", round(max(yearlySrdAvg, na.rm=TRUE), digits=1), "Pa.\n")
  
}

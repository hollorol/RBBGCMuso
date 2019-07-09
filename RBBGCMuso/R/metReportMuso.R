#' metReportMuso
#'
#' This function calculates the daily and yearly statistics for a given meteorology file (mtc43).
#'
#' @author Erzsebet Kristof 
#' @param metFileName The name of the meteorology file (mtc43).
#' @param skip Number of header lines in meteorology file.
#' @param numericReport If numericReport is set to FALSE, the function returns with a text report. If numericReport is set to TRUE, the function returns with a numeric report.
#' @return It depends on the numericReport parameter. The function returns with a text report, or with a numeric report.
#' @export 

metReportMuso <- function(metFileName, skip = 4, numericReport = FALSE){

  intMin <- function(x){
    round(min(x,na.rm = TRUE), digits = 1)
  }

  intMax <- function(x){
    round(max(x,na.rm = TRUE), digits = 1)
  }

  sradAvgShortestDay <- function(x,y){
    round(mean(x[na.omit(y) == min(y, na.rm=TRUE)]), digits=1)
    
  }
  sradAvgLongestDay <- function(x,y){
    round(mean(x[na.omit(y) == max(y, na.rm=TRUE)]), digits=1)
    
  }
  
  metTable <- tryCatch(read.table(metFileName, skip = skip), error = function(e){
    stop(sprintf("Cannot read or find meteorology file: %s", metFileName))
  })

  yearlyPrcpSum <- tapply(metTable$V6,list(metTable$V1), sum)
  yearlyTempAvg <- tapply(metTable$V5,list(metTable$V1), mean)
  yearlyVpdAvg <- tapply(metTable$V7,list(metTable$V1), mean)


  timeFrame <- range(metTable[,1])
 if(!numericReport){
   cat("Daily and yearly statistics of meteorological data for the time period of",
      timeFrame[1], "-", timeFrame[2], ":\n
      
      Precipitation data:
      Minimum and maximum of daily sums:",
      intMin(metTable$V6), "cm and", intMax(metTable$V6), "cm.
      Minimum and maximum of yearly sums:",
      intMin(yearlyPrcpSum), "cm and", intMax(yearlyPrcpSum), "cm.\n
       
      Temperature data:
      Lowest and highest daily temperatures (Tmin and Tmax):",
      intMin(metTable$V4), "deg C and", intMax(metTable$V3), "deg C.
      Minimum and maximum of yearly averages (based on Tday):",
      intMin(yearlyTempAvg), "deg C and", intMax(yearlyTempAvg), "deg C.\n

      Solar radiation data:
      Minimum and maximum of daily values:",
      intMin(metTable$V8), "W m-2 and", intMax(metTable$V8), "W m-2.     
      Averages of the shortest and longest days:",
      sradAvgShortestDay(metTable$V8, metTable$V9),"W m-2 and",
      sradAvgLongestDay(metTable$V8, metTable$V9), "W m-2.\n
       
      Vapour pressure deficit data:
      Minimum and maximum of daily values:",
      intMin(metTable$V7), "Pa and", intMax(metTable$V7), "Pa.
      Minimum and maximum of yearly averages:",
      intMin(yearlyVpdAvg), "Pa and", intMax(yearlyVpdAvg), "Pa.\n")
   
 } else {
   report <- list()
   cat("Numeric report:\n")
   report["Precipitation"] <- list(data.frame(minimum = c(daily = intMin(metTable$V6),
                                                     yearly = intMin(yearlyPrcpSum)),
                                         maximum = c(daily = intMax(metTable$V6),
                                                     yearly = intMax(yearlyPrcpSum))))

   report["Temperature"] <- list(data.frame(minimum = c(daily = intMin(metTable$V4),
                                                     yearly = intMin(yearlyTempAvg)),
                                         maximum = c(daily = intMax(metTable$V3),
                                                     yearly = intMax(yearlyTempAvg))))
   
   report["Solar radiation"] <- list(data.frame(minimum = c(daily = intMin(metTable$V8),
                                                     shortest_longest_day = sradAvgShortestDay(metTable$V8, metTable$V9)),
                                         maximum = c(daily = intMax(metTable$V8),
                                                     shortest_longest_day = sradAvgLongestDay(metTable$V8, metTable$V9))))

   report["Vapour pressure deficit"] <- list(data.frame(minimum = c(daily = intMin(metTable$V7),
                                                        yearly = intMin(yearlyVpdAvg)),
                                            maximum = c(daily = intMax(metTable$V7),
                                                        yearly = intMax(yearlyVpdAvg))))
   
   return(report)
 }

}

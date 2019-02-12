#' musoDate
#'
#' This function generates MuSo compatibla dates for the data
#' @author Roland HOLLOS
#' @param startYear
#' @param numYears
#' @param timestep
#' @param combined
#' @param corrigated
#' @param format
#' @importFrom lubridate leap_year
#' @export


musoDate <- function(startYear, endYears = NULL, numYears, combined = TRUE, leapYearHandling = FALSE, prettyOut = FALSE){

    if(is.null(endYears) & is.null(numYears)){
        stop("You should provide endYears or numYears")
    }
    
    if(is.null(endYears)){
        endYear <- startYear + numYears -1
    }
    
    dates <- seq(from = as.Date(paste0(startYear,"01","01"),format = "%Y%m%d"), to =  as.Date(paste0(endYear,"12","31"),format = "%Y%m%d"), by = "day")
    if(leapYearHandling){
        if(prettyOut){
            return(cbind(format(dates,"%d.%m.%Y"),as.numeric(format(dates,"%d")),as.numeric(format(dates,"%m")),as.numeric(format(dates,"%Y")))   )
        }
        
        if(combined == FALSE){
            return(cbind(format(dates,"%d"),format(dates,"%m"),format(dates,"%Y")))
        } else {
            return(format(dates,"%d.%m.%Y"))            
        }

    } else {
        if(prettyOut){
            return(cbind(format(dates,"%d.%m.%Y"),as.numeric(format(dates,"%d")),as.numeric(format(dates,"%m")),as.numeric(format(dates,"%Y")))   )
        }
        dates <- dates[format(dates,"%m%d")!="0229"]

        if(combined == FALSE){
            return(cbind(format(dates,"%d"),format(dates,"%m"),format(dates,"%Y")))
        } else {
            return(format(dates,"%d.%m.%Y"))            
        }
    }
    
}

corrigLeapYear  <- function(data, dataCol, modellSettings = NULL, startYear, fromDate = NULL,toDate = NULL,formatString = "%Y-%m-%d"){
    data <- as.data.frame(data) 
    numDays <- nrow(data)
    dates <- seq(as.Date(paste0(startYear,"01","01"),format = "%Y%m%d"), by= "day", length = numDays)
    goodInd <- which(!(leap_year(dates)&
                       (format(date,"%m") == "12")&
                       (format(date,"%d") == "31")))
    realDate <- musoDate(startYear = startYear, numYears = numYears)
    
    data <- cbind.data.frame(real,data[goodInd])
    
    modellDates <- musoDate(startYear = settings$startYear,numYears = settings$numYears)
    
    

    if(is.null(modellSettings)){
        modellSettings <- setupMuso()
    }

   
}

alignDataWithModelIndex  <- function(){
    
} 

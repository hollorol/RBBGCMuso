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
            return(cbind(format(dates,"%d.%m.%Y"),
                         as.numeric(format(dates,"%d")),
                         as.numeric(format(dates,"%m")),
                         as.numeric(format(dates,"%Y")))   )
        }
        
        if(combined == FALSE){
            return(cbind(format(dates,"%d"),format(dates,"%m"),format(dates,"%Y")))
        } else {
            return(format(dates,"%d.%m.%Y"))            
        }

    } else {
         dates <- dates[format(dates,"%m%d")!="0229"]
        if(prettyOut){
            return(data.frame(date = format(dates,"%d.%m.%Y"),
                              day = as.numeric(format(dates,"%d")),
                              month = as.numeric(format(dates,"%m")),
                              year = as.numeric(format(dates,"%Y"))))
        }
       

        if(combined == FALSE){
            return(cbind(format(dates,"%d"),format(dates,"%m"),format(dates,"%Y")))
        } else {
            return(format(dates,"%d.%m.%Y"))            
        }
    }
    
}
#' alignData
#'
#' This function align the data to the model and the model to the data
#' @importFrom lubridate leap_year
#' @keywords internal
alignData  <- function(mdata, dataCol, modellSettings = NULL, startDate=NULL, endDate=NULL, formatString = "%Y-%m-%d", leapYear = TRUE, continious = FALSE){

    if(continious){
        if((is.null(startDate) | is.null(endDate))){
            stop("If your date is continuous, you have to provide both startDate and endDate. ")
        }
        startDate <- as.Date(startDate, format = formatString)
        endDate <- as.Date(endDate, format = formatString)
    }

    if(is.null(modellSettings)){
        modellSettings <- setupMuso()
    }

    mdata <- as.data.frame(mdata)

    if(continious){
        dates <- seq(startDate, to = endDate, by= "day")
    } else{
        dates <- do.call(c,lapply(seq(nrow(mdata)), function(i){ as.Date(paste0(mdata[i,1],sprintf("%02d",mdata[i,2]),mdata[i,3]),format = "%Y%m%d")}))
    }

    ## if(!leapYear){
    ##     casualDays <- which(format(dates,"%m%d") != "0229")
    ##     #mdata <- mdata[casualDays,]
    ##     dates <- dates[casualDays]
    ## }
    
    mdata <- mdata[dates >= as.Date(paste0(modellSettings$startYear,"01","01"),format = "%Y%m%d"),]
    dates <- dates[dates >= as.Date(paste0(modellSettings$startYear,"01","01"),format = "%Y%m%d")]
    ## goodInd <- which(!(leap_year(dates)&
    ##                    (format(dates,"%m") == "12")&
    ##                    (format(dates,"%d") == "31")))
    
    if(leapYear){
        goodInd <- which(!(leap_year(dates)&
                       (format(dates,"%m") == "12")&
                       (format(dates,"%d") == "31")))
    } else {
        goodInd <-seq_along(dates)
    }
    realDate <- dates[which(format(dates,"%m%d") != "0229")]
    if(leapYear){
        mdata <- cbind.data.frame(realDate,mdata[goodInd,])        
    } else {
        mdata <- cbind.data.frame(dates,mdata)
    }
    modellDates <- as.Date(musoDate(startYear = modellSettings$startYear,numYears = modellSettings$numYears), format = "%d.%m.%Y")
    mdata <- mdata[mdata[,1] %in% modellDates,]
    nonEmpty <- which(!is.na(mdata[,dataCol+1]))
    mdata <- mdata[nonEmpty,]
    modIndex <- which(modellDates %in% mdata[,1])

    list(measuredData = mdata[,dataCol +1], modIndex = modIndex)
}

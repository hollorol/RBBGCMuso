#' getyearlycum
#'
#' Funtion for getting cumulative yearly data from observations
#' @author Roland Hollós
#' @param A vector of the daily observations.
#' @return A vector of yearly data


getyearlycum<-function(daily_observations){
  number_of_years<-length(daily_observations)/365
  # daily_observations[is.na(daily_observations)]<-0 # 3+NA=NA
  fr<-1
  yearlycum<-rep(NA,number_of_years)
  for(i in 1:number_of_years){
    to<-i*365
    yearlycum[i]<-sum(daily_observations[fr:to],na.rm = TRUE)
    fr<-i*365+1
  }
  return(yearlycum)
}

#' getyearlymax
#'
#' Function for getting the maximum values of the years, from daily data
#' @author Roland Hollós
#' @param daily_observations vector of the daily observations
#' @return A vector of yearly data
#' @usage getyearlymax(daily_observations)

getyearlymax<-function(daily_observations){
  number_of_years<-length(daily_observations)/365
  # daily_observations[is.na(daily_observations)]<-0 # 3+NA=NA
  fr<-1
  yearlycum<-rep(NA,number_of_years)
  for(i in 1:number_of_years){
    to<-i*365
    yearlymax[i]<-max(daily_observations[fr:to],na.rm=TRUE)
    fr<-i*365+1
  }
  return(yearlymax)
}

#' fextension
#'
#' A function for extracting the extension name from the filename string
#' @author Roland Hollós
#' @param filename The string of the filenam
#' @return the extension of the given file
#' @usage fextension(filename)
#' @example
#' fextension(filename="file.csv")

fextension <- function(filename){
    #this function gives back the given filenames extension
    fextension <- tail(unlist(strsplit(filename,"\\.")),1)
}

#'supportedMuso
#'
#' A function for getting the list of the output formats which is supported by RBBGCMuso
#' @author Roland Hollós
#' @param type "outputs" or "message", if you choose "outputs", it gives you a simple vector of the formats, if you choose "message", it gives you a full sentence which contains the same information. 
#' @return if you choose "outputs", it gives you a simple vector of the formats, if you choose "message", it gives you a full sentence which contains the same information.
#' @usage supportedMuso(type="outputs")
#' @example
#'  supportedMuso(type="outputs")

supportedMuso <- function(type="outputs"){
    supportedFormats <- c("xls","xlsx","odt","csv","txt")
    
    if(x=="outputs"){
        #If you add new format supports, please expand the lists
        return(supportedFormats)
    }
    if(x=="message"){
        return(cat("Supported formats are ",supportedFormats,"If your fileformat is something else, we automaticle coerced it to csv.\n"))
    }
}

#' corrigMuso
#'
#' This function leapyear-corrigate the output of the modell
#' @author Roland Hollós
#' @param settings This is the output of the setupMuso() function. It contains all of the RBBGCMuso settings
#' @param data the models outputdata
#' @return It returns the modells leapyear-corrigated output data.
#' @usage corrigMuso(settings, data)

corrigMuso <- function(settings, data){

    insertRow <- function(existingDF, newrow, r){
        nr <- nrow(existingDF)
        existingDF <- rbind(existingDF,rep(NA,ncol(existingDF)))
        existingDF[seq(r+1,nr+1),] <- existingDF[seq(r,nr),]
        existingDF[r,] <- newrow
        existingDF
    }


    numdays <- nrow(data)
    data <- data
    numyears <- settings$numyears
    leapyears <- musoLeapYears(settings)
    sylvesters <- data[seq(from=365, to=numdays, by=365),]
    ind <- 0
    for(i in 1:numyears){
        
        if(leapyears[i]){
            data <- insertRow(data,sylvesters[i],i*360+ind)
            ind <- ind+1
        }
    }
    return(data)
}

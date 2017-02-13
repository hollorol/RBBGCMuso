#' 'Funtion for getting cumulative yearly data from observations
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

#' 'Function for getting the maximum values of the years, from daily data
#' @author Roland Hollós
#' @param A vector of the daily observations
#' @return A vector of yearly data

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

fextension <- function(x){
    #this function gives back the given filenames extension
    fextension <- tail(unlist(strsplit(x,"\\.")),1)
}

supportedMuso <- function(x="outputs"){
    supportedFormats <- c("xls","xlsx","odt","csv","txt")
    
    if(x=="outputs"){
        #If you add new format supports, please expand the lists
        return(supportedFormats)
    }
    if(x=="message"){
        return(cat("Supported formats are ",supportedFormats,"If your fileformat is something else, we automaticle coerced it to csv.\n"))
    }
}

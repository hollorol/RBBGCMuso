#' getyearlycum
#'
#' Funtion for getting cumulative yearly data from observations
#' @author Roland Hollos
#' @param daily_observations vector of the daily observations.
#' @return A vector of yearly data
#' @export


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
#' @author Roland Hollos
#' @param daily_observations vector of the daily observations
#' @return A vector of yearly data
#' @usage getyearlymax(daily_observations)
#' @export

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
#' @author Roland Hollos
#' @param filename The string of the filenam
#' @return the extension of the given file
#' @usage fextension(filename)

fextension <- function(filename){
    #this function gives back the given filenames extension
    fextension <- tail(unlist(strsplit(filename,"\\.")),1)
    return(fextension)
}

#'supportedMuso
#'
#' A function for getting the list of the output formats which is supported by RBBGCMuso
#' @author Roland Hollos
#' @param type "outputs" or "message", if you choose "outputs", it gives you a simple vector of the formats, if you choose "message", it gives you a full sentence which contains the same information. 
#' @return if you choose "outputs", it gives you a simple vector of the formats, if you choose "message", it gives you a full sentence which contains the same information.
#' @usage supportedMuso(type="outputs")
#' @export

supportedMuso <- function(type="outputs"){
    supportedFormats <- c("xls","xlsx","odt","csv","txt")
    
    if(type=="outputs"){
        #If you add new format supports, please expand the lists
        return(supportedFormats)
    }
    if(type=="message"){
        return(cat("Supported formats are ",supportedFormats,"If your fileformat is something else, we automaticle coerced it to csv.\n"))
    }
}

#' corrigMuso
#'
#' This function leapyear-corrigate the output of the modell
#' @author Roland Hollos
#' @param settings This is the output of the setupMuso() function. It contains all of the RBBGCMuso settings
#' @param data the models outputdata
#' @return It returns the modells leapyear-corrigated output data.
#' @export
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

#' file.path2
#'
#' It is an extended file.path function, it can concatenate path where the first ends and the second begins with "/", so
#' there wont be two  slash nearby eachother
#' @author Roland Hollos
#' @param str1 This is the first path string
#' @param str2 This is the second path string
#' @return A concatenated path
#' @export
#' @usage file.path2(str1, str2)

file.path2<-function(str1, str2){
    str1<-file.path(dirname(str1),basename(str1))
    if(substring(str2,1,1)=="/"){
        return(paste(str1,str2,sep=""))
    } else{
        return(file.path(str1,str2))
    }
} 

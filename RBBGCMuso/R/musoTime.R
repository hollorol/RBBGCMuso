#' isLeapyear
#'
#'This function tells us if its argument a leapyear or not.
#'
#'@param year a year
#'@usage isLeapyear(year)
#'@return TRUE, if leapyear, FALSE if dont.
#' @keywords internal
isLeapyear <- function(year){
    ##This Boolean function tells us whether the given year is leapyear or not

    if(((year%%4==0)&(year%%100!=0))|(year%%400==0)){
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#' dayOfMonths
#'
#'This function gives as a vector which contains the number of the days per each month
#'
#'@param year a year
#'@param corrigated Do you want to handle the leapyears, if yes choose TRUE
#'@usage dayOfMonths(year, corrigated=TRUE)
#'@return vector with 12 element. First is January, the last is December. All of the vector element represents the number of the days in that specific month
#'@keywords internal



dayOfMonths <- function(year,corrigated=TRUE){
    ##This function tells us how many days are in the months in the choosen year.

    dayMonths <- c(31,28,31,30,31,30,31,31,30,31,30,31)

    if(corrigated){

        if(isLeapyear(year)==TRUE){
            dayMonths[2] <-29
        }
    }

    return(dayMonths)
}



#' This function tells us how many days are in the given year.
#' 
#' This function tells us how many days are in the given year.
#' @author Roland Hollos
#' @keywords internal

dayOfYears <- function(year, corrigated=TRUE){
    ##This function tells us how many days are in the given year.

    if(corrigated){
        if(isLeapyear(year)==TRUE){
            return(366)   
        } else {
            return(365)
        }
    } else {
        return(365)
    }

}

#' How many days are from the given date and given period length(periodlen)? 
#' 
#'How many days are from the given date and given period length(periodlen)? 
#' @author Roland Hollos
#' @keywords internal

sumDaysOfPeriod <- function(year, periodlen, corrigated=TRUE){
    ##How many days are from the given date and given period length(periodlen)?    

    years <- year:(year+periodlen)

    if(corrigated){
        years100 <-length(which(years%%100==0))
        years400 <-length(which(years%%400==0))
        years4 <- length(which(years%%4==0))
        numberOfLeapdays <- years4-years100+years400
        days <- periodlen*365+numberOfLeapdays
        return(days)
    } else {
        days <- periodlen*365
    }
}

#' Musoleapyear
#' 
#' How many days are from the given date and given period length(periodlen)? 
#' @author Roland Hollos
#' @keywords internal

musoLeapYears <- function(settings){
    days <- 365*settings$numyears
    years <- settings$startyear:(settings$startyear+settings$numyears-1)
    Leapyears <-unlist(lapply(years,isLeapyear)) 
    return(Leapyears)
}

#' It generates BiomeBGC-MuSo dates
#' 
#' It generates all of the day-dates which are between the start and endyear of BiomeBGC-MuSo run.
#' How many days are from the given date and given period length(periodlen)? 
#' @author Roland Hollos
#' @param timestep timestep, which can be daily ("d"), monthly ("m"), yearly("y") 
#' @param settings You have to run the setupMuso function before musoDate. It is its output which contains all of the necessary system variables. It sets the whole environment.
#' @param combined If FALSE the output is a vector of 3 string: day, month year, if true, these strings will be concatenated.
#' @param corrigated If True it counts with leapyears, else dont. 
#' @param format This is the format of the date. It can be "en" (dd.mm.yyyy), or "hu" (yyyy.mm.dd)
#' @return The exact date-vectors for the BioBGC-MuSo output. You can use this for labelling purpose for example. 
#' @export

musoDate <- function(settings,timestep="d",combined=TRUE, corrigated=TRUE, format="en"){
    ##purpose: generate date label for muso

    

    
    days <- sumDaysOfPeriod(settings$startyear,settings$numyears, corrigated=corrigated)
    dates <- matrix(rep(NA,days*3),ncol=3)

        dates[1,] <- c(1,1,settings$startyear)    
        for(i in 2:days){
            dates[i,]<-dates[(i-1),]
            if((dates[i-1,2]==12)&(dates[i-1,1]==31)){
                dates[i,] <-c(1,1,(dates[(i-1),3]+1)) 
            } else {
                
                if(dates[i-1,1]==(dayOfMonths(dates[(i-1),3],corrigated=corrigated)[dates[(i-1),2]] )){
                    dates[i,]<-c(1,(dates[(i-1),2]+1),dates[(i-1),3])
                } else {
                    dates[i,1]<-dates[(i-1),1]+1   
                }
            }
            
        }
    if(format=="en"){
        
    } else {
        if(format=="hu"){
         dates<-dates[,c(3,2,1)]   
        } else {
            cat("format is coerced to english, because I don't know what do you mean by:",format)
        }
    }

    if(combined==TRUE){
        dates <- apply(dates,1,function(x) paste(x,collapse="."))
        return(dates)
    }
    
    return(dates)
    
}

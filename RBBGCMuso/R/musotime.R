
isLeapyear <- function(year){

    if(((year%%4==0)&(year%%100!=0))|(year%%400==0)){
        return(TRUE)
    } else {
        return(FALSE)
    }
}

dayOfMonths <- function(year){

    dayMonths <- c(31,28,31,30,31,30,31,31,30,31,30,31)

    if(isLeapyear(year)==TRUE){
        dayMonths[2] <-29
    }

    return(dayMonths)
}

dayOfYears <- function(year){
    
    if(isLeapyear(year)==TRUE){
        return(366)   
    } else {
        return(365)
    }

}

sumDaysOfPeriod <- function(year, periodlen){
    years <- year:(year+periodlen)
    years100 <-length(which(years%%100==0))
    years400 <-length(which(years%%400==0))
    years4 <- length(which(years%%4==0))
    numberOfLeapdays <- years4-years100+years400
    days <- periodlen*365+numberOfLeapdays
    return(days)
}


musoDate <- function(settings,timestep="d",combined=TRUE, corrigate=TRUE){
    
    days <- sumDaysOfPeriod(settings$startyear,settings$numyears)
    dates <- matrix(rep(NA,days*3),ncol=3)
    dates[1,] <- c(settings$startyear,1,1)

    for(i in 2:days){
        dates[i,]<-dates[(i-1),]
        if((dates[i-1,2]==12)&(dates[i-1,3]==31)){
            dates[i,] <-c((dates[(i-1),1]+1),1,1) 
        } else {
            if(dates[i-1,3]==(dayOfMonths(dates[(i-1),1])[dates[(i-1),2]] )){
                dates[i,]<-c(dates[(i-1),1],(dates[(i-1),2]+1),1)
            } else {
                dates[i,3]<-dates[(i-1),3]+1   
            }
        }
        
    }

    if(corrigate==TRUE){
        if(comined==TRUE){
            dates <- apply(dates,1,function(x) paste(x,collapse="."))[1:(settings$numyears*365)]
            return(dates)
        }
        
        dates<-dates[(1:(settings$numyears*365)),]
        
    }
    
    if(comined==TRUE){
        dates <- apply(dates,1,function(x) paste(x,collapse="."))
        return(dates)
    }
    
    return(dates)
    
}

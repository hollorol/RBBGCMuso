#'plot the BBGCMuso output 
#'
#' This function runs the BBGC-MuSo model and reads in its outputfile in a very structured way, and after that plot the results automaticly 
#' 
#' @author Roland Hollos, Dora Hidy
#' @param settings You have to run the setupMuso function before rungetMuso. It is its output which contains all of the necessary system variables. It sets the whole environment
#' @param timee The required timesteps in the modell output. It can be "d", if it is daily, "m", if it's monthly, "y", it it is yearly
#' @param debugging If it is TRUE, it copies the log file to a Log directory to store it, if it is stamplog it contatenate a number before the logfile, which is one more than the maximum of the represented ones in the LOG directory. If it is true or stamplog it collects the "wrong" logfiles
#' @param keepEpc If TRUE, it keeps the epc file and stamp it, after these copies it to the EPCS directory. If debugging True or false, it copies the wrong epc files to the wrong epc directory.
#' @param export if it is yes or you give a filename here, it converts the output to the specific extension. For example, if you set export to "example.csv", it converts the output to "csv", if you set it to "example.xls" it converts to example.xls with the xlsx package. If it is not installed it gives back a warning message and converts it to csv.
#' @param silent If you set it TRUE all off the modells output to the screen will be suppressed. It can be usefull, because it increases the model-speed.
#' @param aggressive It deletes every possible modell-outputs from the previous modell runs.
#' @param variable column number of the variable which should be plottedor "all" if you have less than 10 variables. In this case it will plot everything in a matrix layout 
#' @param leapYear Should the function do a leapyear correction on the outputdata? If TRUE, then the 31.12 day will be doubled.
#' @param logfilename If you want to set a specific name for your logfiles you can set this via logfile parameter
#' @return It depends on the export parameter. The function returns with a matrix with the modell output, or writes this in a file, which is given previously
#' @usage plotMuso(settings, variable,
#' timee="d", silent=TRUE,
#' debugging=FALSE, keepEpc=FALSE,
#' logfilename=NULL, aggressive=FALSE,
#' leapYear=FALSE, export=FALSE)
#' @import graphics
#' @export

plotMuso <- function(settings=NULL,
                     variable=1,
                     ##compare,
                     ##plotname,
                     timee="d",
                     silent=TRUE,
                     debugging=FALSE,
                     keepEpc=FALSE,
                     logfilename=NULL,
                     aggressive=FALSE,
                     leapYear=FALSE,
                     export=FALSE){


    if(is.null(settings)){
        settings <- setupMuso()
    }
    
    musoData <- rungetMuso(settings=settings,
                           silent=silent,
                           timee=timee,
                           debugging=debugging,
                           keepEpc=keepEpc,
                           logfilename=logfilename,
                           export=export) 

     xlab_muso<- switch(timee, "d"="days","y"="years","m"="months")
     numVari <- ncol(musoData)
    
     if(is.numeric(variable)){
     if((variable>numVari)|(variable<1)){
         return(print(paste("The variable parameter must be between 1 and ",numVari)))
     }


     plot(musoData[,variable],pch=20,col = "dark blue",xlab=xlab_muso,ylab=colnames(musoData)[variable])
     } else {
         if(variable=="all"){
            
             musoData  <- rbind(1:numVari,musoData) #creating the column indexes
             par(mfrow = niceMatrixLayoutForPlots(numVari))

             apply(musoData, 2, function(x) plot(x[2:length(x)],pch=20,col="dark blue",xlab=xlab_muso,ylab = colnames(musoData)[x[1]]))
             par(mfrow=c(1,1))
             return(print("Everything was Ok. ;)"))
         } else {
             return(print("The variable option is the coloumn number of the output data-matrix, so it must be numeric, of if you want to plot the whole data matrix set it \"all\""))
         }

            
        
     }
    
}

#'plot the BBGCMuso output with data 
#'
#' This function runs the BBGC-MuSo model and reads in its outputfile in a very structured way, and after that plot the results automaticly along with a given measurement 
#' 
#' @author Roland Hollos, Dora Hidy
#' @param settings You have to run the setupMuso function before rungetMuso. It is its output which contains all of the necessary system variables. It sets the whole environment
#' @param sep This is the separator used in the measurement file
#' @param savePlot It it is specified, the plot will be saved in a format specified with the immanent extension
#' @param variable The name of the output variable to plot
#' @param NACHAR This is not implemented yet
#' @param csvFile The file of the measurement. It must contain a header.
#' @usage plotMuso(settings, variable,
#' timee="d", silent=TRUE,
#' debugging=FALSE, keepEpc=FALSE,
#' logfilename=NULL, aggressive=FALSE,
#' leapYear=FALSE, export=FALSE)
#' @import ggplot2
#' @export
plotMusoWithData <- function(csvFile, variable, NACHAR=NA, settings=NULL, sep=",", savePlot=NULL){
    if(!is.na(NACHAR)){
        warning("NACHAR is not implemented yet")
    }
    if(is.null(settings)){
        settings <- setupMuso()
    }
    
    numberOfYears <- settings$numYears
    startYear <- settings$startYear
    yearVec <- seq(from = startYear, length=numberOfYears,by=1)

    
    data <- read.table(csvFile,header = TRUE, sep = ",") %>%
        select(variable)
    
    baseData <- calibMuso(settings,silent=TRUE) %>%
        as.data.frame() %>%
        tibble::rownames_to_column("date") %>%
        dplyr::mutate(date2=date,date=as.Date(date,"%d.%m.%Y"),yearDay=rep(1:365,numberOfYears)) %>%
        tidyr::separate(date2,c("day","month","year"),sep="\\.")
    baseData <- cbind(baseData,data)
    colnames(baseData)[ncol(baseData)] <- "measuredData"

    p <- baseData %>%
        ggplot(aes_string("date",variable)) +
        geom_line(colour = "blue") +
        geom_point(aes(date,measuredData)) +
        labs(y = paste0(variable,"_measured"))+
        theme(axis.title.x = element_blank())
    if(!is.null(savePlot)){
        return(p)
    } else {
        ggsave(savePlot,p)
        return(p)
    }
    
}


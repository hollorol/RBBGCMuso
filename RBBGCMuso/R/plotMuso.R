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
#' @param plotType There are two options now: continious time series("cts")  or disctrete time series("dts")
#' @param skipSpinup If TRUE, calibMuso wont do spinup simulation
#' @return It depends on the export parameter. The function returns with a matrix with the modell output, or writes this in a file, which is given previously
#' @usage plotMuso(settings, variable,
#' timee="d", silent=TRUE,
#' debugging=FALSE, keepEpc=FALSE,
#' logfilename=NULL, aggressive=FALSE,
#' leapYear=FALSE, export=FALSE)
#' @import ggplot2, dplyr
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
                     plotName=NULL,
                     plotType="cts",
                     layerPlot=FALSE,
                     colour="blue",
                     skipSpinup=TRUE,
                     fromData=FALSE,
                     dpi=300){

    if( plotType!="cts" && plotType != "dts"){
        warning(paste0("The plotType ", plotType," is not implemented, plotType is set to cts"))
        plotType <- "cts"
    }
    
    if(is.null(settings)){
        settings <- setupMuso()
    }
    
    numberOfYears <- settings$numYears
    startYear <- settings$startYear
    ## musoData <- rungetMuso(settings=settings,
    ##                        silent=silent,
    ##                        timee=timee,
    ##                        debugging=debugging,
    ##                        keepEpc=keepEpc,
    ##                        logfilename=logfilename,
    ##                        export=export)
    if(fromData){
        Reva <- tryCatch(getdailyout(settings), #(:INSIDE: getOutput.R )
                                    error = function (e){
                                        setwd((whereAmI))
                                        stop("Cannot read binary output, please check if the output type is set 2 in the ini files!")})
        colnames(Reva) <- unlist(settings$outputVars[[1]])
        rownames(Reva) <- NULL
        musoData <- cbind(musoDate(startYear = startYear,numYears = numberOfYears,combined = TRUE,corrigated=FALSE),
              rep(1:365,numberOfYears),
              musoDate(startYear = startYear,numYears = numberOfYears,combined = FALSE,corrigated=FALSE),as.data.frame(Reva))
        colnames(musoData)[1:5]<-c("date","yearDay","year","day","month")
        musoData <-musoData %>%
            mutate(date=as.Date(as.character(date),"%d.%m.%Y"))
    } else {
            musoData <- calibMuso(settings,silent = TRUE,skipSpinup=skipSpinup) %>%
                as.data.frame() %>%
                tibble::rownames_to_column("date") %>%
                mutate(date2=date,date=as.Date(date,"%d.%m.%Y"),yearDay=rep(1:365,numberOfYears)) %>%
                tidyr::separate(date2,c("day","month","year"),sep="\\.")
    }

    ## numVari <- ncol(musoData)
     numVari <- ncol(musoData)-5

    pointOrLineOrPlot <- function(musoData, variableName, plotType="cts", expandPlot=FALSE, plotName=NULL){
        if(!expandPlot){
            if(plotType=="cts"){
                if(length(variableName)==1){
                   p <- ggplot(musoData,aes_string("date",variableName))+geom_line(colour=colour)+theme(axis.title.x=element_blank())
                   if(!is.null(plotName)){
                       ggsave(as.character(plotName), plot = p)
                    p
                   }
                   p
                } else {
                    p <- baseData %>%
                        select(c("date", variableName))%>%
                        gather(., key= outputs, value = bla, variableName) %>%
                                        # head  %>%
                        ggplot(aes(x=date,y=bla))+
                        facet_wrap(~ outputs, scales = "free_y",ncol=1) +
                        geom_line(colour=colour)+
                        theme(
                            axis.title.y = element_blank()
                        )
                    if(!is.null(plotName)){
                        ggsave(as.character(plotName), plot = p)
                    }
                    p
                }
            } else {
                if(length(variableName)==1){
                    p <- ggplot(musoData,aes_string("date",variableName))+geom_point(colour=colour)+theme(axis.title.x=element_blank())
                    if(!is.null(plotName)){
                        ggsave(as.character(plotName),p)
                    }
                    p
                } else{
                    p <- baseData %>%
                        select(c("date",variableName))%>%
                        gather(., key= outputs, value = bla,variableName) %>%
                                        # head  %>%
                        ggplot(aes(x=date,y=bla))+
                        facet_wrap(~ outputs, scales = "free_y",ncol=1) +
                        geom_line(colour=colour)+
                        theme(
                            axis.title.y = element_blank()
                        )
                    if(!is.null(plotName)){
                        ggsave(as.character(plotName),p)
                    }
                    p
                }
            }
        } else {
            if(!is.null(plotName)){
                stop("Cannot save a single plot layer to a graphics device")
            }
            
            if(plotType=="cts"){
                if(length(variableName)==1){
                    geom_line(colour=colour, aes_string("date",variableName))
                    
                } else {
                    stop("you cannot add layers for multiple plots")
                }
            } else {
                if(length(variableName)==1){
                    geom_point(colour=colour, aes_string("date",variableName))
                } else{
                    stop("you cannot add layers for multiple plots")
                }
            }
            
        }
        
    }
    


    variableName <-  as.character(settings$outputVars[[1]])[variable]
    if(is.character(variable)){
        if(identical(variable,"all")){
            variableName <- as.character(settings$outputVars[[1]])
        } else {
            if(identical(character(0),setdiff(variable,as.character(settings$outputVars[[1]])))){
                variableName <- variable
            } else {
                stop("The symmetric difference of the set of the output variables specified in the ini files and the set specified with your variable parameter is not the empty set.")
            }   
        }
        
        if(length(variableName)>8){
            warning("Too many variables to plot, the output quality can be poor")
        }
        
     } else {
         
         if(prod(sapply(variable,function(x){
             return(x >= 0 && x <= numVari)
         }))){
             variableName <-  as.character(settings$outputVars[[1]])[variable]
         } else {
             stop("Not all members of the variable parameter are among the output variables")
         }}
    
    pointOrLineOrPlot(musoData = musoData,
                      variableName = variableName,
                      plotType = plotType,
                      expandPlot = layerPlot,
                      plotName = plotName)
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
plotMusoWithData <- function(csvFile, variable, NACHAR=NA, settings=NULL, sep=",", savePlot=NULL,colour=c("black","blue")){
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
        geom_line(colour=colour[1]) +
        geom_point(colour=colour[2], aes(date,measuredData)) +
        labs(y = paste0(variable,"_measured"))+
        theme(axis.title.x = element_blank())
    if(!is.null(savePlot)){
        ggsave(savePlot,p)
        return(p)
    } else {
        return(p)
    }
    
}


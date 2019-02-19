#'plot the Biome-BGCMuSo output 
#'
#' This function runs the Biome-BGCMuSo model and reads its output file in a well structured way, and after that it plots the results automatically. plotMuso is a convenient and quick method to create nice graphs from Biome-BGCMuSo output which is quite painful in other environments. 
#' 
#' @author Roland HOLLOS, Dora HIDY
#' @param settings RBBGCMuso uses variables that define the entire simulation environment. Those environment variables include the name of the INI files, the name of the meteorology files, the path to the model executable and its file name, the entire output list, the entire output variable matrix, the dependency rules for the EPC parameters etc. Using the runMuso function RBBGCMuso can automatically create those environment variables by inspecting the files in the working directory (this happens through the setupMuso function). It means that by default model setup is performed automatically in the background and the user has nothing to do. With this settings parameter we can force runMuso to skip automatic environment setup as we provide the environment settings to runMuso. In a typical situation the user can skip this option.
#' @param timee The required timesteps in the model output. It can be "d", if it is daily, "m", if it is monthly, "y" if it is yearly. It is recommended to use daily data, as the yearly and monthly data is not well-tested yet.
#' @param debugging If debugging is set to TRUE, after model execution the function copies the Biome-BGCMuSo log file into a LOG directory and stores it for further processing. If debugging is set to STAMPLOG instead of TRUE, it concatenates a number before the logfile, which is one plus the maximum of those present in the LOG directory. In each case the log files will be saved. 
#' @param keepEpc If keepEpc is set to TRUE, the function keeps the EPC file and stamps it, and then copies it to the EPCS directory. If debugging is set to TRUE, it copies the wrong EPC files to the wrong epc directory.
#' @param export If it is set to YES or you define a filename here, the function converts the output to the specific file format. For example, if you set export to "example.csv", it converts the output to "csv". If you set it to "example.xls" it converts the output to example.xls with the xlsx package. If the Excel converter package is not installed it gives back a warning message and converts the results to csv.
#' @param silent If you set the silent parameter to TRUE, all of the model's output normally written to the screen will be suppressed. This option can be useful to increase the speed of the model execution.
#' @param aggressive It deletes all previous model-outputs from previous model runs.
#' @param variable Column number of the output variable which should be plotted, or "all" if you have less than 10 variables. In this case the function will plot everything in a matrix layout. 
#' @param leapYear Should the function do a leapyear correction on the output data? If TRUE, then the result for 31 December will be doubled in leap years which means that the results for the leap year will cover all 366 days. See the model's User's Guide for notes on leap years. 
#' @param logfilename If you would like to set a specific name for your logfiles you can set this via the logfile parameter.
#' @param plotType There are two options implemented by now: continuous time series ("cts") or disctrete time series ("dts")
#' @param skipSpinup If TRUE, the function won't perform the spinup simulation. In this case the endpoint file must exist that provides initial conditions for the run. 
#' @return It depends on the export parameter. The function returns with a matrix with the model output, or writes this into a file, which is defined previously
#' @usage plotMuso(settings, variable,
#' timee="d", silent=TRUE,
#' debugging=FALSE, keepEpc=FALSE,
#' logfilename=NULL, aggressive=FALSE,
#' leapYear=FALSE, export=FALSE)
#' @importFrom ggplot2 ggplot aes_string geom_line geom_point aes labs theme ggsave element_blank facet_wrap
#' @importFrom dplyr filter group_by summarize mutate '%>%'
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr separate gather
#' @export

plotMuso <- function(settings = NULL, variable = 1,
                     ##compare, ##plotname,
                     timee = "d", silent = TRUE,
                     calibrationPar = NULL, parameters = NULL,
                     debugging = FALSE, keepEpc = FALSE,
                     fileToChange = "epc", logfilename = NULL,
                     aggressive = FALSE, leapYear = FALSE,
                     plotName = NULL, plotType = "cts",
                     layerPlot = FALSE, colour = "blue",
                     skipSpinup = TRUE, fromData = FALSE,
                    timeFrame = "day", selectYear = NULL,
                    groupFun = mean, separateFile = FALSE, dpi=300){

    if( plotType!="cts" && plotType != "dts"){
        warning(paste0("The plotType ", plotType," is not implemented, plotType is set to cts"))
        plotType <- "cts"
    }
    
    if(is.null(settings)){
        settings <- setupMuso()
    }
    
    numberOfYears <- settings$numYears
    startYear <- settings$startYear
    dailyVarCodes <- settings$dailyVarCodes
    ## musoData <- rungetMuso(settings=settings,
    ##                        silent=silent,
    ##                        timee=timee,
    ##                        debugging=debugging,
    ##                        keepEpc=keepEpc,
    ##                        logfilename=logfilename,
    ##                        export=export)
    
    groupByTimeFrame <- function(data, timeFrame, groupFun){
        datas <- data %>%
            group_by(eval(parse(text=timeFrame))) %>%
            summarize(variable=groupFun(eval(parse(text=variable))))
        datas[,1]<-as.numeric(unlist(datas[,1]))
        colnames(datas) <- c("date",variable)
        datas
    }
    
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
        if(!is.element("cum_yieldC_HRV",unlist(settings$outputVars[[1]]))){
            musoData <- calibMuso(settings,silent = TRUE,skipSpinup=skipSpinup) %>%
                as.data.frame() %>%
                rownames_to_column("date") %>%
                mutate(date2=date,date=as.Date(date,"%d.%m.%Y")) %>%
                separate(date2,c("day","month","year"),sep="\\.")
            if(!is.null(selectYear)){
            musoData <- musoData %>% filter(year == get("selectYear"))    
            }
            
            if(timeFrame!="day"){
                musoData <- tryCatch(groupByTimeFrame(data=musoData, timeFrame = timeFrame, groupFun = groupFun),
                                     error=function(e){stop("The timeFrame or the gropFun is not found")})
            }} else {
                 musoData <- calibMuso(settings,silent = TRUE,skipSpinup=skipSpinup,parameters = parameters, calibrationPar = calibrationPar,fileToChange = fileToChange) %>%
                     as.data.frame() %>%
                     rownames_to_column("date") %>%
                     mutate(date2=date,date=as.Date(date,"%d.%m.%Y"),
                            yearDay=rep(1:365,numberOfYears), cum_yieldC_HRV=cum_yieldC_HRV*22.22) %>%
                     separate(date2,c("day","month","year"),sep="\\.")
                 if(!is.null(selectYear)){
                     musoData <- musoData %>% filter(year == get("selectYear"))    
                 }
                 
                 
                 if(timeFrame!="day"){
                     musoData <- tryCatch(groupByTimeFrame(data=musoData, timeFrame = timeFrame, groupFun = groupFun),
                                          error=function(e){stop("The timeframe or the gropFun is not found")})
                 }
                 
             }
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
                    p <- musoData %>%
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
                        p <- musoData %>%
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
                    geom_line(data=musoData, colour=colour, aes_string("date",variableName))
                    
                } else {
                    stop("you cannot add layers for multiple plots")
                }
            } else {
                if(length(variableName)==1){
                    geom_point(data=musoData, colour=colour, aes_string("date",variableName))
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

#'plot the Biome-BGCMuSo model output with observation data 
#'
#' This function runs the Biome-BGCMuSo model and reads its output file in a well structured way, and after that it plots the results automatically along with a given measurement dataset provided by the user. plotMusoWithData is a convenient and quick method to create nice graphs from Biome-BGCMuSo output which is quite painful in other environments.  
#' 
#' @author Roland HOLLOS, Dora HIDY
#' @param settings RBBGCMuso uses variables that define the entire simulation environment. Those environment variables include the name of the INI files, the name of the meteorology files, the path to the model executable and its file name, the entire output list, the entire output variable matrix, the dependency rules for the EPC parameters etc. Using the runMuso function RBBGCMuso can automatically create those environment variables by inspecting the files in the working directory (this happens through the setupMuso function). It means that by default model setup is performed automatically in the background and the user has nothing to do. With this settings parameter we can force runMuso to skip automatic environment setup as we provide the environment settings to runMuso. In a typical situation the user can skip this option.
#' @param sep This is the separator symbol used in the measurement file (that is supposed to be a delimited text file)
#' @param savePlot It it is specified, the plot will be saved in a graphical format specified by the immanent extension. For example, it the savePlot is set to image01.png then a PNG graphics file will be created. 
#' @param variable The name of the output variable to plot
#' @param NACHAR This is not implemented yet
#' @param csvFile This specifies the filename of the measurements. It must contain a header. Typically this is a CSV file.
#' @param calibrationPar You might want to change some parameters in your EPC file before running the model. The function offers possibility for this without editing the EPC file. In this situation you have to select the appropirate model parameters first. You can refer to these parameters with the number of the line in the EPC file. Indexing of lines start from one. You should use a vector for this referencing like c(1,5,8)
#' @param parameters Using the function it is possible to change some of the EPC parameters prior to model execution. This can be achieved with this option. In the parameters variable you have set the row indices of the variables that you wish to change. In this parameters you can give an exact value for them in a vector form like c(1,2,3,4).
#' @usage plotMuso(settings, variable,
#' timee="d", silent=TRUE,
#' debugging=FALSE, keepEpc=FALSE,
#' logfilename=NULL, aggressive=FALSE,
#' leapYear=FALSE, export=FALSE)
#' @importFrom ggplot2 ggplot geom_line geom_point aes aes_string labs theme element_blank 
#' @export
plotMusoWithData <- function(mdata, plotName=NULL,
                             startDate, endDate,
                             colour=c("black","blue"),dataVar, modelVar, settings = setupMuso(), silent = TRUE, continious = TRUE){

    dataCol<- grep(paste0("^",dataVar,"$"), colnames(mdata))
    selVar <- grep(modelVar,(settings$dailyVarCodes))+4

    list2env(alignData(mdata, dataCol = dataCol,
                       modellSettings = settings,
                       startDate = startDate,
                       endDate = endDate, leapYear = FALSE, continious = continious),envir=environment())
    
    
    ## measuredData is created
    baseData <- calibMuso(settings = settings, silent = silent, prettyOut = TRUE)[modIndex,]
    baseData[,1] <- as.Date(baseData[,1],format = "%d.%m.%Y")
    selVarName <- colnames(baseData)[selVar]
    if(!all.equal(colnames(baseData),unique(colnames(baseData)))){
        notUnique <- setdiff((unlist(settings$dailyVarCodes)),unique(unlist(settings$dailyVarCodes)))
        stop(paste0("Error: daily output variable list in the ini file must contain unique numbers. Check your ini files! Not unique codes: ",notUnique))
    }
    
    p <- baseData  %>%
        ggplot(aes_string("date",selVarName)) +
        geom_line(colour=colour[1]) +
        geom_point(colour=colour[2], aes(date,measuredData)) +
        labs(y = paste0(selVarName,"_measured"))+
        theme(axis.title.x = element_blank())
    if(!is.null(plotName)){
        ggsave(plotName,p)
        return(p)
    } else {
        return(p)
    }
    
}

#' compareMuso 
#'
#' This function runs the model, then changes one of its input data, runs it again, and plots both results in one graph. 
#' 
#' @author Roland HOLLOS
#' @param settings RBBGCMuso uses variables that define the entire simulation environment. Those environment variables include the name of the INI files, the name of the meteorology files, the path to the model executable and its file name, the entire output list, the entire output variable matrix, the dependency rules for the EPC parameters etc. Using the runMuso function RBBGCMuso can automatically create those environment variables by inspecting the files in the working directory (this happens through the setupMuso function). It means that by default model setup is performed automatically in the background and the user has nothing to do. With this settings parameter we can force runMuso to skip automatic environment setup as we provide the environment settings to runMuso. In a typical situation the user can skip this option.
#' @param parameters Using this function it is possible to change some of the EPC parameters prior to model execution. This can be achieved with this option. In the parameters variable you have set the row indices of the variables that you wish to change. In this parameters you can give an exact value for them in a vector form like c(1,2,3,4).
#' @param variable The name of the output variable to plot
#' @param calibrationPar You might want to change some parameters in your EPC file before running the model. This function offers possibility for this without editing the EPC file. In this situation you have to select the appropirate model parameters first. You can refer to these parameters with the number of the line in the EPC file. Indexing of lines start from one. You should use a vector for this referencing like c(1,5,8)
#' @param fileToChange You can change any line of the EPC or the INI file. Please choose "EPC", "INI" or "BOTH". This file will be used for the analysis, and the original parameter values will be changed according to the choice of the user. 
#' @import ggplot2
#' @export
compareMuso <- function(settings=NULL,parameters, variable=1, calibrationPar=NULL, fileToChange="epc", skipSpinup=TRUE, timeFrame="day"){

    if(is.null(settings)){
        settings <- setupMuso()
    }
    

    p1 <- plotMuso(settings = settings,variable = variable,timeFrame = timeFrame)
    p2 <- p1+plotMuso(settings = settings,variable = variable, timeFrame = timeFrame,fileToChange=fileToChange,layerPlot=TRUE)
    p2
    
}

#' saveAllMusoPlots 
#'
#' This simple function takes the parameters from the ini files and generates graphics for all output variable. 
#' 
#' @author Roland HOLLOS
#' @param settings RBBGCMuso uses variables that define the entire simulation environment. Those environment variables include the name of the INI files, the name of the meteorology files, the path to the model executable and its file name, the entire output list, the entire output variable matrix, the dependency rules for the EPC parameters etc. Using the runMuso function RBBGCMuso can automatically create those environment variables by inspecting the files in the working directory (this happens through the setupMuso function). It means that by default model setup is performed automatically in the background and the user has nothing to do. With this settings parameter we can force runMuso to skip automatic environment setup as we provide the environment settings to runMuso. In a typical situation the user can skip this option.
#' @param plotName The basename for the output plots
#' @param destination The destination for the output plots, it not exits the  function will create it.
#' @param silent if true do not suspect for printfs... 
#' @importFrom ggplot2 theme_classic ggplot geom_line geom_point theme element_blank geom_bar labs aes_string aes ggsave 
#' @export


saveAllMusoPlots <- function(settings=NULL, plotName = ".png",
                             silent = TRUE, type = "line", outFile = "annual.csv",
                             colour = NULL, skipSpinup = FALSE){

    if(is.null(settings)){
        settings <- setupMuso()
    }
    
    dailyVarCodes <- settings$dailyVarCodes
    annualVarCodes <-settings$annualVarCodes
    outputVars <- unlist(settings$outputVars[[1]])
    musoData <- calibMuso(settings = settings, prettyOut = TRUE, silent = silent, skipSpinup = skipSpinup)
    for(i in seq_along(dailyVarCodes)){
        bases <- ggplot(data = musoData, mapping = aes_string(x = "date", y = outputVars[i]))
        object <-ifelse(type == "line",paste0("geom_line(colour = '",colour,"')"),
                                  ifelse(type == "point",paste0("geom_line(colour = ",colour,")"),
                                         stop("The")))
        outPlot <- bases + eval(parse(text = object)) + theme_classic() + theme(axis.title.x=element_blank())
        ggsave(paste0("daily-",dailyVarCodes[i],plotName),outPlot)
    }
    
    musoYData <- getyearlyout(settings)
    write.csv(musoYData,paste0(settings$outputNames[[2]],outFile))
     for(i in seq_along(annualVarCodes)){
        outPlot <- ggplot(data = musoYData, mapping = aes_string(x = "year", y = paste0("var_",annualVarCodes[i])))+
            geom_bar(stat = "identity")+ labs(y = musoMapping(annualVarCodes[i])) + theme_classic() +
            theme(axis.title.x=element_blank())
        ggsave(paste0("annual-",annualVarCodes[i],plotName),outPlot)
     }   
    
}

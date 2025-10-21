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
#' @importFrom data.table ':=' data.table
#' @export

plotMuso <- function(settings = NULL, variable = "all",
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
                    groupFun = mean, separateFile = FALSE, dpi=300, postProcString = NULL){

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
    groupByTimeFrame <- function(Data, timeFrame, groupFun){
        Data <- data.table(Data)
        Data[,c(variable):=groupFun(get(variable)),get(timeFrame)]
        Data <- as.data.frame(Data) 
        Data[,1] <- as.Date(Data[,1],"%d.%m.%Y")
        Data
    }
    
    if(fromData){
        Reva <- tryCatch(getdailyout(settings), #(:INSIDE: getOutput.R )
                                    error = function (e){
                                        setwd((whereAmI))
                                        stop("Cannot read binary output, please check if the output type is set 2 in the ini files!")})
        colnames(Reva) <- unlist(settings$outputVars[[1]])
        rownames(Reva) <- NULL
        musoData <- cbind(musoDate(startYear = startYear,numYears = numberOfYears,combined = TRUE),
              rep(1:365,numberOfYears),
              musoDate(startYear = startYear,numYears = numberOfYears,combined = FALSE),as.data.frame(Reva))
        colnames(musoData)[1:5]<-c("date","yearDay","year","day","month")
        musoData <-musoData %>%
            mutate(date=as.Date(as.character(date),"%d.%m.%Y"))
    } else {
        if(!is.element("cum_yieldC_HRV",unlist(settings$outputVars[[1]]))){
            musoData <- calibMuso(postProcString = postProcString,settings,
                                  calibrationPar=calibrationPar,
                                  parameters = parameters,
                                  silent = TRUE,skipSpinup=skipSpinup,prettyOut = TRUE)
            if(!is.null(selectYear)){
            musoData <- musoData %>% filter(year == get("selectYear"))    
            }
            
            if(timeFrame!="day"){
                musoData <- tryCatch(groupByTimeFrame(Data=musoData, timeFrame = timeFrame, groupFun = groupFun),
                                     error=function(e){stop("The timeFrame or the groupFun is not found")})
            }} else {
                 musoData <- calibMuso(postProcString = postProcString,settings,silent = TRUE,skipSpinup=skipSpinup,parameters = parameters, calibrationPar = calibrationPar,fileToChange = fileToChange) %>%
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
     # numVari <- ncol(musoData)-5
     numVari <- length(settings$dailyVarCodes)

    pointOrLineOrPlot <- function(musoData, variableName, plotType="cts", expandPlot=FALSE, plotName=NULL){
        if(!inherits(musoData$date[1], "Date")){
            musoData$date<- as.Date(as.character(musoData$date),"%d.%m.%Y")
        }
        if(!expandPlot){
            if(plotType=="cts"){
                if(length(variableName)==1){
                   p <- ggplot(musoData,aes_string("date",variableName,group=1))+geom_line(colour=colour)+theme(axis.title.x=element_blank())
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
    if(variable == "all"){
        variableName <-  as.character(settings$outputVars[[1]])
    }
    if(is.character(variable)){


        if(identical(variable,"all")){
            variable <- as.character(settings$outputVars[[1]])
            
        } else {

            if(is.element(variable, settings$dailyVarCodes)){
                variable <- settings$outputVars[[1]][match(variable,settings$dailyVarCodes)]
            }

            if(identical(character(0),setdiff(variable,as.character(settings$outputVars[[1]])))){
                variableName <- variable
            } else {
                if(!is.null(postProcString)){
                 variableName <- variable     
                } else {
                    stop("The symmetric difference of the set of the output variables specified in the ini files and the set specified with your variable parameter is not the empty set.")                    
                }

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
             print(numVari)
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
plotMusoWithData <- function(mdata, plotName = NULL,
                                     startDate = NULL, endDate = NULL,
                                     colour = c("black", "blue"),
                                     dataVar,  # Name of the variable in mdata to plot
                                     modelVar, # Name of the variable in model output (can be custom)
                                     settings = setupMuso(),
                                     silent = TRUE,
                                     continious = FALSE,
                                     leapYearHandling = FALSE,
                                     skipSpinup = TRUE,
                                     postProcString = NULL
                                     ) {

    if (continious & (is.null(startDate) | is.null(endDate))) {
        stop("If your date is continuous, you have to provide both startDate and endDate.")
    }

   
    dataCol_indices <- grep(paste0("^", dataVar, "$"), colnames(mdata)) # Exact match
    if (length(dataCol_indices) == 0) {
        stop(paste("Measured variable '", dataVar, "' not found in mdata. Available: ", paste(colnames(mdata), collapse=", "), sep = ""))
    }
    dataCol <- dataCol_indices[1]

    alignment_result <- alignData(mdata, dataCol = dataCol, modellSettings = settings,
                                  startDate = startDate, endDate = endDate,
                                  leapYear = leapYearHandling, continious = continious)
    aligned_measured_values <- alignment_result$measuredData
    model_indices_for_measurements <- alignment_result$modIndex


    
    baseData_from_calibMuso <- calibMuso(settings = settings, silent = silent, prettyOut = FALSE, skipSpinup = skipSpinup,
                                         postProcString = postProcString,
                                         leapYearHandling = leapYearHandling)

    if (is.null(baseData_from_calibMuso) || nrow(baseData_from_calibMuso) == 0) stop("calibMuso returned no data.")

   
    model_data_for_lineplot <- as.data.frame(baseData_from_calibMuso)


    if (length(rownames(model_data_for_lineplot)) == nrow(model_data_for_lineplot)) {
        date_formats_to_try <- c("%d.%m.%Y", "%Y.%m.%d", "%Y-%m-%d") # Common formats
        parsed_dates_full <- NULL
        for (fmt in date_formats_to_try) {
            parsed_dates_full <- tryCatch({ as.Date(rownames(model_data_for_lineplot), format = fmt) }, warning = function(w) NULL, error = function(e) NULL)
            if (!is.null(parsed_dates_full) && !all(is.na(parsed_dates_full))) break
        }
        if (is.null(parsed_dates_full) || all(is.na(parsed_dates_full))) {
             parsed_dates_full <- tryCatch({ as.Date(rownames(model_data_for_lineplot)) }, error = function(e) { stop(paste("Could not parse date from rownames. Example:", rownames(model_data_for_lineplot)[1]), call. = FALSE) })
        }
        if (any(is.na(parsed_dates_full))) warning("Some dates for model_data_for_lineplot could not be parsed.", call. = FALSE)
        model_data_for_lineplot$plot_date <- parsed_dates_full # Assign to the data frame
    } else {
        stop("Rownames for date conversion are missing or have incorrect length in model_data_for_lineplot.")
    }

    
    selVarName <- NULL
    if (modelVar %in% colnames(model_data_for_lineplot)) {
        selVarName <- modelVar
    } else {
        
        var_names_from_settings <- unlist(settings$outputVars[[1]]) # For daily
        
        actual_col_index <- NA
        # Try if modelVar is a code
        modelVar_as_code <- suppressWarnings(as.numeric(modelVar))
        if (!is.na(modelVar_as_code)) {
            idx <- match(modelVar_as_code, settings$dailyVarCodes)
            if(!is.na(idx) && idx > 0 && idx <= length(var_names_from_settings)) actual_col_index <- idx
        }
        # Try if modelVar is a name (if not already found as a direct colname)
        if (is.na(actual_col_index) && is.character(modelVar)) {
             idx <- match(modelVar, var_names_from_settings)
             if(!is.na(idx) && idx > 0) actual_col_index <- idx
        }

        if (!is.na(actual_col_index) && actual_col_index <= ncol(model_data_for_lineplot)) {
            selVarName <- colnames(model_data_for_lineplot)[actual_col_index]
        }
    }

    if (is.null(selVarName)) {
        stop(paste("Model variable '", modelVar,
                   "' could not be identified in model output. Available columns: '",
                   paste(colnames(model_data_for_lineplot), collapse = "', '"), # Use the correct data frame here
                   "'. Check name or settings$dailyVarCodes.", sep = ""))
    }


    plot_mesData <- data.frame(plot_date = as.Date(character(0)), measured = numeric(0)) 
    if (length(model_indices_for_measurements) > 0 && length(model_indices_for_measurements) == length(aligned_measured_values)) {
        # Ensure plot_date column exists in model_data_for_lineplot before subsetting
        if("plot_date" %in% colnames(model_data_for_lineplot)) {
             plot_mesData <- data.frame(
                plot_date = model_data_for_lineplot$plot_date[model_indices_for_measurements],
                measured = aligned_measured_values
            )
        } else {
            warning("plot_date column missing from model_data_for_lineplot when preparing measured points.", call.=FALSE)
        }
    } else if (length(model_indices_for_measurements) > 0) {
        warning("Mismatch between length of model_indices_for_measurements and aligned_measured_values.", call.=FALSE)
    }


   
    p <- ggplot(model_data_for_lineplot, aes_string(x = "plot_date", y = selVarName)) +
        geom_line(colour = colour[1], na.rm = TRUE)
    
    if(nrow(plot_mesData) > 0){ 
        p <- p + geom_point(data = plot_mesData, aes(x = plot_date, y = measured), colour = colour[2], na.rm = TRUE)
    }
    
    p <- p + labs(y = paste0("Model: ", selVarName, " / Measured: ", dataVar), x = "Date") +
        theme(axis.title.x = element_text())

    if (!is.null(plotName)) ggsave(plotName, p)
    return(p)
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
                             colour = "blue", skipSpinup = FALSE){

    if(is.null(settings)){
        settings <- setupMuso()
    }

    dailyVarCodes <- settings$dailyVarCodes
    annualVarCodes <-settings$annualVarCodes
    outputVars <- unlist(settings$outputVars[[1]])
    musoData <- calibMuso(settings = settings, prettyOut = TRUE, silent = silent, skipSpinup = skipSpinup)
    musoData$date<- as.Date(musoData$date,"%d.%m.%Y")
    for(i in seq_along(dailyVarCodes)){
        bases <- ggplot(data = musoData, mapping = aes_string(x = "date", y = outputVars[i]))
        object <-ifelse(type == "line",paste0("geom_line(colour = '",colour,"')"),
                                  ifelse(type == "point",paste0("geom_line(colour = ",colour,")"),
                                         stop("The")))
        outPlot <- bases + eval(parse(text = object)) + theme_classic() + theme(axis.title.x=element_blank())
        imName <- paste0("daily-",dailyVarCodes[i],plotName)
        cat(sprintf("Saving daily output image of %s as %s\n",outputVars[i],imName))
        suppressMessages(ggsave(imName, outPlot))
    }
    if(settings$normOutputFlags["annual"]!=2){
        return("Annual output graphs was not saved (no annual output from the model)")
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



#' Create Ensemble Plot
#'
#' Generates an ensemble plot from model run outputs, including
#' measurement data and a "best run" simulation.
#'
#' @param run_output_subfolder Character. Name of the subfolder within `working_directory`
#'        that contains the individual run output folders by default calibrateMuso creates a "thread" folder in the root dir which in turn
#'        contains "thread_1/calib/", "thread_2/calib/", etc.
#' @param measurement_data R object or Character. Either a data.frame/data.table object
#'        containing the measurement data, or a character string path to the measurement CSV file.
#' @param model_mapping_code Numeric. The model variable code used by
#'        `musoMapping()` to get the variable name.
#' @param measurement_data_column Character. The name of the column in
#'        `measurement_data` to be used for plotting observed values.
#' @param plot_individual_lines Logical. If `TRUE`, plots individual ensemble member lines.
#'        If `FALSE`, plots ensemble summary (median and quantiles). Default is `TRUE`.
#' @param year_axis_interval Numeric. Interval for year ticks on the x-axis. Default is `2`.
#' @param best_run_param_file Character. Path to the CSV file containing parameters for the
#'        "best run" simulation (most likely "maxlikelihood_parameters.csv"). This path should be
#'        relative to `working_directory` or an absolute path. If `NULL` or file not found,
#'        the best run line is not plotted. Default is "maxlikelihood_parameters.csv" since calibrateMuso creates this by default.
#' @param fileToChange Character. The type of MuSo file to change for the best run
#'        simulation, typically "epc" or "soil". Default is "epc".
#' @param settings An RBBGCMuso settings object (output of `setupMuso()`).
#'        Required if plotting the best run. If `NULL` and best run is attempted,
#'        a warning will be issued.
#' @param output_plot_filename_prefix Character. Prefix for the output PNG filename.
#'        The plot type (individual/summary) will be appended. Default "ensemble_plot".
#' @param meas_point_size Numeric. Size of the measurement data points. Default is `2.5`.
#' @param meadian_line_size Numeric. Size of the median line in summary plots. Default is `0.8`.
#' @param best_run_line_size Numeric. Size of the best run line. Default is `0.6`.
#' @param years_to_plot Numeric vector or NULL. Specific years to plot. If `NULL`, all years are plotted. Default is `NULL`, you may give c(2000,2001) etc.
#'
#' @return Invisibly returns the ggplot object. Saves the plot to a PNG file in `working_directory`.
#'
#' @details
#' The function expects model run output CSVs to be structured as:
#' `working_directory/run_output_subfolder/thread_*/calib/*.csv`.
#' The first column of these CSVs should be dates parsable by `as.Date` with format "%d.%m.%Y".
#' The `best_run_param_file` is expected to have a structure that `changeMuso`
#' can use, typically with parameter values in the third column and calibration parameter names/codes
#' in the second. This one is provided by calibrateMuso
#' 
#' Currently this function uses one binded data.table for all ensemble members so it can use high amounts of memory.
#' 
#' Optionally you can use the 'ragg' package for a faster plotting, but it is not required.
#'
#' @export
#' @importFrom data.table fread rbindlist as.data.table setnames
#' @importFrom ggplot2 ggplot theme_minimal labs theme element_text element_rect element_line scale_x_date geom_line geom_ribbon geom_point ggsave
#' @importFrom lubridate year month day
#' @importFrom progress progress_bar
#' @importFrom grDevices dev.off


musoEnsemblePlot <- function(
    run_output_subfolder = "thread",
    measurement_data,
    model_mapping_code,
    measurement_data_column,
    plot_individual_lines = TRUE,
    year_axis_interval = 2,
    best_run_param_file = "maxlikelihood_parameters.csv",
    fileToChange = "epc",
    settings = setupMuso(),
    output_plot_filename_prefix = "ensemble_plot",
    meas_point_size = 2.5,
    meadian_line_size = 0.8,
    best_run_line_size = 0.6,
    years_to_plot = NULL
) {

  working_directory <- settings$inputLoc
  #Validate Inputs
  if (missing(working_directory) || !dir.exists(working_directory)) {
    stop("`working_directory` must exist.")
  }
  if (missing(measurement_data)) {
    stop("`measurement_data` (file path or data.frame/data.table) must be provided.")
  }
  if (missing(model_mapping_code)) {
    stop("`model_mapping_code` must be provided.")
  }
  if (missing(measurement_data_column)) {
    stop("`measurement_data_column` must be provided (the column name in measurement_data).")
  }

  # Measurement Data Handling 
  md_table <- NULL
  if (is.character(measurement_data) && length(measurement_data) == 1) {
    if (!file.exists(measurement_data)) {
      message("Measurement data file not found at: ", measurement_data)
      md_table <- data.table::data.table() # Empty table
    } else {
      md_table <- tryCatch({
        data.table::fread(measurement_data)
      }, error = function(e) {
        message("Error reading measurement data file: ", measurement_data)
        message("Original error: ", e$message)
        data.table::data.table()
      })
    }
  } else if (is.data.frame(measurement_data) || data.table::is.data.table(measurement_data)) {
    md_table <- data.table::as.data.table(measurement_data)
  } else {
    stop("`measurement_data` must be a file path string or a data.frame/data.table object.")
  }

  if (nrow(md_table) > 0) {
    md_table[md_table == -9999] <- NA
  } else {
    message("Measurement data is empty or could not be loaded. Plot will not include measurement points.")
  }

  # Path and File Setup
  run_csv_base_path <- file.path(working_directory, run_output_subfolder)
  if (!dir.exists(run_csv_base_path)) {
      stop(paste("Run output subfolder not found:", run_csv_base_path))
  }

  csv_paths <- list.files(
    path = run_csv_base_path,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  # Further filter to ensure they are from 'calib' subdirectories within specific thread folders
  csv_paths <- csv_paths[grepl(paste0(run_output_subfolder, "_[^/]+/calib/.*\\.csv$"), csv_paths)]


  if (length(csv_paths) == 0) {
    stop(paste0("No CSV files found in '", run_csv_base_path, "/**/", run_output_subfolder,"_*/calib/' matching the pattern."))
  }

  first_csv_data <- tryCatch({
    data.table::fread(csv_paths[1], select = 1, data.table = FALSE)
  }, error = function(e) {
    stop("Error reading the first CSV file to get dates: ", csv_paths[1], ". Error: ", e$message)
  })

  if (ncol(first_csv_data) < 1 || nrow(first_csv_data) == 0) {
    stop("The first CSV file ", csv_paths[1], " does not contain any data or columns.")
  }
  dates_from_files <- first_csv_data[[1]]
  dates_from_files <- tryCatch({
    as.Date(dates_from_files, format = "%d.%m.%Y")
  }, warning = function(w) {
    message("Warning while parsing dates from the first CSV: ", w$message)
    message("Please ensure the date format in the first column is 'dd.mm.YYYY'.")
    tryCatch(as.Date(dates_from_files), error = function(e) dates_from_files)
  }, error = function(e) {
    message("Error parsing dates from the first CSV: ", e$message)
    dates_from_files
  })

  if (any(is.na(dates_from_files))) {
    warning("Some dates could not be parsed from run outputs and resulted in NA. Expected format: %d.%m.%Y")
  }

  dates_from_files <- dates_from_files[!is.na(dates_from_files)]


  if (!is.null(years_to_plot) && is.numeric(years_to_plot)) {
    message("Filtering data to include only year(s): ", paste(years_to_plot, collapse = ", "))
    dates_from_files <- dates_from_files[lubridate::year(dates_from_files) %in% years_to_plot]
    if (length(dates_from_files) == 0) {
      stop("No data available for the selected year(s). Please check the 'years_to_plot' argument.")
    }
  }


  year_starts <- seq.Date(
    from = as.Date(format(min(dates_from_files, na.rm = TRUE), "%Y-01-01")),
    to = as.Date(format(max(dates_from_files, na.rm = TRUE), "%Y-01-01")),
    by = paste(year_axis_interval, "years")
  )
  
  # Model Variable Name 
  model_var_name <- tryCatch({
    musoMapping(model_mapping_code)
  }, error = function(e) {
    message("Error calling musoMapping with model_mapping_code: ", model_mapping_code)
    message("Ensure RBBGCMuso is loaded and model_mapping_code is valid. Using 'UnknownVariable' as placeholder.")
    message("Original error: ", e$message)
    return("UnknownVariable")
  })

  plot_type_string <- if (plot_individual_lines) "Individual Runs" else "Ensemble Summary"
  plot_title <- paste0("Ensemble of ", model_var_name, " with measurements (", plot_type_string, ")")

  # Combine all run data
  all_runs_data_list <- list()
  total_files <- length(csv_paths)
  message("Reading and combining data from ", total_files, " CSV files for plotting...")
  pb_read <- progress::progress_bar$new(
    format = "Reading run CSVs [:bar] :percent (:current/:total) ETA: :eta",
    total = total_files,
    width = 60
  )

  for (i in seq_along(csv_paths)) {
    file <- csv_paths[i]
    pb_read$tick()
    current_data <- tryCatch({
      data.table::fread(file, select = model_var_name, data.table = TRUE)
    }, error = function(e) {
      message("\nWarning: Could not read or find column '", model_var_name, "' in file: ", file, ". Skipping.")
      NULL
    })

    if (!is.null(current_data) && model_var_name %in% names(current_data) && nrow(current_data) == length(dates_from_files)) {
      current_data[, date := dates_from_files]
      current_data[, run_id := paste0("run_", i)]
      data.table::setnames(current_data, old = model_var_name, new = "value")
      all_runs_data_list[[i]] <- current_data[, .(date, run_id, value)]
    } else if (!is.null(current_data) && nrow(current_data) != length(dates_from_files)) {
      message("\nWarning: File ", file, " has ", nrow(current_data), " rows, but expected ", length(dates_from_files), ". Skipping.")
    } else if (!is.null(current_data) && !(model_var_name %in% names(current_data))) {
       message("\nWarning: Column '", model_var_name, "' not found in file: ", file, ". Skipping.")
    }
  }
  all_runs_data <- data.table::rbindlist(all_runs_data_list, fill = TRUE)

  if (nrow(all_runs_data) == 0) {
    stop("No valid run data could be processed from the CSV files for plotting. Aborting.")
  }

  # Initialize ggplot 
  p <- ggplot2::ggplot() +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(x = "Date", y = model_var_name, title = plot_title) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = ggplot2::rel(1.2)),
      axis.text = ggplot2::element_text(size = ggplot2::rel(0.9)),
      axis.title = ggplot2::element_text(size = ggplot2::rel(1)),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.background = ggplot2::element_rect(fill = "#F5F5F5", color = NA),
      panel.background = ggplot2::element_rect(fill = "#F5F5F5", color = NA),
      axis.line = ggplot2::element_line(color = "black", linewidth = 0.5),
      panel.grid.major = ggplot2::element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_line(color = "grey95", linewidth = 0.2)
    ) +
    ggplot2::scale_x_date(breaks = year_starts, date_labels = "%Y")

  # Plotting based on choice
  if (plot_individual_lines) {
    message("Adding individual ensemble member lines to plot...")
    p <- p + ggplot2::geom_line(data = all_runs_data, ggplot2::aes(x = date, y = value, group = run_id), color = "grey40", alpha = 0.05, linewidth = 0.15)
  } else {
    message("Calculating ensemble summaries (median, quantiles)...")
    ensemble_summary <- all_runs_data[, .(
      median_value = stats::median(value, na.rm = TRUE), 
      q25_value = stats::quantile(value, 0.25, na.rm = TRUE),
      q75_value = stats::quantile(value, 0.75, na.rm = TRUE),
      q05_value = stats::quantile(value, 0.05, na.rm = TRUE),
      q95_value = stats::quantile(value, 0.95, na.rm = TRUE)
    ), by = date]

    message("Adding ensemble summary (ribbons and median line) to plot...")
    p <- p + ggplot2::geom_ribbon(data = ensemble_summary, ggplot2::aes(x = date, ymin = q05_value, ymax = q95_value), fill = "grey70", alpha = 0.5)
    p <- p + ggplot2::geom_ribbon(data = ensemble_summary, ggplot2::aes(x = date, ymin = q25_value, ymax = q75_value), fill = "grey50", alpha = 0.6)
    p <- p + ggplot2::geom_line(data = ensemble_summary, ggplot2::aes(x = date, y = median_value), color = "steelblue", linewidth = meadian_line_size)
  }

  # Best Run Data 
  best_run_data_for_plot <- NULL
  actual_best_run_param_file <- if (!is.null(best_run_param_file) && !startsWith(best_run_param_file, "/") && !grepl("^[A-Za-z]:", best_run_param_file)) {
      file.path(working_directory, best_run_param_file)
  } else {
      best_run_param_file
  }


  if (!is.null(actual_best_run_param_file) && file.exists(actual_best_run_param_file)) {
    if (is.null(settings)) {
        message("Warning: `settings` is NULL. Cannot simulate 'best run' line without RBBGCMuso settings.")
    } else {
        paramVal_best <- data.table::fread(actual_best_run_param_file, sep = ",", header = TRUE)

        if (ncol(paramVal_best) >=3 ) {
            tryCatch({
              changeMuso(settings,
                                    fileToChange = fileToChange,
                                    parameters = paramVal_best[[3]],
                                    calibrationPar = paramVal_best[[2]], 
                                    fixAlloc = FALSE)
              result_maxlikelihood <- calibMuso(settings = settings, skipSpinup = TRUE, prettyOut = FALSE, silent = TRUE)

              if (!is.null(result_maxlikelihood) && !is.null(colnames(result_maxlikelihood))) {
                if (model_var_name %in% colnames(result_maxlikelihood)) {
                  modelVar_maxlikelihood_values <- result_maxlikelihood[, model_var_name]
                  if (length(dates_from_files) == length(modelVar_maxlikelihood_values)) {
                    best_run_data_for_plot <- data.frame(date = dates_from_files, value = modelVar_maxlikelihood_values)
                  } else {
                    message("Length mismatch for 'best run' output. Best run line not plotted.")
                  }
                } else {
                  message("Model variable '", model_var_name, "' not found in 'best run' output. Best run line not plotted.")
                }
              } else {
                message("'Best run' simulation output is NULL or has no column names. Best run line not plotted.")
              }
            }, error = function(e) {
              message("Error during 'best run' simulation: ", e$message, ". Best run line not plotted.")
            })
        } else {
            message("Best run parameter file '", actual_best_run_param_file, "' does not have the expected format (at least 3 columns). Best run line not plotted.")
        }
    }
  } else {
    if (!is.null(best_run_param_file)) message("Best run parameter file not found: ", actual_best_run_param_file, ". Best run line not plotted.")
  }

  if (!is.null(best_run_data_for_plot) && nrow(best_run_data_for_plot) > 0) {
    p <- p + ggplot2::geom_line(data = best_run_data_for_plot, ggplot2::aes(x = date, y = value), color = "red", linewidth = best_run_line_size)
  }

  #  Measurement Points 
  # OLD VERSION
  # if (nrow(md_table) > 0 && measurement_data_column %in% names(md_table)) {
  #   measurement_values_for_plot <- tryCatch(as.numeric(md_table[[measurement_data_column]]), warning = function(w) {
  #       message("Warning: Measurement column '", measurement_data_column, "' could not be coerced to numeric.")
  #       rep(NA_real_, nrow(md_table))
  #   })

  #   if (length(dates_from_files) == length(measurement_values_for_plot)) {
  #     md_plot_data <- data.table::data.table(date = dates_from_files, value_md = measurement_values_for_plot)
  #     md_plot_data <- md_plot_data[!is.na(value_md)]
  #     if(nrow(md_plot_data) > 0) {
  #         p <- p + ggplot2::geom_point(data = md_plot_data, ggplot2::aes(x = date, y = value_md), color = "blue", size = 2.5, shape = 19)
  #     } else {
  #         message("No valid (non-NA) measurement data points to plot for '", measurement_data_column, "'.")
  #     }
  #   } else {
  #     message("Length mismatch between simulation dates and measurement data rows. Measurement points will not be plotted.")
  #   }
  # } else if (nrow(md_table) > 0 && !(measurement_data_column %in% names(md_table))) {
  #   message("Column '", measurement_data_column, "' not found in measurement data. Measurement points will not be plotted.")
  # }

  # VERSION 2 ALigning measurement
  # if (nrow(md_table) > 0 && measurement_data_column %in% names(md_table)) {

  #   if (ncol(md_table) < 3) {
  #     message("Measurement data must have at least 3 columns (year, month, day) to construct dates. Points will not be plotted.")
  #   } else {
  #     # Make a copy to avoid modifying the original md_table by reference
  #     md_to_align <- data.table::copy(md_table)

  #     # Robustly create a date column from the first three columns
  #     md_to_align[, date := tryCatch({
  #         as.Date(paste(md_to_align[[1]], md_to_align[[2]], md_to_align[[3]], sep = "-"))
  #       }, error = function(e) {
  #         message("Could not create valid dates from the first three columns of measurement data.")
  #         as.Date(NA) # Return NA on failure
  #     })]
      
  #     md_to_align <- md_to_align[!is.na(date)]

  #     # Data Alignment via Merge 
  #     if (nrow(md_to_align) > 0) {
  #       model_dates_dt <- data.table::data.table(date = dates_from_files)

  #       # Subset and rename for clarity and to avoid column name conflicts
  #       md_subset <- md_to_align[, .SD, .SDcols = c("date", measurement_data_column)]
  #       data.table::setnames(md_subset, old = measurement_data_column, new = "value_md")

  #       # Align measurement data to the model's timeline using a left join
  #       aligned_md <- merge(model_dates_dt, md_subset, by = "date", all.x = TRUE)

  #       # Ensure the value column is numeric
  #       aligned_md[, value_md := as.numeric(value_md)]

  #       # Filter out NA values for plotting (these are dates with no corresponding measurement)
  #       md_plot_data <- aligned_md[!is.na(value_md)]

  #       if(nrow(md_plot_data) > 0) {
  #           message("Plotting ", nrow(md_plot_data), " aligned measurement points.")
  #           p <- p + ggplot2::geom_point(data = md_plot_data, ggplot2::aes(x = date, y = value_md), color = "blue", size = 2.5, shape = 19)
  #       } else {
  #           message("No valid (non-NA) measurement data points found after aligning with model dates.")
  #       }

  #     } else {
  #       message("No valid dates could be constructed from measurement data to perform alignment.")
  #     }
  #   }

  # } else if (nrow(md_table) > 0 && !(measurement_data_column %in% names(md_table))) {
  #   message("Value column '", measurement_data_column, "' not found in measurement data. Measurement points will not be plotted.")
  # }

  if (nrow(md_table) > 0 && measurement_data_column %in% names(md_table)) {

    if (ncol(md_table) < 3) {
      message("Measurement data must have at least 3 columns (year, month, day) to construct dates. Points will not be plotted.")
    } else {
      # Prepare the measurement data first
      measurements_processed <- md_table %>%
        tibble::as_tibble() %>%
        dplyr::mutate(
          date = lubridate::make_date(year = .[[1]], month = .[[2]], day = .[[3]]),
          value_md = as.numeric(.data[[measurement_data_column]])
        ) %>%
        dplyr::filter(!is.na(date) & !is.na(value_md)) %>%
        dplyr::select(date, value_md)

      # Create a tibble for the model's date range and join the measurements
      md_plot_data <- tibble::tibble(date = dates_from_files) %>%
        dplyr::left_join(measurements_processed, by = "date") %>%
        tidyr::drop_na(value_md) # Remove dates that don't have a measurement


      if(nrow(md_plot_data) > 0) {
          message("Plotting ", nrow(md_plot_data), " aligned measurement points.")
          p <- p + ggplot2::geom_point(data = md_plot_data, ggplot2::aes(x = date, y = value_md), color = "blue", size = meas_point_size, shape = 19)
      } else {
          message("No valid (non-NA) measurement data points found after aligning with model dates.")
      }
    }

  } else if (nrow(md_table) > 0 && !(measurement_data_column %in% names(md_table))) {
    message("Value column '", measurement_data_column, "' not found in measurement data. Measurement points will not be plotted.")
  }

  #  Saving the plot
  filename_suffix_plot <- if (plot_individual_lines) "individual_lines" else "ensemble_summary"
  final_plot_filename <- file.path(working_directory, paste0(output_plot_filename_prefix, "_", filename_suffix_plot, ".png"))
  message("\nSaving the plot to ", final_plot_filename, "...")

  if (requireNamespace("ragg", quietly = TRUE)) {
    message("Using ragg package for PNG saving.")
    tryCatch({
      ragg::agg_png(
        filename = final_plot_filename,
        width = 10, height = 6, units = "in", res = 300, background = "#F5F5F5"
      )
      print(p) # Explicitly print the ggplot object
      grDevices::dev.off()
      message("Plot saved successfully using ragg.")
    }, error = function(e) {
      message("Error using ragg: ", e$message, ". Falling back to ggsave.")
      ggplot2::ggsave(
        filename = final_plot_filename, plot = p,
        width = 10, height = 6, dpi = 300, bg = "#F5F5F5"
      )
      message("Plot saved successfully using ggsave as fallback.")
    })
  } else {
    message("ragg package not found. Falling back to ggsave.")
    ggplot2::ggsave(
      filename = final_plot_filename, plot = p,
      width = 10, height = 6, dpi = 300, bg = "#F5F5F5"
    )
    message("Plot saved successfully using ggsave.")
  }
  
  print(paste("Plot saved as", basename(final_plot_filename), "in", working_directory))
  return(invisible(p)) # Return the ggplot object invisibly
}

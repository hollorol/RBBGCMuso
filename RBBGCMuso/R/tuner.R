#' tuneMusoUI 
#' 
#' This is a simple parameter tuner function which works great in a flat directory system
#'
#' @param parameterFile optional, the parameter csv file
#' @importFrom shinyjs useShinyjs toggle
#' @importFrom shinyWidgets pickerInput
#' @importFrom plotly plotlyOutput renderPlotly layout
#' @importFrom shiny tags actionButton numericInput HTML checkboxInput titlePanel radioButtons textAreaInput fluidPage sidebarLayout sidebarPanel mainPanel getShinyOption tabsetPanel tabPanel tagList selectInput sliderInput renderUI
#' @usage ...
#' @export 

tuneMusoUI <- function(parameterFile = NULL, ...){
    setwd(getShinyOption("musoRoot"))
    dir.create("bck",showWarnings = FALSE)
    file.copy("n.ini","bck/n.ini", overwrite=FALSE)
    if(is.null(parameterFile)){
        parameterFile <- "parameters.csv"
    }
    parameters <- read.csv(parameterFile, stringsAsFactors=FALSE)
    settings <- setupMuso(...)

    #for some reason it can't find this function from setupMuso even though it's exported and within namespace, will check later why
    searchBellow <- function(inFile, key, stringP = TRUE,  n=1, management = FALSE){
        
            if(stringP){
                unlist(strsplit(inFile[grep(key,inFile, perl=TRUE)+n],split = "\\s+", useBytes = TRUE))[1]
            } else {
                as.numeric(unlist(strsplit(inFile[grep(key,inFile,perl=TRUE)+n],split = "\\s+", useBytes = TRUE))[1])
            }
    }

    # looking for the planting file if there is any, else use the epc file within the ini file
    iniContent <- readLines(settings$iniInput[2])
    management_file <- searchBellow(iniContent, "MANAGEMENT_FILE", stringP = TRUE, n = 1)


     if (file.exists(management_file)) {
        managementContent <- readLines(management_file)

        # Extract planting file name
        planting_file <- searchBellow(managementContent, "PLANTING", stringP = TRUE, n = 2)

        if (file.exists(planting_file)) {
            
            planting_data <- read.table(planting_file, header = TRUE, sep = "", stringsAsFactors = FALSE)
 
            epc_files <- unique(unlist(strsplit(paste(planting_data$CROP.file, collapse = " "), " +")))
            #used for plotting (not yet implemented):
            epc_dates <- as.Date(planting_data$DATE, format="%Y.%m.%d") 
            epc_index_map <- setNames(seq_along(epc_files), epc_files)   
            epc_labels <- paste0(seq_along(epc_files), ") ", epc_files)

        } else {
              
            warning("Planting file not found: ", planting_file)
            epc_files <- settings$epcInput[2]
            epc_labels <- NULL
        }
    } else {
        
        warning("Management file not found: ", management_file)
        epc_files <- settings$epcInput[2]
        epc_labels <- NULL
    }


    fluidPage(
            useShinyjs(),  
            actionButton("toggleUI", "Show/Hide Controls"),  
            tags$script(HTML("$('#toggleUI').css('margin-bottom', '10px');")),  # Add some spacing
          
            # control + enter shortcut to run the model
            tags$script(HTML("
                $(document).on('keydown', function(event) {
                    if (event.ctrlKey && event.key === 'Enter') {
                        $('#runModel').click();
                    }
                });
            ")),

        # tags$head(tags$style(HTML("#iniContainer {width: 80vw;}"))),
        tags$head(tags$style(HTML("#contolp {height: 80vh;overflow-y:scroll;}"))),
        titlePanel("Biome-BGCMuSo parameter tuner"),
        sidebarLayout(
            div(id="controlPanel",
            sidebarPanel(tabsetPanel(type="tabs",
                tabPanel("params",
                fileInput("measurementFile", "Upload Measurement File", 
                        accept = c(".txt")),
                   checkboxInput("autoupdate","Automatic update"),
                     checkboxInput("singleYear", "Single year mode", value = FALSE),
                    uiOutput("yearRangeUI"),
                    selectInput("selected_epc", "Select EPC File", choices = setNames(epc_files,epc_labels), selected = epc_files[1]),
                    #trying to perfectly align the box and the button
                   tags$div(
                        style = "display: flex; align-items: center; gap: 10px;",  
                        actionButton("resetParams", "Reset to originals"),
                        checkboxInput("restoreOnExit", "Restore originals on exit", value = FALSE)
                    ),

                   tags$div(id="controlp",shinyWidgets::pickerInput(
                                inputId = "selected_vars",
                                label = "Select output variables (multiple can be chosen)",
                                choices = settings$dailyOutputTable$name, 
                                multiple = TRUE,
                                options = list(`actions-box` = TRUE)  
                                ), 
                                ## slider for parameters
                                 uiOutput("param_sliders") #created in server
                                 
                    ),
              tags$div(actionButton(inputId="runModel","Run MuSo"),
                       radioButtons(inputId="destination",
                                    label="reference or modified",
                                    choiceValues=c("auto","prev","nextVal"),
                                    choiceNames=c("automatic","reference","modified")))),
                tabPanel("ini",tags$div(id="iniContainer",
                                        textAreaInput("inifile","Normal Ini file",
                                                      value=paste(readLines(settings$iniInput[2]),
                                                                  collapse="\n"))),
                         actionButton(inputId="getOriginalIni", "Load original"),
                         actionButton(inputId="overwriteIni", "overwrite")

                )
                ))),
            #mainPanel(plotlyOutput(outputId="Result"))
            mainPanel(uiOutput("dynamicPlots"))
          
        ) 
    )
}

#' tuneMusoServer 
#' 
#' Server program for tumeMuso
#'
#' @param input shiny input
#' @param output shiny output
#' @param session dinamic session management for shiny
#' @importFrom shiny reactiveValues isolate observeEvent
#' @importFrom plotly renderPlotly plot_ly add_trace
#' @usage ...
#' @export 

tuneMusoServer <- function(input, output, session){

    settings <- setupMuso()
    dates <- as.Date(musoDate(settings$startYear, numYears=settings$numYears),"%d.%m.%Y") 
    rv <- reactiveValues(settings = setupMuso())

    parameters <- read.csv("parameters.csv", stringsAsFactors=FALSE)

    epcValues <- reactiveValues()  # Store EPC values

    
    outputList <- reactiveValues(prev = character(0), nextVal = character(0))

    # original values for epc
    defaultValues <- reactive({
    req(input$selected_epc)
    musoGetValues(input$selected_epc, parameters[, 2])
  })


    currentValues <- reactive({
        req(input$selected_epc)
        epc <- input$selected_epc
        if (!is.null(epcValues[[epc]])) {
        epcValues[[epc]]
        } else {
        defaultValues()
        }
    })

    # Get the "boot up" values for each EPC file for resetting
    InitialDefaults <- reactiveValues()

    observe({
        
        for (epc in epc_files) {
            # Only initialize if not already present
            if (is.null(InitialDefaults[[epc]])) {
            # Call musoGetValues to get the default parameters for this EPC
            #This is done only once per EPC
            InitialDefaults[[epc]] <- musoGetValues(epc, parameters[, 2])
            }
        }
        })

    # Reset the sliders to the default values for the selected EPC
    observeEvent(input$resetParams, { 
        req(input$selected_epc)
        epc <- input$selected_epc
        defaults <- InitialDefaults[[epc]]
        epcValues[[epc]] <- defaults

        for (i in seq_len(nrow(parameters))) {
            updateSliderInput(session, paste0("param_", i), value = defaults[i])
        }
        print(paste0("Reset sliders to defaults for", epc))
    })


    #exit box not quite working as intended, we'll store its state directly
        restoreFlag <- reactiveVal(FALSE)

        observeEvent(input$restoreOnExit, {
            restoreFlag(input$restoreOnExit)
        })

    
    session$onSessionEnded(function() {
        if (isolate(restoreFlag())) {  
            cat("Restoring all EPC files to original...\n")
            
            isolate({  # Ensuring all reactive values are accessed
                for (epc in epc_files) {
                    if (!is.null(InitialDefaults[[epc]])) {
                        paramVal <- InitialDefaults[[epc]]  

                        settings$epcInput[["normal"]] <- epc
                        changeMuso(settings, paramVal, calibrationPar = parameters[, 2], 
                                fileToChange = "epc", fixAlloc = FALSE)

                        cat(paste0("Restored ", epc, " to original values.\n"))
                    }
                }
            })
        }
    })



    # Year range sliders
    output$yearRangeUI <- renderUI({
    req(settings)
    min_year <- as.numeric(format(min(dates), "%Y"))
    max_year <- as.numeric(format(max(dates), "%Y"))

    if (input$singleYear) {
      # Single-year slider
      sliderInput(
        "yearRange",
        label = "Select Year",
        min = min_year,
        max = max_year,
        value = min_year,
        step = 1,
        sep = ""
      )
    } else {
      # Year range slider
      sliderInput(
        "yearRange",
        label = "Select Year Range",
        min = min_year,
        max = max_year,
        value = c(min_year, max_year),
        step = 1,
        sep = ""
      )
    }
  })

    # Read the measurement file
    measurements <- reactive({
    req(input$measurementFile)  
    df <- read.table(input$measurementFile$datapath, header = TRUE, sep = "", stringsAsFactors = FALSE)
    
    
    df[df < 0] <- NA
    
    # Convert year, month, day into a Date object
    df$Date <- as.Date(with(df, paste(yyyy, mm, dd, sep = "-")), "%Y-%m-%d")
    
    

    return(df)
    })

    
    # param sliders
    output$param_sliders <- renderUI({
        req(currentValues())
        sliders <- lapply(1:nrow(parameters), function(i) {
        sliderInput(
            paste0("param_", i),
            label = parameters[i, 1],
            min = parameters[i, 3],
            max = parameters[i, 4],
            value = currentValues()[i],
            step = (parameters[i, 4] - parameters[i, 3]) / 100
        )
        })
        do.call(tagList, sliders)
    })


    # upon selection if no stored values for the selected EPC, initialize with defaults 
        observeEvent(input$selected_epc, {
        req(currentValues())
        newVals <- currentValues()
        for(i in seq_len(nrow(parameters))) {
            updateSliderInput(session, paste0("param_", i), value = newVals[i])
        }
        })


                observe({
                req(input$selected_epc)
                epc <- input$selected_epc
                updated <- epcValues[[epc]]
                indices <- seq_len(nrow(parameters))
                for (i in indices) {
                    slider_val <- input[[paste0("param_", i)]]
                    if (!is.null(slider_val) && length(slider_val) > 0) {
                    updated[i] <- slider_val
                    }
                }
                epcValues[[epc]] <- updated
                })


            # Create a reactive value to store the previously selected EPC.
            prevEPC <- reactiveVal(NULL)
            # When the selected EPC changes (when switching away), write the current slider values for the previous EPC to its file.
            observeEvent(input$selected_epc, {
                new_epc <- input$selected_epc
                old_epc <- prevEPC()
                if (!is.null(old_epc) && old_epc != new_epc) {
                # Retrieve the stored slider values for the old EPC.
                paramVal_old <- epcValues[[old_epc]]
 
                settings$epcInput[["normal"]] <- old_epc
                # Write the slider values into that EPC file.
                changeMuso(settings, paramVal_old, calibrationPar = parameters[,2],
                            fileToChange = "epc", fixAlloc = FALSE)
                print(paste("Saved changes for", old_epc))
                }
                prevEPC(new_epc)
            })

    # Toggle visibility of the sidebar panel
    observeEvent(input$toggleUI, {
    toggle("controlPanel")  
    })



 observeEvent(input$runModel, {
    req(input$selected_epc)
    epc <- input$selected_epc
    paramVal <- sapply(1:nrow(parameters), function(i) input[[paste0("param_", i)]])
    
    settings$epcInput[["normal"]] <- epc
    print(paste("Updating EPC file", epc, "with new parameters before running model."))
   
    changeMuso(settings, paramVal, calibrationPar = parameters[,2],
               fileToChange = "epc", fixAlloc = FALSE)
    
    settings <- setupMuso()

    
    #tryCatch({
    #  system(paste(settings$executable, settings$iniInput[2], sep = " "))
      
    #}, error = function(e) {
    #  stop("Error running the model.")
    #})
    
    
    result <- calibMuso(settings = settings, calibrationPar = parameters[,2], parameters = paramVal)
    if (length(result) == 0) {
      showNotification("Model did not return results!", type = "error")
    } else {


    outputList$nextVal <- result

    }})
    



    observe({
        if(input$autoupdate){
                     paramVal <- sapply(1:nrow(parameters),function(x){
                                            input[[paste0("param_", x)]]
              })


                     if(isolate(input$destination) == "auto"){
                         outputList[['prev']] <- isolate(outputList[['nextVal']]) 
                         outputList[['nextVal']] <- calibMuso(settings = settings,
                                                           calibrationPar = parameters[,2],
                                                           parameters = paramVal)       
                     } else {
                         outputList[[isolate(input$destination)]] <- calibMuso(settings = settings,
                                                           calibrationPar = parameters[,2],
                                                           parameters = paramVal)      

                     }
            
        }
    })

                output$dynamicPlots <- renderUI({
                req(input$selected_vars)
                
                plot_outputs <- lapply(input$selected_vars, function(var) {
                    plotlyOutput(paste0("plot_", var), height = "400px")
                })
                do.call(tagList, plot_outputs)
                })

                

    observe({
        if(input$autoupdate){
                     paramVal <- sapply(1:nrow(parameters),function(x){
                                            input[[paste0("param_", x)]]
              })


                     if(isolate(input$destination) == "auto"){
                         outputList[['prev']] <- isolate(outputList[['nextVal']]) 
                         outputList[['nextVal']] <- calibMuso(settings = settings,
                                                           calibrationPar = parameters[,2],
                                                           parameters = paramVal)       
                     } else {
                         outputList[[isolate(input$destination)]] <- calibMuso(settings = settings,
                                                           calibrationPar = parameters[,2],
                                                           parameters = paramVal)       

                     }
            
        }
    })

                output$dynamicPlots <- renderUI({
                req(input$selected_vars)
                
                plot_outputs <- lapply(input$selected_vars, function(var) {
                    plotlyOutput(paste0("plot_", var), height = "400px")
                })
                do.call(tagList, plot_outputs)
                })

                
                observe({
                req(input$selected_vars, length(outputList$nextVal) != 0)
                lapply(input$selected_vars, function(var) {
                    output[[paste0("plot_", var)]] <- renderPlotly({
                    # giving condition to check to avoid warning messages
                    if (isTRUE(input$singleYear)) {
                        validate(
                        need(is.finite(input$yearRange), "Year not available yet")
                        )
                        selectedYears <- input$yearRange  
                        validate(
                        need(length(input$yearRange) == 2 &&
                            is.finite(input$yearRange[1]) &&
                            is.finite(input$yearRange[2]),
                            "Year range not available yet")
                        )
                        selectedYears <- seq(input$yearRange[1], input$yearRange[2])
                    }
                                        
                    #selectedYears <- if (input$singleYear) input$yearRange else seq(input$yearRange[1], input$yearRange[2])
                    filteredDates <- dates[as.numeric(format(dates, "%Y")) %in% selectedYears]
                    
                    # Get simulation data
                    filteredPrev <- if (length(outputList$prev) != 0) {
                        outputList$prev[as.numeric(format(dates, "%Y")) %in% selectedYears, ]
                    } else NULL
                    filteredNext <- outputList$nextVal[as.numeric(format(dates, "%Y")) %in% selectedYears, ]
                    
                    # Apply scaling 
                    if (var %in% c("GPP", "TR", "NEE")) {
                        if (!is.null(filteredPrev)) filteredPrev[, var] <- filteredPrev[, var] * 1000
                        filteredNext[, var] <- filteredNext[, var] * 1000
                    }
                    
                    
                    p <- plot_ly()
                    if (!is.null(filteredPrev)) {
                        p <- add_trace(p, x = filteredDates, y = filteredPrev[, var], 
                                    type = 'scatter', mode = 'lines', name = "Previous Simulation")
                    }
                    p <- add_trace(p, x = filteredDates, y = filteredNext[, var], 
                                    type = 'scatter', mode = 'lines', name = "New Simulation", line = list(color = "red"))
                    
                    # Add measurements IF available
                    if (var %in% c("NEE", "GPP", "TR", "ET") && !is.null(input$measurementFile)) {
                        df <- measurements()
                        measurement_col <- switch(var, "NEE" = 4,"GPP" = 5, "TR" = 6, "ET" = 7)
                        df_filtered <- df[df$yyyy %in% selectedYears, ]
                        #df_filtered[, measurement_col][is.na(df_filtered[, measurement_col])] <- NA
                        p <- add_trace(p, x = df_filtered$Date, y = df_filtered[, measurement_col],
                                    type = 'scatter', mode = 'markers', name = paste(var, "Measurement"),
                                    marker = list(symbol = "circle", size = 7))
                    }
                    p <- p %>% plotly::layout(
                        #title = list(text = paste("Plot of", var),
                        #    font = list(size = 16, color = "black")),
                        #xaxis = list(title = "Date"),
                        yaxis = list(title = var)
                        )

                    p
                    })
                })
                })


    observeEvent(input$getOriginalIni,{
                     updateTextAreaInput(session, "inifile", value=paste(readLines("bck/n.ini"),
                                                                                              collapse="\n") )
    })

}


#' tuneMuso
#'
#' launchApp launch the shiny app
#' @param ... Other parameters for shinyApp function
#' @importFrom shiny shinyApp shinyOptions
#' @export
tuneMuso <- function(directory = NULL,...){ 
    shinyOptions(workdir = getwd())
    if(is.null(directory)){
        shinyOptions(musoRoot = ".")
    } else {
        shinyOptions(musoRoot = normalizePath(directory))
    }
    shinyApp(ui = tuneMusoUI(), server = tuneMusoServer, options = list(...))
}

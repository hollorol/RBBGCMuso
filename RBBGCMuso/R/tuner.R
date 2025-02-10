#' tuneMusoUI 
#' 
#' This is a simple parameter tuner function which works great in a flat directory system
#'
#' @param parameterFile optional, the parameter csv file
#' @importFrom shinyjs useShinyjs toggle show hide disable enable removeEvent runjs 
#' @importFrom dplyr filter 
#' @importFrom shinyjqui jqui_resizable 
#' @importFrom lubridate year
#' @importFrom shinyWidgets pickerInput 
#' @importFrom plotly plotlyOutput renderPlotly layout
#' @importFrom shiny tags actionButton numericInput HTML checkboxInput titlePanel radioButtons textAreaInput fluidPage sidebarLayout sidebarPanel mainPanel getShinyOption tabsetPanel tabPanel tagList selectInput sliderInput renderUI div fileInput uiOutput updateSliderInput observe observeEvent validate need showNotification 
#' @usage ...
#' @export 
tuneMusoUI <- function(parameterFile = NULL, ...) {
    setwd(getShinyOption("musoRoot"))
    dir.create("bck", showWarnings = FALSE)
    file.copy("n.ini", "bck/n.ini", overwrite = FALSE)
    
    if (is.null(parameterFile)) {
        parameterFile <- "parameters.csv"
    }
    
    parameters <- read.csv(parameterFile, stringsAsFactors = FALSE)
    settings <- setupMuso(...)

    fluidPage(
        useShinyjs(),
    
    # Global CSS, disable page scroll and define our panel layouts (different scroller for ui and plots respectively)
    tags$head(tags$style(HTML("
      /* Prevent global scrolling */
      html, body {
          height: 100%;
          overflow: hidden;
      }
      
      /* Floating toggle button (always visible) */
      #toggleUIButton {
          position: fixed;
          top: 10px;
          left: 10px;
          z-index: 10000;
      }
      
      /* Flex container for panels */
      .row-container {
          display: flex;
          width: 100%;
          height: 85vh;
      }
      
      /* Control panel styling */
      #controlPanel {
          background: #f8f8f8;
          padding: 15px;
          border-right: 2px solid #ddd;
          min-width: 250px;
          max-width: 450px;
          overflow-y: auto;
          box-sizing: border-box;
      }
      
      /* Plot panel styling – fills remaining space */
      #plotPanel {
          flex-grow: 1;
          padding: 20px;
          overflow-y: auto;
          background: #fff;
          width: 100%;
          height: 100%;
          max-width: none;
          max-height: none;
      }
    "))),
    
    # moving the title to the right so the toggleui button has space
    titlePanel(div(style = "margin-left: 100px;", "Biome-BGCMuSo Parameter Tuner")),
    
    # Floating toggle button to collapse/restore the control panel
    div(
      id = "toggleUIButton",
      actionButton("toggleUI", label = "Toggle UI", icon = icon("bars"))
    ),
    
    # Default keyboard shortcut for running the model
    tags$script(HTML("
      $(document).on('keydown', function(event) {
          if (event.ctrlKey && event.key === 'Enter') {
              $('#runModel').click();
          }
      });
    ")),
    

    tags$script(HTML("
      $(document).on('mousedown', function(event) {
        if (event.which === 2) {  // Middle mouse button (Mouse3)
          $('#runModel').click();
          event.preventDefault();
        }
      });
    ")),

    # Main container with both panels
    div(class = "row-container",
      # Resizable control panel using jqui
      jqui_resizable(
        div(
          id = "controlPanel",
          tabsetPanel(type = "tabs",
            tabPanel("Parameters",
              fileInput("measurementFile", "Upload Measurement File", 
                        accept = c(".txt")),
              checkboxInput("autoupdate", "Automatic update"),
              checkboxInput("singleYear", "Single year mode", value = FALSE),
              uiOutput("yearRangeUI"),
              uiOutput("selectEPC"),
              
              # Reset buttons
              tags$div(
                style = "display: flex; align-items: center; gap: 10px;",  
                actionButton("resetParams", "Reset to originals"),
                checkboxInput("restoreOnExit", "Restore originals on exit", value = FALSE)
              ),
              
              # Select variables for plotting
              tags$div(
                id = "controlp",
                shinyWidgets::pickerInput(
                  inputId = "selected_vars",
                  label = "Select output variables (multiple can be chosen)",
                  choices = settings$dailyOutputTable$name, 
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE)
                ),
                tags$div(id = "slider-container", uiOutput("param_sliders"))
              ),
              
              # Hotkey input and run button
              tags$div(
                style = "margin-bottom: 15px;",
                textInput("hotkeyInput", "Set Hotkey", value = "Ctrl+Enter", placeholder = "e.g. Ctrl+Enter"),
                tags$div(
                  style = "display: flex; gap: 10px; align-items: center; margin-top: 10px;",
                  actionButton("setHotkey", "Apply Hotkey"),
                  actionButton(inputId = "runModel", "Run MuSo")
                )
              ),
              
              # Reference or modified selection
              radioButtons(
                inputId = "destination",
                label = "Reference or Modified",
                choiceValues = c("auto", "prev", "nextVal"),
                choiceNames = c("automatic", "reference", "modified")
              )
            ),
            
            tabPanel("INI File",
              tags$div(
                id = "iniContainer",
                textAreaInput("inifile", "Normal Ini file",
                              value = paste(readLines(settings$iniInput[2]), collapse = "\n"))
              ),
              actionButton(inputId = "getOriginalIni", "Load original"),
              actionButton(inputId = "overwriteIni", "Overwrite")
            )
          )
        ),
        options = list(handles = "e")  # Allow resizing only on the right edge
      ),
      
      # Plot panel: scrollable and fills remaining space
      div(
        id = "plotPanel", 
        uiOutput("dynamicPlots")
      )
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
#' @importFrom plotly renderPlotly plot_ly add_trace add_annotations layout
#' @usage ...
#' @export 

tuneMusoServer <- function(input, output, session){

    #for some reason it can't find this function from setupMuso even though it's exported and within namespace, will check later why
    searchBellow <- function(inFile, key, stringP = TRUE,  n=1, management = FALSE){
        
            if(stringP){
                unlist(strsplit(inFile[grep(key,inFile, perl=TRUE)+n],split = "\\s+", useBytes = TRUE))[1]
            } else {
                as.numeric(unlist(strsplit(inFile[grep(key,inFile,perl=TRUE)+n],split = "\\s+", useBytes = TRUE))[1])
            }
    }


    settings <- setupMuso()
    dates <- as.Date(musoDate(settings$startYear, numYears=settings$numYears),"%d.%m.%Y") 
    rv <- reactiveValues(settings = setupMuso(), epc_files = character(0), epc_labels = character(0), epc_dates = data.frame(), epc_num_labels = character(0))

      parameters <- read.csv("parameters.csv", stringsAsFactors=FALSE)

    epcValues <- reactiveValues()  # Store EPC values


    # looking for the planting file if there is any, else use the epc file within the ini file
    observe({
        req(file.exists(settings$iniInput[2]))  # Ensure the INI file exists before reading
        iniContent <- readLines(settings$iniInput[2])
        management_file <- searchBellow(iniContent, "MANAGEMENT_FILE", stringP = TRUE, n = 1)

        if (file.exists(management_file)) {
            managementContent <- readLines(management_file)
            planting_file <- searchBellow(managementContent, "PLANTING", stringP = TRUE, n = 2)


            if (file.exists(planting_file)) {
                planting_data <- read.table(planting_file, header = TRUE, sep = "", stringsAsFactors = FALSE)

                epc_files <- unique(unlist(strsplit(paste(planting_data$CROP.file., collapse = " "), " +")))
                planting_data$DATE <- as.Date(planting_data$DATE, format = "%Y.%m.%d")
                epc_dates <- planting_data
                #planting_dates <- 
                #print("EPC files found:")
                #print(epc_files)

                # Ensure that the update happens safely
                isolate({
                    rv$epc_files <- epc_files
                    rv$epc_labels <- paste0(seq_along(epc_files), ") ", epc_files)
                    rv$epc_num_labels <- paste0(seq_along(rv$epc_files), ")")

                    rv$epc_dates <- epc_dates
                })
            } else {
                warning("Planting file not found: ", planting_file)
                print(paste0("Using EPC file from INI file ", settings$epcInput[2]))
                isolate({
                    rv$epc_files <- settings$epcInput[2]
                    rv$epc_labels <- paste0(seq_along(rv$epc_files), ") ", rv$epc_files)
                    rv$epc_num_labels <- paste0(seq_along(rv$epc_files), ")")
                    #rv$epc_dates <- as.Date(musoDate(settings$startYear, numYears=1),"%d.%m.%Y")[1]   
                    rv$epc_dates <- NULL
                })
            }
        } else {
            warning("Management file not found: ", management_file)
            isolate({
                print(paste0("Using EPC file from INI file ", settings$epcInput[2]))
                rv$epc_files <- settings$epcInput[2]
                rv$epc_labels <- paste0(seq_along(rv$epc_files), ") ", rv$epc_files)
                rv$epc_num_labels <- paste0(seq_along(rv$epc_files), ")")
                #rv$epc_dates <- as.Date(musoDate(settings$startYear, numYears=1),"%d.%m.%Y")[1]   
                rv$epc_dates <- NULL
            })
        }
    })

            



        output$selectEPC <- renderUI({
        req(length(rv$epc_files) > 0)  

        selectInput(
            "selected_epc",
            "Select EPC File",
            choices = setNames(rv$epc_files, rv$epc_labels),
            selected = rv$epc_files[1]
        )
        })

    
    outputList <- reactiveValues(prev = character(0), nextVal = character(0))


    # Track if a model run is already in progress (needed for auto-update loop bug fix which doesn't work yet)
    isRunning <- reactiveVal(FALSE)

    # Debounced trigger for slider changes
    sliderDebounce <- reactive({
        lapply(1:nrow(parameters), function(x) input[[paste0("param_", x)]])
        TRUE
    }) %>% debounce(500)  # Debounce time (500ms (0.5 sec)), how long to wait after the last change before triggering the event

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
        req(rv$epc_files)
        for (epc in rv$epc_files) {
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
        print(paste0("Reset sliders to initials for ", epc))
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
                for (epc in rv$epc_files) {
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


    # creating tracker that will avoid auto-update from running the model upon epc switching (not yet used later)
    updatingEPC <- reactiveVal(FALSE)

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


            # Create a reactive value to store the previously selected EPC
            prevEPC <- reactiveVal(NULL)
            # When the selected EPC changes (when switching away), write the current slider values for the previous EPC to its file
            observeEvent(input$selected_epc, {
                new_epc <- input$selected_epc
                old_epc <- prevEPC()
                if (!is.null(old_epc) && old_epc != new_epc) {
                # Retrieve the stored slider values for the old EPC
                paramVal_old <- epcValues[[old_epc]]
 
                settings$epcInput[["normal"]] <- old_epc
                # Write the slider values into that EPC file
                changeMuso(settings, paramVal_old, calibrationPar = parameters[,2],
                            fileToChange = "epc", fixAlloc = FALSE)
                print(paste("Saved changes for", old_epc))
                }
                prevEPC(new_epc)
            })

    # Toggle visibility of the sidebar panel
    observeEvent(input$toggleUI, {
    toggle("controlPanel", anim = FALSE)
    shinyjs::runjs("
        // If the control panel is hidden, force the plot panel to expand
        if ($('#controlPanel').is(':visible')) {
            $('#plotPanel').css('flex', '2');
        } else {
            // Set flex property so that plot panel fills the entire row
            $('#plotPanel').css('flex', '1 1 100%');
        }
        // Trigger a window resize event
        $(window).trigger('resize');
        // Trigger a resize on the plot container
        if(typeof Plotly !== 'undefined'){
            Plotly.Plots.resize(document.getElementById('dynamicPlots'));
        }
    ")
    })



    observeEvent(input$runModel, {
        req(input$selected_epc)
        epc <- input$selected_epc
        paramVal <- sapply(1:nrow(parameters), function(i) input[[paste0("param_", i)]])
        
        settings$epcInput[["normal"]] <- epc
        print(paste("Updating EPC file", epc, "with new parameters before running model"))
    
        changeMuso(settings, paramVal, calibrationPar = parameters[,2],
                fileToChange = "epc", fixAlloc = FALSE)
        
        settings <- setupMuso()
        
        
        result <- calibMuso(settings = settings, calibrationPar = parameters[,2], parameters = paramVal, silent = TRUE)
        if (length(result) == 0) {
        showNotification("Model did not return results!", type = "error")
        } else {

        print("Model ran successfully")
        outputList$nextVal <- result

        }})
    
    # visual feedback for when the model is running so to prevent the user from changing sliders
    # preventing infinite loop of auto-update. Doesn't seem to work. I mean the model won't run but the values keep alternating indefinitely, breaking the app so sadge
    observeEvent(isRunning(), {
        if (isRunning()) {
            shinyjs::disable("selected_epc")
            shinyjs::disable("runModel")  # Disable manual run button
            shinyjs::disable("autoupdate")  # Disable auto-update checkbox
            shinyjs::disable("sliders-container")  # Disable all sliders
            shinyjs::show("loading-spinner")
        } else {
            shinyjs::enable("selected_epc")
            shinyjs::enable("runModel")
            shinyjs::enable("autoupdate")
            shinyjs::enable("sliders-container")
            shinyjs::hide("loading-spinner")
        }
    })

    # auto update function
    observe({
        req(input$autoupdate, sliderDebounce())

           if (isRunning()) return()
            isRunning(TRUE)
            on.exit(isRunning(FALSE))

        isolate({
            epc <- input$selected_epc

            paramVal <- sapply(1:nrow(parameters),function(x){
                                                input[[paste0("param_", x)]]
            })

            settings$epcInput[["normal"]] <- epc

            #print(paste("Updating EPC file", epc, "with new parameters before running model"))
            changeMuso(settings, paramVal, 
                 calibrationPar = parameters[, 2],
                 fileToChange = "epc", 
                 fixAlloc = FALSE)
        })
             

        isolate({
            if(input$destination == "auto") {
                outputList$prev <- outputList$nextVal
                outputList$nextVal <- calibMuso(
                    settings = settings,
                    calibrationPar = parameters[, 2],
                    parameters = paramVal
                )
            } else {
                outputList[[input$destination]] <- calibMuso(
                    settings = settings,
                    calibrationPar = parameters[, 2],
                    parameters = paramVal
                )
            }
        })
            
        
    }) %>% bindEvent(sliderDebounce()) #triggering the event upon slider change


        # hotkey insertions
        observeEvent(input$setHotkey, {
            tryCatch({
                hotkey <- trimws(input$hotkeyInput)
                if(hotkey == "") stop("Empty hotkey")
                
                parts <- strsplit(hotkey, "+", fixed = TRUE)[[1]]
                modifiers <- head(parts, -1)
                key <- tail(parts, 1)
                
                # Validating modifiers
                allowed_modifiers <- c("ctrl", "control", "shift", "alt")
                if(length(modifiers) > 0) {
                invalid <- !tolower(modifiers) %in% allowed_modifiers
                if(any(invalid)) {
                    stop(paste("Invalid modifier:", modifiers[invalid][1]))
                }
                }
                
                # Validating key
                if(nchar(key) != 1 && !tolower(key) %in% c("enter", "backspace", "tab")) {
                stop("Key must be a single character or special key")
                }

                
                js_code <- paste0("
                $(document).off('keydown.customHotkey').on('keydown.customHotkey', function(event) {
                    let match = true;
                    let key = '", tolower(key), "';
                    let expectedKey = key === 'enter' ? 'Enter' : key;
                ")
                
                if(length(modifiers) > 0) {
                for(mod in modifiers) {
                    mod <- trimws(tolower(mod))
                    js_code <- paste0(js_code, "
                    if(!event.", ifelse(mod == "ctrl", "ctrl", mod), "Key) match = false;
                    ")
                }
                }
                
                js_code <- paste0(js_code, "
                    if(match && event.key.toLowerCase() === expectedKey.toLowerCase()) {
                    $('#runModel').click();
                    event.preventDefault();
                    }
                });
                ")
                
                shinyjs::runjs(js_code)
                showNotification(paste("Hotkey set:", hotkey), type = "message")
            }, error = function(e) {
                showNotification(paste("Error:", e$message), type = "error")
            })
        })

            ################ PLOTTING ###############
                output$dynamicPlots <- renderUI({
                req(input$selected_vars)
                
                plot_outputs <- lapply(input$selected_vars, function(var) {
                    plotlyOutput(paste0("plot_", var), height = "100%")
                })
                do.call(tagList, plot_outputs)
                })

                

                output$dynamicPlots <- renderUI({
                req(input$selected_vars)
                
                plot_outputs <- lapply(input$selected_vars, function(var) {
                    plotlyOutput(paste0("plot_", var), height = "100%")
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
                    selectedYears <- input$yearRange  # single value
                } else {
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
                    
                # Adding the epc labels on the x axis
            #if (file.exists(planting_file)) {
                planting_dates <- rv$epc_dates
                #print(planting_dates)
                if (!is.null(planting_dates) && nrow(planting_dates) > 0) {
                selected_planting <- planting_dates %>%
                dplyr::filter(lubridate::year(DATE) %in% selectedYears)

            #print(paste0("Selected planting dates: ", selected_planting))
                if (nrow(selected_planting) > 0) {
                    for (i in 1:nrow(selected_planting)) {
                        current_date <- selected_planting$DATE[i]
                        current_epcs <- unlist(strsplit(selected_planting$CROP.file.[i], " +"))
                        if (input$singleYear || length(selectedYears) <= 3) {
                              
                                epc_labels <- sapply(current_epcs, function(epc) {
                                    idx <- which(rv$epc_files == epc)
                                    if (length(idx) > 0) rv$epc_labels[idx] else epc
                                })
                                label <- paste(unique(epc_labels), collapse = ", ")
                        }
                        else {
                        epc_numbers <- sapply(current_epcs, function(epc) {
                            idx <- which(rv$epc_files == epc)
                            if (length(idx) > 0) rv$epc_num_labels[idx] else epc
                        })
                        label <- paste(unique(epc_numbers), collapse = ", ")
                        }

                        p <- p %>% add_annotations(
                            x = current_date,
                            y = 0,                  
                            xref = "x",
                            yref = "paper",
                            text = "▼",            # changable symbol
                            showarrow = FALSE,
                            font = list(color = "green", size = 14)
                        ) %>% #epc labels
                        add_annotations(
                                x = current_date,
                                y = 0,               
                                xref = "x",
                                yref = "paper",
                                text = label,
                                showarrow = FALSE,
                                yshift = -7,         # shift label down
                                font = list(color = "green", size = 10)
                        )
                        
                         }}}
            #}



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

#' tuneMusoUI 
#' 
#' This is a simple parameter tuner function which works great in a flat directory system
#'
#' @param parameterFile optional, the parameter csv file
#' @importFrom shinyjs useShinyjs toggle show hide disable enable removeEvent runjs 
#' @importFrom dplyr filter %>% select full_join
#' @importFrom shinyjqui jqui_resizable 
#' @importFrom lubridate year month day 
#' @importFrom DT dataTableOutput datatable renderDataTable
#' @importFrom shinyWidgets pickerInput updatePickerInput
#' @importFrom plotly plotlyOutput renderPlotly layout add_trace add_annotations 
#' @importFrom shiny tags actionButton numericInput HTML checkboxInput titlePanel radioButtons textAreaInput fluidPage sidebarLayout sidebarPanel mainPanel getShinyOption tabsetPanel tabPanel tagList selectInput sliderInput renderUI div fileInput uiOutput updateSliderInput observe observeEvent validate need showNotification icon textInput isRunning reactiveVal reactiveValues isolate debounce bindEvent fluidRow column checkboxGroupInput showModal modalDialog modalButton removeModal h4
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
    
    # keeping the scrollbar at the same position after refresh upon model run
    scrollbar_position_retainer <- "
        Shiny.addCustomMessageHandler('save_scroll', function(message) {
        var scrollDiv = document.getElementById(message.id);
        if (scrollDiv) {
            Shiny.setInputValue(message.id + '_scroll', scrollDiv.scrollTop, {priority: 'event'});
        }
        });

        Shiny.addCustomMessageHandler('restore_scroll', function(message) {
        var scrollDiv = document.getElementById(message.id);
        if (scrollDiv) {
            scrollDiv.scrollTop = message.scroll;
        }
        });
        "

        hide_plot_area <- "
            $(document).ready(function() {
                Shiny.addCustomMessageHandler('toggle_plot_visibility', function(message) {
                    let plotPanel = $('#plotPanel');
                    let controlPanel = $('#controlPanel');
                    let button = $('#toggle_plot_field');
                    let standardSliders = $('#standardSliders'); // for non-grouped sliders
                    let dependentContainers = $('.dependentSliderContainer'); // for grouped sliders
                    let parametersLayout = $('#parametersLayout'); // container for controls
                    let hotkeyContainer = $('#hotkeyContainer');
                    
                    plotPanel.toggle();
                    if (plotPanel.is(':visible')) {
                        button.text('Hide Plot Area');
                        button.find('i').removeClass('eye').addClass('eye-slash');
                        controlPanel.css({'flex': '0 1 auto', 'max-width': '450px'});
                        standardSliders.removeClass('expanded');
                        dependentContainers.removeClass('expanded');
                        parametersLayout.removeClass('expanded');
                        $('#hotkeyOriginal').append(hotkeyContainer);
                        hotkeyContainer.find('#runMusoExtra').show();
                        Shiny.setInputValue('plotHidden', false);
                    } else {
                        button.text('Show Plot Area');
                        button.find('i').removeClass('eye-slash').addClass('eye');
                        controlPanel.css({'flex': '1 1 100%', 'max-width': 'none'});
                        standardSliders.addClass('expanded');
                        dependentContainers.addClass('expanded');
                        parametersLayout.addClass('expanded');
                        $('.colRight').append(hotkeyContainer);
                        hotkeyContainer.find('#runMusoExtra').hide();
                        Shiny.setInputValue('plotHidden', true);
                    }
                    $(window).trigger('resize');
                });
            });
        "



 fluidPage(
  useShinyjs(),
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
          height: calc(100vh - 30px);
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
          flex: 0 1 auto;
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

      /* Standard sliders layout */
      #standardSliders .form-group {
          display: block;
          width: 100%;
      }
      #standardSliders.expanded .form-group {
          display: inline-block;
          width: 32%;
          margin-right: 1%;
          vertical-align: top;
      }

      /* Dependent sliders layout */
      .dependentSliderContainer .slider-col {
          display: inline-block;
          vertical-align: top;
          width: 48%;
          margin-right: 4%;
          box-sizing: border-box;
      }
      .dependentSliderContainer.expanded .slider-col {
          width: 23%;
          margin-right: 2%;
      }
      .dependentSliderContainer .slider-col:nth-child(2n) {
          margin-right: 0;
      }
      .dependentSliderContainer.expanded .slider-col:nth-child(4n) {
          margin-right: 0;
      }

      /* Parameters layout */
      #parametersLayout {
          display: block;
      }
      #parametersLayout .colLeft,
      #parametersLayout .colRight {
          width: 100%;
          box-sizing: border-box;
          margin-bottom: 10px;
      }
      #parametersLayout.expanded {
          display: flex;
          flex-direction: row;
          justify-content: space-between;
      }
      #parametersLayout.expanded .colLeft,
      #parametersLayout.expanded .colRight {
          width: 50%;
          margin-bottom: 0;
      }
  "))),
    
    tags$head(tags$script(HTML(paste(scrollbar_position_retainer, hide_plot_area, sep = "\n")))),

    # moving the title to the right so the toggleui button has space
    titlePanel(div(style = "margin-left: 100px;", "Biome-BGCMuSo Parameter Tuner")),
    
    # Floating toggle button to collapse/restore the control panel
    div(
      id = "toggleUIButton",
      actionButton("toggleUI", label = "Toggle UI", icon = icon("bars"))
    ),
    # toggle legend button for... toggling the legend. And also toggle plot field for expanded ui
    div(
        style = "position: absolute; top: 10px; right: 10px; z-index: 1000; display: flex; gap: 10px;",
        actionButton("toggle_plot_field", "Hide Plot Area"),
        actionButton("toggle_legend", "Hide Legend", icon = icon("eye-slash"))
    ),

    
    # Default keyboard shortcut for running the model
    tags$script(HTML("
      $(document).on('keydown', function(event) {
          if (event.ctrlKey && event.key === 'Enter') {
              $('#runModel').click();
          }
      });
    ")),
    
    # Mouse3 as an additional (permanent) hotkey to run the model (unaffected by later changes in hotkeys)
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
                                   # Container for two columns when UI is expanded
                                   div(id = "parametersLayout",
                                       # Left column: top Run Muso button, variable picker, and file input
                                       div(class = "colLeft",
                                           div(style = "margin-top: 10px;",
                                               actionButton("runModel", "Run Muso",
                                                style = "background-color: red; color: white; border-color: red;")
                                           ),
                                           tags$div(
                                               id = "controlp",
                                               pickerInput(
                                                   inputId = "selected_vars",
                                                   label = "Select output variables (multiple can be chosen)",
                                                   choices = settings$dailyOutputTable$name, 
                                                   multiple = TRUE,
                                                   options = list(`actions-box` = TRUE)
                                               )
                                           ),
                                           fileInput("measurementFile", "Upload Measurement File", 
                                                     accept = c(".txt"), multiple = TRUE)
                                       ),
                                       # Right column: checkboxes, single year, year range.
                                       div(class = "colRight",
                                           checkboxInput("autoupdate", "Automatic update"),
                                           checkboxInput("singleYear", "Single year mode", value = FALSE),
                                           uiOutput("yearRangeUI")
                                       )
                                   ),
                                   uiOutput("selectEPC"),
                                   # Reset buttons
                                   tags$div(
                                       style = "display: flex; align-items: center; gap: 10px;", 
                                       actionButton("resetParams", "Reset to originals"),
                                       checkboxInput("restoreOnExit", "Restore originals on exit", value = FALSE)
                                   ),
                                   tags$div(id = "controlp",
                                            tags$div(id = "slider-container", uiOutput("param_sliders"))
                                   ),
                                   # Hotkey container placeholder & container
                                div(id = "hotkeyOriginal",
                                       div(id = "hotkeyContainer",
                                           # First row: hotkey input
                                           tags$div(
                                               textInput("hotkeyInput", "Set Hotkey for model run", 
                                                         value = "Ctrl+Enter", placeholder = "e.g. Ctrl+Enter")
                                           ),
                                           # Second row: Apply Hotkey and extra Run Muso button side-by-side
                                           tags$div(
                                               style = "display: flex; gap: 10px; align-items: center;",
                                               actionButton("setHotkey", "Apply Hotkey"),
                                               actionButton("runMusoExtra", "Run Muso",
                                                style = "background-color: red; color: white; border-color: red;")
                                           )
                                       )
                                   ),
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
                          ),
                          tabPanel("Measurement Manager",
                            fluidRow(
                                column(12,
                                DT::dataTableOutput("measurementTable")
                                ),
                                actionButton("outputMapping", "Output Mapping",
                                style = "background-color: blue; color: white; border-color: black;"),

                                    column(4,
                                    h4("Delete Columns"),
                                    checkboxGroupInput("colsToDelete", "Select columns to delete:", choices = NULL),
                                    actionButton("deleteCols", "Delete Selected Columns")
                                )
                            )
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

    
    parameters <- parameters[!is.na(parameters$ABREVIATION) & parameters$ABREVIATION != "", ]
    # indexing the rows for the allocation parameters (if they exist)
    parameters$group <- ifelse(grepl("^(132|133|134|135)\\.", parameters$INDEX),
                           # Remove the "132.", "133.", "134." or "135." prefix,
                           sub("^(132|133|134|135)\\.", "", parameters$INDEX),
                           NA)

    required_main <- c(132, 133, 134, 135)

    # Find the unique dependent groups already in the CSV.
    dep_groups <- unique(parameters$group[!is.na(parameters$group)])

    # Loop over each dependent group and check for each required main index to see if any of the 3 dependent rows are missing
    for (g in dep_groups) {
        for (m in required_main) {
            # Construct the expected INDEX value 
            expected_index <- paste0(m, ".", g)
            # Check if this expected_index is present in the parameters data frame
            if (!(expected_index %in% parameters$INDEX)) {
            # The row is missing, we will append a new row
            
            new_row <- data.frame(
                ABREVIATION = paste("Missing", expected_index),
                INDEX = as.numeric(expected_index),
                min = 0,   
                max = 1,    
                group = g,
                stringsAsFactors = FALSE
            )
            parameters <- rbind(parameters, new_row)
            message("Added missing parameter row for ", expected_index, " for it was not found in parameters.csv")
            }
        }
    }


      # Read the measurement file
    measurementData <- reactiveVal(NULL)


observeEvent(input$measurementFile, {
  req(input$measurementFile)
  
  files <- input$measurementFile
  
  # Read each file using column indexes.
  new_data_list <- lapply(seq_len(nrow(files)), function(i) {
    # Read file with header = TRUE.
    df <- read.table(files$datapath[i], header = TRUE, stringsAsFactors = FALSE)
    
    # Create a Date column from columns 1, 2, and 3 (assumed to be yyyy, mm, dd).
    df$Date <- as.Date(paste(df[[1]], df[[2]], df[[3]], sep = "-"), format = "%Y-%m-%d")
    df[df == -9999] <- NA
    # Extract measurement data from columns 4 onward.
    meas <- df[ , -(1:3), drop = FALSE]
    # In case the file header includes a "Date" column in these measurement columns,
    # remove it so that only our computed Date column remains.
    meas <- meas[, !(colnames(meas) %in% c("Date")), drop = FALSE]
    
    # Create a new data frame with only one Date column and the measurement data.
    new_df <- data.frame(Date = df$Date, meas, stringsAsFactors = FALSE)
    return(new_df)
  })
  
  # Combine the new data frames by full joining on Date.
  new_data_combined <- Reduce(function(x, y) {
    dplyr::full_join(x, y, by = "Date")
  }, new_data_list)
  
  # Create a complete sequence of dates from the minimum to maximum date.
  all_dates <- seq.Date(min(new_data_combined$Date, na.rm = TRUE),
                        max(new_data_combined$Date, na.rm = TRUE),
                        by = "day")
  base_df <- data.frame(Date = all_dates)
  
  # Merge to ensure that every date in the full range is present (missing measurement values become NA).
  new_data_complete <- dplyr::full_join(base_df, new_data_combined, by = "Date")
  
  # Update the cumulative data: if no data exists yet, use the new data; otherwise, merge.
  if (is.null(measurementData())) {
    measurementData(new_data_complete)
  } else {
    combined <- dplyr::full_join(measurementData(), new_data_complete, by = "Date")
    measurementData(combined)
  }
})



            observe({
            req(measurementData())
            # Get all column names except "Date" (if you want to keep Date always)
            cols <- setdiff(colnames(measurementData()), "Date")
            updateCheckboxGroupInput(session, "colsToDelete", choices = cols, selected = character(0))
            })

            # Render the measurement table.
            output$measurementTable <- DT::renderDataTable({
            req(measurementData())
            #trying to display NAs
            df <- measurementData()
            df_display <- df
            df_display[is.na(df_display)] <- "NA"
            DT::datatable(df_display, editable = FALSE,
                            options = list(pageLength = 10, scrollY = "400px", autoWidth = TRUE),
                            rownames = FALSE)
            })

            # When the user clicks the delete button, remove the selected columns.
            observeEvent(input$deleteCols, {
            req(measurementData())
            colsToRemove <- input$colsToDelete
            if(length(colsToRemove) > 0){
                # Remove the selected columns from the data frame.
                df <- measurementData()
                df <- df[, !(colnames(df) %in% colsToRemove), drop = FALSE]
                measurementData(df)
                
                # Update the checkbox group input to reflect the new column names.
                updateCheckboxGroupInput(session, "colsToDelete", choices = setdiff(colnames(df), "Date"), selected = character(0))
            }
            })

        mappingRV <- reactiveVal(NULL)
            observeEvent(input$outputMapping, {
            req(measurementData())
            
            # Get measurement columns (all except "Date")
            measCols <- setdiff(colnames(measurementData()), "Date")
            
            # Get available output variables from your settings
            availableOutputVars <- settings$dailyOutputTable$name
            
            # Create the modal's content:
            modalContent <- tagList(
                h3("Map Measurement Columns to Output Variables"),
                # Header row:
                fluidRow(
                column(6, strong("Measurement Column")),
                column(6, strong("Mapped Output Variable"))
                ),
                # For each measurement column, create a row with the column name and a dropdown.
                lapply(measCols, function(col) {
                fluidRow(
                    column(6, div(style = "padding: 5px;", col)),
                    column(6, 
                    selectInput(
                        inputId = paste0("mapping_", col),
                        label = NULL,
                        choices = c("None", availableOutputVars),
                        # Use the saved mapping if it exists, otherwise default to "None"
                        selected = if (!is.null(mappingRV()) && !is.null(mappingRV()[[col]])) {
                        mappingRV()[[col]]
                        } else {
                        "None"
                        },
                        width = "100%"
                    )
                    )
                )
                })
            )
            
            showModal(modalDialog(
                modalContent,
                title = "Output Mapping",
                size = "l",
                footer = tagList(
                modalButton("Cancel"),
                actionButton("saveMapping", "Save Mapping")
                ),
                easyClose = TRUE
            ))
            })

        observeEvent(input$saveMapping, {
            req(measurementData())
            
            # Get measurement columns (all except "Date")
            measCols <- setdiff(colnames(measurementData()), "Date")
            
            # Collect the mapping for each column
            mapping <- sapply(measCols, function(col) {
                input[[paste0("mapping_", col)]]
            }, simplify = FALSE)
            
            # Save the mapping in the reactive value
            mappingRV(mapping)
            
            # Optionally, you can print mappingRV() to verify
            #print(mappingRV())
            
            # Close the modal
            removeModal()
            })


    # EPC HANDLING
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

    # making the first output variable the initial selected variable upon opening the app    
    observe({
        req(settings$dailyOutputTable$name)  
        updatePickerInput(
            session, 
            inputId = "selected_vars", 
            selected = settings$dailyOutputTable$name[1]  
        )
    })



        #ui for epc selection
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

    # making a storage for the previous epc file
    prevEPC <- reactiveVal(NULL)

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
            if (is.na(parameters$group[i])) {
            # Standard (non-grouped) parameter: update slider with id "param_i"
            updateSliderInput(session, paste0("param_", i), value = defaults[i])
            } else {
            # Dependent (grouped) parameter: update slider with id "dep_<INDEX>"
            updateSliderInput(session, paste0("dep_", parameters$INDEX[i]), value = defaults[i])
            }
        }
        print(paste0("Reset sliders to initials for ", epc))
    })


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

        # reactive value that will track the locked or unlock state of the allocation locking button
        lockStates <- reactiveValues()

        # making the slider ui (both standard and dependent)
        output$param_sliders <- renderUI({
            req(input$selected_epc)
            
            vals <- currentValues()
            if(length(vals) < nrow(parameters) || any(is.na(vals))) {
                vals <- defaultValues()
            }
            
            dep_indices <- which(!is.na(parameters$group))
            non_dep_indices <- setdiff(seq_len(nrow(parameters)), dep_indices)
            
            standard_sliders <- lapply(non_dep_indices, function(i) {
                safe_value <- if (is.null(vals[i]) || is.na(vals[i])) parameters[i, 3] else vals[i]
                sliderInput(
                paste0("param_", i),
                label = parameters$ABREVIATION[i],
                min   = parameters[i, 3],
                max   = parameters[i, 4],
                value = safe_value,
                step  = (parameters[i, 4] - parameters[i, 3]) / 100
                )
            })
            
            dep_groups <- unique(parameters$group[dep_indices])
            
            dependent_sliders <- lapply(dep_groups, function(g) {
                group_rows <- which(!is.na(parameters$group) &
                                    parameters$group == g &
                                    as.numeric(sub("\\..*", "", parameters$INDEX)) %in% c(132,133,134,135))
                group_rows <- group_rows[order(as.numeric(sub("\\..*", "", parameters$INDEX[group_rows])))]
                
                slider_list <- lapply(group_rows, function(i) {
                safe_value <- if (is.null(vals[i]) || is.na(vals[i])) parameters[i, 3] else vals[i]
                slider_id <- paste0("dep_", parameters$INDEX[i])
                lock_btn_id <- paste0("lock_", parameters$INDEX[i])
                div(
                    sliderInput(
                    inputId = slider_id,
                    label   = parameters$ABREVIATION[i],
                    min     = 0,
                    max     = 1,
                    value   = safe_value,
                    step    = 0.01
                    ),
                    actionButton(lock_btn_id, label = NULL, icon = icon("unlock"),
                                style = "margin-top: -10px; margin-bottom: 10px;")
                )
                })
                
                # condition whether hide plot area is active or not (so upon epc switching the sliders will retain their aligments)
                containerClass <- if (!is.null(input$plotHidden) && input$plotHidden) "dependentSliderContainer expanded" else "dependentSliderContainer"
                
                tagList(
                h4(paste("Allocation Group", g)),
                    div(style = "margin-bottom: 10px;",
                        div(style = "display: inline-block; vertical-align: middle;",
                            checkboxInput(inputId = paste0("autoCalc_", g), label = "Auto‑calc", value = TRUE)
                        ),
                        div(style = "display: inline-block; vertical-align: middle; margin-left: 0px;",
                            textOutput(paste0("sumCounter_", g), container = span)
                        )
                    ),
                div(class = containerClass,
                    lapply(slider_list, function(slider) {
                        div(class = "slider-col", slider)
                    })
                )
                )
            })
            
            tagList(
                div(id = "standardSliders", class = if (!is.null(input$plotHidden) && input$plotHidden) "expanded" else "", standard_sliders),
                dependent_sliders
            )
            })


        observe({
            req(input$selected_epc)
            dep_indices <- which(!is.na(parameters$group))
            # For each dependent parameter, set its lock state if not already set.
            for(i in dep_indices) {
                slider_id <- paste0("dep_", parameters$INDEX[i])
                if (is.null(lockStates[[slider_id]]))
                lockStates[[slider_id]] <- FALSE
            }
            })

        # lock button observer
        observe({
            req(input$selected_epc)
            dep_indices <- which(!is.na(parameters$group))
            
            lapply(parameters$INDEX[dep_indices], function(idx) {
                slider_id <- paste0("dep_", idx)
                lock_btn_id <- paste0("lock_", idx)
                
                # Only attach if the input for the lock button exists.
                if (!is.null(input[[lock_btn_id]])) {
                observeEvent(input[[lock_btn_id]], {
                    # Toggle the lock state
                    lockStates[[slider_id]] <- !lockStates[[slider_id]]
                    # Update the button icon accordingly:
                    new_icon <- if (lockStates[[slider_id]]) "lock" else "unlock"
                    updateActionButton(session, lock_btn_id, icon = icon(new_icon))
                }, ignoreInit = TRUE)
                }
            })
            })

        # sum to 1 counter for allocation
        observe({
        req(input$selected_epc)
        
        tol <- 1e-6  # small tolerance to avoid oscillation
        
        # Get the unique groups
        dep_groups <- unique(parameters$group[!is.na(parameters$group)])
        
        lapply(dep_groups, function(g) {
            # For group g, get the rows and slider IDs
            group_rows <- which(!is.na(parameters$group) &
                                parameters$group == g &
                                as.numeric(sub("\\..*", "", parameters$INDEX)) %in% c(132,133,134,135))
            group_rows <- group_rows[order(as.numeric(sub("\\..*", "", parameters$INDEX[group_rows])))]
            ids <- paste0("dep_", parameters$INDEX[group_rows])
            
            # A flag to prevent recursive updates
            groupUpdating <- reactiveVal(FALSE)
            
            for(i in seq_along(ids)) {
            local({
                j <- i
                slider_id <- ids[j]
                debouncedSliderVal <- reactive({ input[[slider_id]] }) %>% debounce(500)
                
                observeEvent(debouncedSliderVal(), {
                    if (groupUpdating()) return()
                    groupUpdating(TRUE)
                    
                    # Only auto-calc if the auto-calc checkbox is checked for this group
                    if (isTRUE(input[[paste0("autoCalc_", g)]])) {
                        # Compute total locked for the whole group
                        locked_vals <- unlist(lapply(ids, function(x) {
                        if (isTRUE(lockStates[[x]])) {
                            if (is.null(input[[x]])) 0 else as.numeric(input[[x]])
                        } else 0
                        }))
                        L <- sum(locked_vals)
                        available_total <- 1 - L
                        
                        new_val <- as.numeric(debouncedSliderVal())
                        # Clamp if new_val exceeds available_total
                        if (new_val >= available_total - tol) {
                        new_val <- available_total
                        updateSliderInput(session, slider_id, value = new_val)
                        # Set all other unlocked sliders to 0
                        for (other in ids[-j]) {
                            if (!isTRUE(lockStates[[other]]))
                            updateSliderInput(session, other, value = 0)
                        }
                        } else {
                        # Distribute the remaining available among the other unlocked sliders
                        remaining_available <- available_total - new_val
                        other_ids <- ids[-j][ !sapply(ids[-j], function(x) isTRUE(lockStates[[x]])) ]
                        current_unlocked <- unlist(lapply(other_ids, function(x) {
                            if (is.null(input[[x]])) 0 else as.numeric(input[[x]])
                        }))
                        total_unlocked <- sum(current_unlocked)
                        if (length(other_ids) > 0) {
                            if (total_unlocked == 0) {
                            new_unlocked <- rep(remaining_available / length(other_ids), length(other_ids))
                            } else {
                            new_unlocked <- unname(remaining_available * (current_unlocked / total_unlocked))
                            }
                            for (k in seq_along(other_ids)) {
                            updateSliderInput(session, other_ids[k], value = new_unlocked[k])
                            }
                    }
                    }
                }
                groupUpdating(FALSE)
                }, ignoreInit = TRUE)
            })
            }
        })
        })

        observe({
        req(input$selected_epc)
        
        dep_groups <- unique(parameters$group[!is.na(parameters$group)])
        
        lapply(dep_groups, function(g) {
            observeEvent(input[[paste0("autoCalc_", g)]], {
            # When autoCalc is toggled on, perform a recalculation for group g.
            if (isTRUE(input[[paste0("autoCalc_", g)]])) {
                group_rows <- which(!is.na(parameters$group) &
                                    parameters$group == g &
                                    as.numeric(sub("\\..*", "", parameters$INDEX)) %in% c(132,133,134,135))
                group_rows <- group_rows[order(as.numeric(sub("\\..*", "", parameters$INDEX[group_rows])))]
                ids <- paste0("dep_", parameters$INDEX[group_rows])
                
                # Calculate total locked and available for unlocked.
                locked_vals <- unlist(lapply(ids, function(x) {
                if (isTRUE(lockStates[[x]])) {
                    if (is.null(input[[x]])) 0 else as.numeric(input[[x]])
                } else 0
                }))
                L <- sum(locked_vals)
                available_total <- 1 - L
                
                # For unlocked sliders, recalculate their values proportionally.
                unlocked_ids <- ids[ !sapply(ids, function(x) isTRUE(lockStates[[x]])) ]
                current_unlocked <- unlist(lapply(unlocked_ids, function(x) {
                if (is.null(input[[x]])) 0 else as.numeric(input[[x]])
                }))
                total_unlocked <- sum(current_unlocked)
                if (length(unlocked_ids) > 0) {
                if (total_unlocked == 0) {
                    new_unlocked <- rep(available_total / length(unlocked_ids), length(unlocked_ids))
                } else {
                    new_unlocked <- unname(available_total * (current_unlocked / total_unlocked))
                }
                for (x in seq_along(unlocked_ids)) {
                    updateSliderInput(session, unlocked_ids[x], value = new_unlocked[x])
                }
                }
            }
            }, ignoreInit = TRUE)
        })
        })


        observe({
        req(input$selected_epc)
        
        dep_groups <- unique(parameters$group[!is.na(parameters$group)])
        lapply(dep_groups, function(g) {
            group_rows <- which(!is.na(parameters$group) &
                                parameters$group == g &
                                as.numeric(sub("\\..*", "", parameters$INDEX)) %in% c(132,133,134,135))
            group_rows <- group_rows[order(as.numeric(sub("\\..*", "", parameters$INDEX[group_rows])))]
            ids <- paste0("dep_", parameters$INDEX[group_rows])
            
            output[[paste0("sumCounter_", g)]] <- renderText({
            vals <- unlist(lapply(ids, function(x) {
                if (is.null(input[[x]])) 0 else as.numeric(input[[x]])
            }))
            total <- sum(vals)
            if (total > 1) {
                paste0("Total: ", round(total, 2), " (Warning: Sum > 1!)")
            } else {
                paste0("Total: ", round(total, 2))
            }
            })
        })
        })


        observe({
        req(input$selected_epc)
        dep_groups <- unique(parameters$group[!is.na(parameters$group)])
        lapply(dep_groups, function(g) {
            group_rows <- which(!is.na(parameters$group) &
                                parameters$group == g &
                                as.numeric(sub("\\..*", "", parameters$INDEX)) %in% c(132, 133, 134, 135))
            group_rows <- group_rows[order(as.numeric(sub("\\..*", "", parameters$INDEX[group_rows])))]
            ids <- paste0("dep_", parameters$INDEX[group_rows])
            
            output[[paste0("sumCounter_", g)]] <- renderText({
            vals <- unlist(lapply(ids, function(x) {
                if (is.null(input[[x]])) 0 else as.numeric(input[[x]])
            }))
            total <- sum(vals)
            if (total > 1) {
                paste0("Total: ", round(total, 2), " (Warning: Sum > 1!)")
            } else {
                paste0("Total: ", round(total, 2))
            }
            })
        })
        })





    # creating tracker that will avoid auto-update from running the model upon epc switching (not yet used later)
    updatingEPC <- reactiveVal(FALSE)
    
        # saving epc values upon epc change, updating sliders
        observeEvent(input$selected_epc, {
            req(input$selected_epc)
            new_epc <- input$selected_epc

            # Saving the previous EPC's slider values 
            old_epc <- prevEPC()
            if (!is.null(old_epc) && old_epc != new_epc) {
                # Retrieving the stored values for the old EPC, if not available, use defaultValues.
                updated_old <- epcValues[[old_epc]]
                if (is.null(updated_old) || length(updated_old) < nrow(parameters))
                updated_old <- defaultValues()
                
                # Updating standard sliders (those with group == NA)
                non_dep_indices <- which(is.na(parameters$group))
                for (i in non_dep_indices) {
                    slider_val <- input[[paste0("param_", i)]]
                    if (!is.null(slider_val) && length(slider_val) > 0) {
                        updated_old[i] <- slider_val
                    }
                }
                
                # Updating dependent sliders (those with a group)
                dep_indices <- which(!is.na(parameters$group))
                for (i in dep_indices) {
                    inputId <- paste0("dep_", parameters$INDEX[i])
                    slider_val <- input[[inputId]]
                    if (!is.null(slider_val) && length(slider_val) > 0) {
                        updated_old[i] <- slider_val
                    }
                }
                epcValues[[old_epc]] <- updated_old
                
                # Saving the previous EPC's values to file
                settings$epcInput[["normal"]] <- old_epc
                changeMuso(settings, updated_old, calibrationPar = parameters[, 2],
                        fileToChange = "epc", fixAlloc = FALSE)
                print(paste("Saved changes for", old_epc))
            }
            
            # Initializing the new EPC's values if needed
            if (is.null(epcValues[[new_epc]]) || length(epcValues[[new_epc]]) < nrow(parameters)) {
                epcValues[[new_epc]] <- defaultValues()
            }
            newVals <- epcValues[[new_epc]]
            
            # Updating all slider inputs for the new EPC
            for (i in seq_len(nrow(parameters))) {
                if (is.na(parameters$group[i])) {
                # Standard slider
                    updateSliderInput(session, paste0("param_", i), value = newVals[i])
                } else {
                # Dependent slider (using its INDEX-based input ID)
                    updateSliderInput(session, paste0("dep_", parameters$INDEX[i]), value = newVals[i])
                }
            }
            
            # Updating the tracker for the previous EPC
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


    observeEvent(input$toggle_plot_field, {
        session$sendCustomMessage("toggle_plot_visibility", list())
    })



    # desperate try to save epc values on model run with this overkill of a function since the solution is probably something easy but my head can't get around it as of 14:08 CET, 2025.02.11 but at least it works, alright?
    updateCurrentEPCValues <- function() {
        req(input$selected_epc)
        epc <- input$selected_epc
        # Retrieving the current vector, if missing, fall back to defaultValues
        updated <- epcValues[[epc]]
        if (is.null(updated) || length(updated) < nrow(parameters))
            updated <- defaultValues()
        
        # Updating standard (non-dependent) slider values
        non_dep_indices <- which(is.na(parameters$group))
        for (i in non_dep_indices) {
            slider_val <- input[[paste0("param_", i)]]
            if (!is.null(slider_val) && length(slider_val) > 0) {
            updated[i] <- slider_val
            }
        }
        
        # Updating dependent slider values
        dep_indices <- which(!is.na(parameters$group))
        for (i in dep_indices) {
            inputId <- paste0("dep_", parameters$INDEX[i])
            slider_val <- input[[inputId]]
            if (!is.null(slider_val) && length(slider_val) > 0) {
            updated[i] <- slider_val
            }
        }
        
        epcValues[[epc]] <<- updated  # Updating the reactive storage
    }




    observeEvent(input$runModel, {
        req(input$selected_epc)
        epc <- input$selected_epc

        # forcing an update with the "over-kill" function
        updateCurrentEPCValues()

        paramVal <- epcValues[[epc]]

        # saving scroll position
        session$sendCustomMessage("save_scroll", list(id = "plotPanel"))
        
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

        # restoring scrollbar position
        observe({
            if (!is.null(input$plotPanel_scroll)) {
            session$sendCustomMessage("restore_scroll", list(id = "plotPanel", scroll = input$plotPanel_scroll))
            }
        })


    
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

           paramVal <- epcValues[[input$selected_epc]]


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


            # Reactive value to track legend visibility
            legendVisible <- reactiveVal(TRUE)  # Default: legend is shown

            # Toggle legend state when button is clicked
            observeEvent(input$toggle_legend, {
                new_state <- !legendVisible()
                legendVisible(new_state) 
                updateActionButton(session, "toggle_legend", label = ifelse(legendVisible(), "Hide Legend", "Show Legend"),icon = icon(ifelse(new_state, "eye-slash", "eye")))
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
                            # adding invisible markers for epc legend
                            p <- p %>% add_trace(
                                x = selected_planting$DATE[1],  
                                y = 0,  
                                type = 'scatter',
                                mode = 'markers',
                                marker = list(symbol = "triangle-down", color = "green", size = 10),
                                name = "Planting Dates",
                                visible = "legendonly" 
                            )

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
                            text = "▼",          
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
                        
                         } 
                }}
            #}

                    # adding measurements for the current variable (var)
                    mapping <- mappingRV()
                    df <- measurementData()
                    # Filter data by selected years
                    df_filtered <- df[format(df$Date, "%Y") %in% selectedYears, ]

                    if (!is.null(mapping)) {
                        # Find measurement columns mapped to the current var
                        mappedCols <- names(mapping)[mapping == var]
                        
                        if (length(mappedCols) > 0) {
                            # Add each mapped measurement column
                            for (col in mappedCols) {
                                yData <- df_filtered[[col]]
                                
                                # Convert negatives to NA for GPP and TR (might be obsolete if data frame manipulation is added)
                                if (var %in% c("GPP", "TR")) {
                                    yData[yData < 0] <- NA
                                }
                                
                                p <- add_trace(p,
                                            x = df_filtered$Date,
                                            y = yData,
                                            type = 'scatter',
                                            mode = 'markers',
                                            name = paste(col, "Measurement"),
                                            marker = list(symbol = "circle", size = 7, color = "#337a12"))
                            }
                        }
                    }
                    p <- p %>% plotly::layout(
                        #title = list(text = paste("Plot of", var),
                        #    font = list(size = 16, color = "black")),
                        #xaxis = list(title = "Date"),
                        yaxis = list(title = var)
                        )

                    p <- p %>% plotly::layout(
                        yaxis = list(title = var),
                        showlegend = legendVisible()  # Conditionally show/hide legend
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

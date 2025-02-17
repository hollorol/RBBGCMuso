#' tuneMusoUI 
#' 
#' This is a simple parameter tuner function which works great in a flat directory system
#'
#' @param parameterFile optional, the parameter csv file
#' @importFrom shinyjs useShinyjs toggle show hide disable enable removeEvent runjs
#' @importFrom dplyr filter %>% select full_join
#' @importFrom shinyjqui jqui_resizable 
#' @importFrom lubridate year month day 
#' @importFrom data.table fread fwrite
#' @importFrom DT dataTableOutput datatable renderDataTable
#' @importFrom shinyWidgets pickerInput updatePickerInput
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom plotly plotlyOutput renderPlotly layout add_trace add_annotations 
#' @importFrom shiny tags actionButton numericInput HTML checkboxInput titlePanel radioButtons textAreaInput fluidPage sidebarLayout sidebarPanel mainPanel getShinyOption tabsetPanel tabPanel tagList selectInput sliderInput renderUI div fileInput uiOutput updateSliderInput observe observeEvent validate need showNotification icon textInput isRunning reactiveVal reactiveValues isolate debounce bindEvent fluidRow column checkboxGroupInput showModal modalDialog modalButton removeModal h4 downloadButton downloadHandler
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

    # expanded window
    expandedWindow <- "
            $(document).ready(function(){
            window.moveTo(0, 0);
            window.resizeTo(screen.width, screen.height);
        });
    "

    fullscreen <- "
      function toggleFullscreen() {
        if (!document.fullscreenElement && 
            !document.mozFullScreenElement && 
            !document.webkitFullscreenElement && 
            !document.msFullscreenElement) {
          // Enter full screen
          var elem = document.documentElement;
          if (elem.requestFullscreen) {
            elem.requestFullscreen();
          } else if (elem.mozRequestFullScreen) { 
            elem.mozRequestFullScreen();
          } else if (elem.webkitRequestFullscreen) { 
            elem.webkitRequestFullscreen(Element.ALLOW_KEYBOARD_INPUT);
          } else if (elem.msRequestFullscreen) {
            elem.msRequestFullscreen();
          }
        } else {
          // Exit full screen
          if (document.exitFullscreen) {
            document.exitFullscreen();
          } else if (document.mozCancelFullScreen) { 
            document.mozCancelFullScreen();
          } else if (document.webkitExitFullscreen) { 
            document.webkitExitFullscreen();
          } else if (document.msExitFullscreen) {
            document.msExitFullscreen();
          }
        }
      }
      
      // Bind the toggle function to the button click event
      $(document).on('click', '#fullscreen_btn', function(){
        toggleFullscreen();
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
       /* Initially hidden hover area for big plot changes*/
        #hoverSliderContainer {
        position: fixed;
        bottom: 0;
        left: 0;
        width: 100%;
        height: 10px; /* Small height initially */
        background: rgba(0, 0, 0, 0.2); /* Transparent black */
        text-align: center;
        transition: height 0.3s ease-in-out;
        z-index: 1000;
        display: none; /* HIDE by default */
        }

        /* Expanded area when hovered */
        #hoverSliderContainer:hover {
        height: 60px; /* Expand when hovered */
        background: rgba(0, 0, 0, 0.8); /* Darker background */
        }

        /* Inner div for the year slider */
        #yearSliderContent {
        display: none; /* Hidden by default */
        color: white;
        padding-top: 10px;
        }

        /* Show content when hovered */
        #hoverSliderContainer:hover #yearSliderContent {
        display: block;
        }

        /* Base lock styles */
        .btn.lock-btn {
            background-color: transparent;
            border: none;
            padding: 5px;
            margin-top: -10px;
            margin-bottom: 10px;
            border-radius: 50%;
            width: 30px;
            height: 30px;
            display: flex;
            align-items: center;
            justify-content: center;
            transition: background-color 0.3s ease;
        }

        /* Locked state */
        .btn.lock-btn.locked {
            background-color: #ff4444 !important;
            border-color: #ff4444 !important;
        }

        /* Unlocked state */
        .btn.lock-btn.unlocked {
            background-color: #44ff44 !important;
            border-color: #44ff44 !important;
        }
        /* Make the tab headers sticky within the control panel */
        #controlPanel .nav-tabs {
            position: sticky;
            top: 0;
            z-index: 1100;
            background-color: #fff; /* ensures it covers content underneath */
            border-bottom: 1px solid #ddd;
            margin-bottom: 0; /* remove extra spacing so content doesn't slide under */
        }
        body.modal-open #controlPanel .nav-tabs {
            display: none;
        }
  "))),
    
    # year slider for the hover area
    #div(
    #id = "hoverSliderContainer",
    #div(id = "yearSliderContent", uiOutput("yearRangeUI"))
    #),

    tags$head(tags$script(HTML(paste(scrollbar_position_retainer, hide_plot_area, expandedWindow, fullscreen, sep = "\n"))),
    
    tags$title("Biome-BGCMuSo Parameter Tuner")
    ),


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
        actionButton("settings_btn", label = NULL, icon = icon("cog")),
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
                                           fileInput("measurementFile", "Upload Measurement Files", 
                                                     accept = c(".txt",".csv"), multiple = TRUE)
                                       ),
                                       # Right column: checkboxes, single year, year range.
                                       div(class = "colRight",
                                           checkboxInput("autoupdate", "Automatic update"),
                                            div(style = "display: flex; align-items: center; gap: 0px;",
                                            checkboxInput("singleYear", "Single year mode", value = FALSE),
                                            checkboxInput("auto_epc_selection", "Auto EPC selection in single year mode", value = TRUE)
                                            ),
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
                                column(12,
                                    fileInput("measurementFile2", "Upload Measurement Files", 
                                            accept = c(".txt", ".csv"), multiple = TRUE)
                                ),
                                column(12,
                                    actionButton("outputMapping", "Output Mapping", 
                                                style = "background-color: blue; color: white; border-color: black;"),
                                    actionButton("editColNames", "Edit Column Names"),
                                    downloadButton("exportData", "Export Data"),
                                    actionButton("editMeasurementTransforms", "Edit Measurement Data"),
                                    actionButton("make_output", "Make Output Variable"),
                                    checkboxInput("keepMapping", "Keep mapping upon export", value = TRUE)
                                ),
                                # Now wrap the delete and reset checkboxes side by side in their own fluidRow:
                                fluidRow(
                                    column(6,
                                    h4("Delete Columns"),
                                    checkboxGroupInput("colsToDelete", "Select columns to delete:", choices = NULL),
                                    actionButton("deleteCols", "Delete Selected Columns")
                                    ),
                                    column(6,
                                    h4("Reset Columns From Edited Values"),
                                    checkboxGroupInput("colsToReset", "Select columns to reset:", choices = NULL),
                                    actionButton("resetCols", "Reset Selected Columns")
                                    )
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
    #epcIni <- settings$epcInput[2]
    dates <- as.Date(musoDate(settings$startYear, numYears=settings$numYears),"%d.%m.%Y") 
    rv <- reactiveValues(settings = setupMuso(), epc_files = character(0), epc_labels = character(0), epc_dates = data.frame(), epc_num_labels = character(0))

      parameters <- read.csv("parameters.csv", stringsAsFactors=FALSE)

    
    parameters <- parameters[!is.na(parameters$ABREVIATION) & parameters$ABREVIATION != "", ]
    # indexing the rows for the allocation parameters (if they exist)
    parametersFixed <- sprintf("%.2f", parameters$INDEX)
    parameters$group <- ifelse(grepl("^(132|133|134|135)\\.", parametersFixed),
                           # Remove the "132.", "133.", "134." or "135." prefix,
                           sub("^(132|133|134|135)\\.", "", parametersFixed),
                           NA)

    required_main <- c(132, 133, 134, 135)

    allocationNames <- c("132" = "Leaf",
                     "133" = "Fine Root",
                     "134" = "Fruit",
                     "135" = "Soft Stem")

    # Find the unique dependent groups already in the CSV
    dep_groups <- unique(parameters$group[!is.na(parameters$group)])



    # Loop over each dependent group and check for each required main index to see if any of the 4 dependent rows are missing
    for (g in dep_groups) {
        for (m in required_main) {
            # Construct the expected INDEX value 
            expected_index <- paste0(m, ".", g)
            expected_index_num <- as.numeric(expected_index)

            # Check if this expected_index is present in the parameters data frame
            if (!(expected_index_num %in% parameters$INDEX)) {
            # The row is missing, we will append a new row
            
            new_row <- data.frame(
                ABREVIATION = paste("Missing", allocationNames[as.character(m)]),
                INDEX = as.numeric(expected_index),
                min = 0,   
                max = 1,    
                group = g,
                stringsAsFactors = FALSE
            )
            parameters <- rbind(parameters, new_row)
           message("Added missing parameter row for ", expected_index, " (", allocationNames[as.character(m)], ") since it was not found in parameters.csv")
            }
        }
    }

     dailyOutputNames <- reactiveVal(settings$dailyOutputTable$name)

    # Reading and processing of measurement files
    measurementData <- reactiveVal(NULL)
    initialMeasurementData <- reactiveVal(NULL)

    observeEvent(input$measurementFile, {
        req(input$measurementFile)
        files <- input$measurementFile
        
        # Reading and combine files 
        new_data_list <- lapply(seq_len(nrow(files)), function(i) {
            df <- read.table(files$datapath[i], header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
            df$Date <- as.Date(paste(df[[1]], df[[2]], df[[3]], sep = "-"), format = "%Y-%m-%d")
            df[df == -9999] <- NA
            meas <- df[ , -(1:3), drop = FALSE]
            meas <- meas[, !(colnames(meas) %in% c("Date")), drop = FALSE]
            data.frame(Date = df$Date, meas, stringsAsFactors = FALSE, check.names = FALSE)
        })
        
            new_data_combined <- Reduce(function(x, y) dplyr::full_join(x, y, by = "Date"), new_data_list)
            
            # Create complete date sequence
            req(settings)
            min_year <- as.numeric(format(min(dates), "%Y"))
            max_year <- as.numeric(format(max(dates), "%Y"))
            sim_start <- as.Date(paste0(min_year, "-01-01"))
            sim_end <- as.Date(paste0(max_year, "-12-31"))
            all_dates <- seq.Date(sim_start, sim_end, by = "day")
            base_df <- data.frame(Date = all_dates)
        
        # Merging with base dates
        new_data_complete <- dplyr::full_join(base_df, new_data_combined, by = "Date")
        
        # Processing mappings IN THE COMPLETE DATA
        mapping_cols <- grep("_MAPPING$", names(new_data_complete), value = TRUE)
        mapping <- list()
        
        # handling mapping columns, if they exist we map them to the output variables and remove them from the data table
        if (length(mapping_cols) > 0) {
            for (map_col in mapping_cols) {
                output_var <- sub("_MAPPING$", "", map_col, fixed = FALSE)
                meas_cols <- unique(na.omit(new_data_complete[[map_col]])) 
                
                if (length(meas_cols) > 0) {
                    for (col in strsplit(meas_cols, ",")[[1]]) {
                        if (col %in% names(new_data_complete)) {
                            mapping[[col]] <- output_var
                        }
                    }
                }
            }
            new_data_complete <- new_data_complete[, !names(new_data_complete) %in% mapping_cols]
        }
        
        # Updating measurementData with complete, filtered data
        if (is.null(measurementData())) {
            measurementData(new_data_complete)
            initialMeasurementData(new_data_complete)
        } else {
            combined <- dplyr::full_join(measurementData(), new_data_complete, by = "Date")
            combined <- dplyr::filter(combined, Date >= sim_start & Date <= sim_end)
            measurementData(combined)

            init_data <- initialMeasurementData()
             new_cols <- setdiff(colnames(new_data_complete), colnames(init_data))

                 if (length(new_cols) > 0) {
                    # Create a data frame with Date and the new columns
                    new_initials <- new_data_complete[, c("Date", new_cols), drop = FALSE]
                    # Merge the new columns into the initial data
                    init_data <- dplyr::full_join(init_data, new_initials, by = "Date")
                    initialMeasurementData(init_data)
                }
        }
        
        # Setting mapping AFTER data processing
        if (length(mapping) > 0) mappingRV(mapping)
    })


        # DUPLICATING CODE FOR THE SECOND MEASUREMENT READ BUTTON I KNOW IT'S HORRIBLE BUT I actually don't see a trivial way to do this, I'll edit it later when I can get my head around it
         observeEvent(input$measurementFile2, {
        req(input$measurementFile2)
        files <- input$measurementFile2
        
        # Reading and combine files 
        new_data_list <- lapply(seq_len(nrow(files)), function(i) {
            df <- read.table(files$datapath[i], header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
            df$Date <- as.Date(paste(df[[1]], df[[2]], df[[3]], sep = "-"), format = "%Y-%m-%d")
            df[df == -9999] <- NA
            meas <- df[ , -(1:3), drop = FALSE]
            meas <- meas[, !(colnames(meas) %in% c("Date")), drop = FALSE]
            data.frame(Date = df$Date, meas, stringsAsFactors = FALSE, check.names = FALSE)
        })
        
            new_data_combined <- Reduce(function(x, y) dplyr::full_join(x, y, by = "Date"), new_data_list)
            
            # Create complete date sequence
            req(settings)
            min_year <- as.numeric(format(min(dates), "%Y"))
            max_year <- as.numeric(format(max(dates), "%Y"))
            sim_start <- as.Date(paste0(min_year, "-01-01"))
            sim_end <- as.Date(paste0(max_year, "-12-31"))
            all_dates <- seq.Date(sim_start, sim_end, by = "day")
            base_df <- data.frame(Date = all_dates)
        
        # Merging with base dates
        new_data_complete <- dplyr::full_join(base_df, new_data_combined, by = "Date")
        
        # Processing mappings IN THE COMPLETE DATA
        mapping_cols <- grep("_MAPPING$", names(new_data_complete), value = TRUE)
        mapping <- list()
        
        # handling mapping columns, if they exist we map them to the output variables and remove them from the data table
        if (length(mapping_cols) > 0) {
            for (map_col in mapping_cols) {
                output_var <- sub("_MAPPING$", "", map_col, fixed = FALSE)
                meas_cols <- unique(na.omit(new_data_complete[[map_col]])) 
                
                if (length(meas_cols) > 0) {
                    for (col in strsplit(meas_cols, ",")[[1]]) {
                        if (col %in% names(new_data_complete)) {
                            mapping[[col]] <- output_var
                        }
                    }
                }
            }
            new_data_complete <- new_data_complete[, !names(new_data_complete) %in% mapping_cols]
        }
        
        # Updating measurementData with complete, filtered data
        if (is.null(measurementData())) {
            measurementData(new_data_complete)
            initialMeasurementData(new_data_complete)
        } else {
            combined <- dplyr::full_join(measurementData(), new_data_complete, by = "Date")
            combined <- dplyr::filter(combined, Date >= sim_start & Date <= sim_end)
            measurementData(combined)
            
            init_data <- initialMeasurementData()
             new_cols <- setdiff(colnames(new_data_complete), colnames(init_data))

                 if (length(new_cols) > 0) {
                    # Create a data frame with Date and the new columns
                    new_initials <- new_data_complete[, c("Date", new_cols), drop = FALSE]
                    # Merge the new columns into the initial data
                    init_data <- dplyr::full_join(init_data, new_initials, by = "Date")
                    initialMeasurementData(init_data)
                }
        }
        
        # Setting mapping AFTER data processing
        if (length(mapping) > 0) mappingRV(mapping)
    })


            observe({
            req(measurementData())
            # Get all column names except Date
            cols <- setdiff(colnames(measurementData()), "Date")
            updateCheckboxGroupInput(session, "colsToDelete", choices = cols, selected = character(0))
            })

            
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

            # When the user clicks the delete button, remove the selected columns
            observeEvent(input$deleteCols, {
            req(measurementData())
            colsToRemove <- input$colsToDelete
            if(length(colsToRemove) > 0){
                # Remove the selected columns from the data frame
                df <- measurementData()
                df <- df[, !(colnames(df) %in% colsToRemove), drop = FALSE]
                measurementData(df)
                
                # Update the checkbox group input to reflect the new column names
                updateCheckboxGroupInput(session, "colsToDelete", choices = setdiff(colnames(df), "Date"), selected = character(0))
            }
            })

        # Mapping ui
        mappingRV <- reactiveVal(NULL)
            observeEvent(input$outputMapping, {
            req(measurementData())
            
            # Get measurement columns (all except "Date")
            measCols <- setdiff(colnames(measurementData()), "Date")
            
            # Get available output variables from your settings
            #availableOutputVars <- reactive({ settings$dailyOutputTable$name })
            
            # Create the modal's content:
            modalContent <- tagList(
                h3("Map Measurement Columns to Output Variables"),
                # Header row:
                fluidRow(
                column(6, strong("Measurement Column")),
                column(6, strong("Mapped Output Variable"))
                ),
                # For each measurement column, create a row with the column name and a dropdown
                lapply(measCols, function(col) {
                fluidRow(
                    column(6, div(style = "padding: 5px;", col)),
                    column(6, 
                    selectInput(
                        inputId = paste0("mapping_", col),
                        label = NULL,
                        choices = c("None", dailyOutputNames()),
                        # Use the saved mapping if it exists, otherwise default to "None"
                        selected = if (!is.null(mappingRV()) && !is.null(mappingRV()[[col]])) {
                        mappingRV()[[col]]
                        } else {
                        character(0)
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
            
          
            #print(mappingRV())
            
            # Close the modal
            removeModal()
            })

        # update column names button
        observeEvent(input$editColNames, {
            req(measurementData())
            cols <- colnames(measurementData())
            
            showModal(modalDialog(
                title = "Edit Column Names",
                # Only generate inputs for columns 2 through end (keeping Date as it is)
                tagList(
                lapply(2:length(cols), function(i) {
                    textInput(inputId = paste0("colName_", i), 
                            label = paste("Column", i, ":"), 
                            value = cols[i])
                })
                ),
                footer = tagList(
                modalButton("Cancel"),
                actionButton("saveColNames", "Save")
                ),
                easyClose = TRUE,
                size = "m"
            ))
        })

        # saving the new column names
        observeEvent(input$saveColNames, {
            req(measurementData())
            oldCols <- colnames(measurementData())
            
            newCols <- c(oldCols[1],
                        sapply(2:length(oldCols), function(i) {
                            input[[paste0("colName_", i)]]
                        }))
            
            df <- measurementData()
            colnames(df) <- newCols
            measurementData(df)
            removeModal()
        })





        # exporting our data frame
        output$exportData <- downloadHandler(
            filename = function() {
                paste("tuneMusoExport_measurementData-", Sys.Date(), ".csv", sep = "")
            },
            content = function(file) {
                # Make a copy for export
                export_df <- measurementData()
                
                # Create Year, Month, and Day columns from the Date column (so we have the same file format required for our measurement inputs)
                export_df$Year  <- format(export_df$Date, "%Y")
                export_df$Month <- format(export_df$Date, "%m")
                export_df$Day   <- format(export_df$Date, "%d")
                
                # Add mapping columns if mapping export button is pressed
                if (input$keepMapping) {
                    mapping <- mappingRV()
                    var_mapping <- list()
                    
                    # Create inverse mapping (output var -> measurement cols)
                    for (meas_col in names(mapping)) {
                        output_var <- mapping[[meas_col]]
                        if (output_var != "None") {
                        var_mapping[[output_var]] <- c(var_mapping[[output_var]], meas_col)
                        }
                    }
                    
                    for (output_var in names(var_mapping)) {
                        export_df[[paste0(output_var, "_MAPPING")]] <- 
                        paste(var_mapping[[output_var]], collapse = ",")
                    }
                }
               
                other_cols <- setdiff(colnames(export_df), c("Date", "Year", "Month", "Day"))
                export_df <- export_df[, c("Year", "Month", "Day", other_cols)]
                
                # Replace NA values with -9999 (for export only), currently if we leave NAs they will show up as empty cells in the CSV (bad)
                export_df[is.na(export_df)] <- -9999
                
              
                fwrite(export_df, file, row.names = FALSE, sep = " ")
            }
        )




    ########## EPC HANDLING ############
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




    #exit box not quite working as intended, we'll store its state directly (now it works)
        restoreFlag <- reactiveVal(FALSE)

        observeEvent(input$restoreOnExit, {
            restoreFlag(input$restoreOnExit)
        })

    
    session$onSessionEnded(function() {
        if (isolate(restoreFlag())) {  
            cat("Restoring all EPC files to original...\n")
            
            isolate({  # Ensuring all reactive values are accessed to avoid buggies
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

    # year range refresh delay
    debounced_yearRange <- reactive({
        input$yearRange
    }) %>% debounce(500)

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

        # saving the scrollbar position on year changing UI refresh
        observeEvent(input$yearRange, {
    
            session$sendCustomMessage("save_scroll", list(id = "plotPanel"))
            
            
                if (!is.null(input$plotPanel_scroll)) {
                session$sendCustomMessage("restore_scroll", list(id = "plotPanel", scroll = input$plotPanel_scroll))
                }
            
        })

       

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
                    actionButton(lock_btn_id, label = NULL, icon = icon("unlock"), class = "btn lock-btn unlocked",
                                style = "margin-top: -10px; margin-bottom: 10px;")
                )
                })
                
                    group_numeric <- as.numeric(g)
                    if (!is.na(group_numeric) && group_numeric >= 60) {
                        phenophase <- group_numeric - 59
                        groupLabel <- paste("Allocation group, phenophase", phenophase)
                    } else {
                        groupLabel <- paste("Allocation group", g)
                    }
    
                # condition whether hide plot area is active or not (so upon epc switching the sliders will retain their aligments)
                containerClass <- if (!is.null(input$plotHidden) && input$plotHidden) "dependentSliderContainer expanded" else "dependentSliderContainer"
                
                tagList(
                h4(groupLabel),
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

        # reactive value that will track the locked or unlock state of the allocation locking button
      # Define lockStates only once
        lockStates <- reactiveValues()

        # Initialize lockStates for each dependent parameter (if not already set)
        observe({
        req(input$selected_epc)
        dep_indices <- which(!is.na(parameters$group))
        for (i in dep_indices) {
            slider_id <- paste0("dep_", parameters$INDEX[i])
            if (is.null(lockStates[[slider_id]])) {
            lockStates[[slider_id]] <- FALSE
            }
        }
        })

    ####### lock button observer ########
        # Lock states outside reactivity
        #lockStates <- reactiveValues()
        #dep_indices <- which(!is.na(parameters$group))
        #lapply(dep_indices, function(i) {
        #slider_id <- paste0("dep_", parameters$INDEX[i])
        #lockStates[[slider_id]] <- FALSE
        #})

        # Single observer for all buttons
     

        dep_indices <- which(!is.na(parameters$group))
        lapply(dep_indices, function(i) {
            slider_id <- paste0("dep_", parameters$INDEX[i])
            lock_btn_id <- paste0("lock_", parameters$INDEX[i])
            
            observeEvent(input[[lock_btn_id]], {
                isolate({
                # Toggle the lock state
                lockStates[[slider_id]] <- !lockStates[[slider_id]]
                
                if (lockStates[[slider_id]]) {
                    shinyjs::addClass(id = lock_btn_id, class = "locked")
                    shinyjs::removeClass(id = lock_btn_id, class = "unlocked")
                } else {
                    shinyjs::addClass(id = lock_btn_id, class = "unlocked")
                    shinyjs::removeClass(id = lock_btn_id, class = "locked")
                }
                
                    # Update the button icon accordingly
                    new_icon <- if (lockStates[[slider_id]]) "lock" else "unlock"
                    updateActionButton(session, lock_btn_id, icon = icon(new_icon))
                    })
            }, ignoreInit = TRUE)
        })


        ##### sum to 1 counter for allocation ######
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
            # When autoCalc is toggled on, INSTANTLY perform a recalculation for group g 
            if (isTRUE(input[[paste0("autoCalc_", g)]])) {
                group_rows <- which(!is.na(parameters$group) &
                                    parameters$group == g &
                                    as.numeric(sub("\\..*", "", parameters$INDEX)) %in% c(132,133,134,135))
                group_rows <- group_rows[order(as.numeric(sub("\\..*", "", parameters$INDEX[group_rows])))]
                ids <- paste0("dep_", parameters$INDEX[group_rows])
                
                # Calculate total locked and available for unlocked
                locked_vals <- unlist(lapply(ids, function(x) {
                if (isTRUE(lockStates[[x]])) {
                    if (is.null(input[[x]])) 0 else as.numeric(input[[x]])
                } else 0
                }))
                L <- sum(locked_vals)
                available_total <- 1 - L
                
                # For unlocked sliders, recalculate their values proportionally
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

        # sum counter viusalization
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
                paste0("Total sum: ", round(total, 2), " (Warning: Sum > 1!)")
            } else {
                paste0("Total sum: ", round(total, 2))
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
                # Retrieving the stored values for the old EPC, if not available, use defaultValues
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
last_year <- reactiveVal(NULL)

debounced_yearRange2 <- reactive({ input$yearRange }) %>% debounce(500)

observeEvent(
  list(debounced_yearRange2(), input$auto_epc_selection, input$singleYear), 
  {
    req(!is.null(input$auto_epc_selection),
        !is.null(input$singleYear),
        rv$epc_dates)
    
    if (isTRUE(input$auto_epc_selection) && isTRUE(input$singleYear)) {
      req(debounced_yearRange2())
      
      # If the slider returns more than one value, take the first
      selected_year_val <- debounced_yearRange2()
      if (length(selected_year_val) > 1) {
        selected_year_val <- selected_year_val[1]
      }
      selected_year <- as.character(selected_year_val)
      
      # Ensure the DATE column is in Date format.
      if (!inherits(rv$epc_dates$DATE, "Date")) {
        rv$epc_dates$DATE <- as.Date(rv$epc_dates$DATE, format = "%Y.%m.%d")
      }
      
      # Filter EPC files for the selected year.
      filtered <- rv$epc_dates[
        format(rv$epc_dates$DATE, "%Y") == selected_year &
          rv$epc_dates[["CROP.file."]] %in% rv$epc_files, 
      ]
      
      if (nrow(filtered) > 0) {
        # Get the available EPC file names for this year.
        choices <- unique(filtered[["CROP.file."]])
        # Get corresponding labels (assuming rv$epc_files and rv$epc_labels align).
        choices_labels <- rv$epc_labels[match(choices, rv$epc_files)]
        # Build a named vector for the selectInput.
        choices_named <- setNames(choices, choices_labels)
        
        # Update the selectInput with the filtered choices.
        updateSelectInput(session, "selected_epc", choices = choices_named)
        
        # Only auto-select the earliest EPC if:
        # - The year has just changed, OR
        # - The current (isolated) selection is not among the choices.
        if (is.null(last_year()) ||
            last_year() != selected_year ||
            !isolate(input$selected_epc) %in% choices) {
          earliest_row <- filtered[which.min(filtered$DATE), ]
          earliest_epc <- earliest_row[["CROP.file."]]
          updateSelectInput(session, "selected_epc", selected = earliest_epc)
        }
        
        # Store the current year.
        last_year(selected_year)
      }
      
    } else {
      # When auto-selection is disabled, update to show the full list with labels.
      full_choices <- setNames(rv$epc_files, rv$epc_labels)
      updateSelectInput(session, "selected_epc", choices = full_choices)
    }
  }
)




        # measurement manipuplation modal
        observeEvent(input$editMeasurementTransforms, {
  req(measurementData())
  
  showModal(modalDialog(
    title = "Measurement Data Transformations",
    size = "l",
    easyClose = TRUE,
    footer = modalButton("Close"),
    tabsetPanel(
      # Tab for replacing negatives with NA
 tabPanel("Set Values to NA",
  fluidRow(
    column(4,
      selectInput("col_to_na", "Select column:", 
                  choices = setdiff(colnames(measurementData()), "Date"))
    ),
    column(4,
      numericInput("na_lower", "Lower bound:", value = NA)
    ),
    column(4,
      numericInput("na_upper", "Upper bound:", value = NA)
    )
  ),
  fluidRow(
    column(4,
      checkboxInput("na_newcol", "Add as new column", value = FALSE)
    ),
    column(8,
      actionButton("apply_na", "Apply Transformation")
    )
  )
),
      
      # Tab for arithmetic operations
 tabPanel("Arithmetic Operation",
  fluidRow(
    column(4,
      selectInput("col_arith", "Select column:", 
                  choices = setdiff(colnames(measurementData()), "Date"))
    ),
    column(4,
      selectInput("arith_op", "Operation", 
                  choices = c("Add", "Subtract", "Multiply", "Divide"))
    ),
    column(4,
      numericInput("arith_val", "Value:", value = 0)
    )
  ),
  fluidRow(
    column(4,
      checkboxInput("arith_newcol", "Add as new column", value = FALSE)
    ),
    column(8,
      actionButton("apply_arith", "Apply Transformation")
    )
  )
),

      
      # Tab for column interaction
tabPanel("Column Interaction",
  fluidRow(
    column(4,
      selectInput("col1", "Column 1:", 
                  choices = setdiff(colnames(measurementData()), "Date"))
    ),
    column(4,
      selectInput("col2", "Column 2:", 
                  choices = setdiff(colnames(measurementData()), "Date"))
    ),
    column(4,
      selectInput("interaction_op", "Operation", 
                  choices = c("Multiply", "Add", "Subtract", "Divide"))
    )
  ),
  fluidRow(
    column(4,
      checkboxInput("interaction_newcol", "Add as new column", value = FALSE)
    ),
    column(8,
      actionButton("apply_interaction", "Apply Transformation")
    )
  )
)

    )
  ))
})


observeEvent(input$apply_na, {
  req(measurementData(), input$col_to_na)
  df <- measurementData()
  col <- input$col_to_na
  
  lower_bound <- input$na_lower
  upper_bound <- input$na_upper
  
  # Start with the current values.
  new_values <- df[[col]]
  
  if (!is.na(lower_bound) && !is.na(upper_bound)) {
    new_values[new_values >= lower_bound & new_values <= upper_bound] <- NA
  } else if (!is.na(lower_bound)) {
    new_values[new_values >= lower_bound] <- NA
  } else if (!is.na(upper_bound)) {
    new_values[new_values <= upper_bound] <- NA
  } else {
    showNotification("Please specify at least one bound.", type = "error")
    return()
  }
  
  if (isTRUE(input$na_newcol)) {
    new_col_name <- paste(col, "NA", sep = "_")
    df[[new_col_name]] <- new_values
  } else {
    df[[col]] <- new_values
  }
  
  measurementData(df)
  showNotification(paste("Updated", col, "with NA transformation"))
})

observeEvent(input$apply_arith, {
  req(measurementData(), input$col_arith, input$arith_op, input$arith_val)
  df <- measurementData()
  col <- input$col_arith
  op <- input$arith_op
  val <- input$arith_val
  
  # Calculate new values based on the chosen operation.
  new_values <- switch(op,
    "Add" = df[[col]] + val,
    "Subtract" = df[[col]] - val,
    "Multiply" = df[[col]] * val,
    "Divide" = {
      if(val == 0) {
        showNotification("Division by zero not allowed", type = "error")
        return()
      } else {
        df[[col]] / val
      }
    }
  )
  
  if (isTRUE(input$arith_newcol)) {
    # Create a new column name, for example: OriginalColumn_Add_5
    new_col_name <- paste(col, op, val, sep = "_")
    df[[new_col_name]] <- new_values
  } else {
    # Update the selected column in place.
    df[[col]] <- new_values
  }
  
  measurementData(df)
  showNotification(paste("Applied", op, "operation to", col))
})

observeEvent(input$apply_interaction, {
  req(measurementData(), input$col1, input$col2, input$interaction_op)
  df <- measurementData()
  col1 <- input$col1
  col2 <- input$col2
  op <- input$interaction_op
  
  new_values <- switch(op,
    "Multiply" = df[[col1]] * df[[col2]],
    "Add"      = df[[col1]] + df[[col2]],
    "Subtract" = df[[col1]] - df[[col2]],
    "Divide"   = {
      div_res <- df[[col1]] / ifelse(df[[col2]] == 0, NA, df[[col2]])
      if(any(df[[col2]] == 0, na.rm = TRUE)) {
        showNotification("Division by zero encountered; resulting values set to NA", type = "warning")
      }
      div_res
    }
  )
  
  if (isTRUE(input$interaction_newcol)) {
    new_col_name <- paste(col1, op, col2, sep = "_")
    df[[new_col_name]] <- new_values
  } else {
    # Update the first chosen column (col1) in place.
    df[[col1]] <- new_values
  }
  
  measurementData(df)
  showNotification(paste("Applied", op, "operation between", col1, "and", col2))
})

    # reset manipulated column
    observe({
    req(measurementData())
    cols <- setdiff(colnames(measurementData()), "Date")
    updateCheckboxGroupInput(session, "colsToReset", choices = cols, selected = character(0))
    })

    observeEvent(input$resetCols, {
        req(measurementData(), initialMeasurementData())
        colsToReset <- input$colsToReset
        if (length(colsToReset) > 0) {
            df_current <- measurementData()
            df_initial <- initialMeasurementData()
            
            # For each selected column, revert its values to the initial values
            for (col in colsToReset) {
            if (col %in% names(df_current) && col %in% names(df_initial)) {
                df_current[[col]] <- df_initial[[col]]
            }
            }
            
            measurementData(df_current)
            
            updateCheckboxGroupInput(session, "colsToReset", choices = setdiff(colnames(df_current), "Date"), selected = character(0))
            showNotification("Selected column(s) have been reset to their initial values", type = "message")
        }
    })


    # Toggle visibility of the sidebar panel
    observeEvent(input$toggleUI, {
    toggle("controlPanel", anim = FALSE)
    shinyjs::runjs("
        // If the control panel is hidden, force the plot panel to expand
        if ($('#controlPanel').is(':visible')) {
            $('#plotPanel').css('flex', '2');
            $('#hoverSliderContainer').hide(); // HIDE the hover slider when UI is visible
        } else {
            // Set flex property so that plot panel fills the entire row
            $('#plotPanel').css('flex', '1 1 100%');
            $('#hoverSliderContainer').show(); // SHOW the hover slider when UI is hidden
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

    # if new variable creation is chosen, we'll store here
    newVars <- reactiveValues(defs = list())

          layers <- list(
            c(0, 3),    # Layer 1: VWC[0]
            c(3, 10),   # Layer 2: VWC[1]
            c(10, 30),  # Layer 3: VWC[2]
            c(30, 60),  # Layer 4: VWC[3]
            c(60, 90),  # Layer 5: VWC[4]
            c(90, 120), # Layer 6: VWC[5]
            c(120, 150),# Layer 7: VWC[6]
            c(150, 200),# Layer 8: VWC[7]
            c(200, 400),# Layer 9: VWC[8]
            c(400, 1000)# Layer 10: VWC[9]
        ) 
        
    calc_weighted_swc <- function(swc_values, min_depth, max_depth, layers) {
        # Use only as many layers as are available in swc_values:
        n <- length(swc_values)
        total_weight <- 0
        weighted_sum <- 0
        for (i in seq_len(n)) {
            layer_min <- layers[[i]][1]
            layer_max <- layers[[i]][2]
            overlap <- max(0, min(max_depth, layer_max) - max(min_depth, layer_min))
            if (overlap > 0) {
                weighted_sum <- weighted_sum + swc_values[i] * overlap
                total_weight <- total_weight + overlap
            }
        }
        if (total_weight > 0) return(weighted_sum / total_weight) else return(NA)
    }

        #### MODEL RUN ####
    observeEvent(list(input$runModel, input$runMusoExtra), {
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
        
        # this is probably not needed but I'm smoothbraining it. Explanation why not needed: 
        # 1) Since this is crop rotation it doesn't matter what's the epc file in the ini
        # 2) At non crop rotation situations this is a redefinition since their is only 1 epc file
        # 3) calibMuso doesn't even use this information
        # okay I'm convinced I'll comment it out and delete it later when I'm not smoothbraining, I gotta watch Shrek 2
        # settings$epcInput[["normal"]] <- epcIni
        
        
        result <- calibMuso(settings = settings, calibrationPar = parameters[,2], parameters = paramVal, silent = TRUE)
        if (length(result) == 0) {
            showNotification("Model did not return results!", type = "error")
        } else {

        print("Model ran successfully")

        dfs_orig <- as.data.frame(result)  # 'result' is the simulation output matrix
        # Detect the VWC columns from the original output:
        #vwc_cols <- grep("^VWC\\[", names(dfs_orig), value = TRUE)
        #print("Detected VWC columns:")
        #print(vwc_cols)

        if (length(newVars$defs) > 0) {
            #dfs_orig <- as.data.frame(result)
            #vwc_cols <- grep("^VWC\\[", names(dfs_orig), value = TRUE)
            #vwc_indices <- as.numeric(gsub("VWC\\[|\\]", "", vwc_cols))
            #current_layers <- layers[vwc_indices + 1]  # Adjust indexing
                for (var_name in names(newVars$defs)) {
                    def <- newVars$defs[[var_name]]
                    pattern <- paste0("^", def$base_variable, "\\[")
                    base_cols <- grep(pattern, names(dfs_orig), value = TRUE)
                     
                    if (length(base_cols) == 0) {
                        showNotification(paste("No columns found for base variable", def$base_variable), type = "error")
                        next
                    }
                    pattern_ind <- paste0(def$base_variable, "\\[|\\]")
                    base_indices <- as.numeric(gsub(pattern_ind, "", base_cols))
                    current_layers <- layers[base_indices + 1]  # Adjust for R's 1-based indexing
                        
     
                   
                    
                    
                    
                    
                    new_val <- apply(dfs_orig[, base_cols, drop = FALSE], 1, function(r) {
                    swc_vals <- as.numeric(r)
                    calc_weighted_swc(swc_vals, def$min_depth, def$max_depth, current_layers)
                    })
                    dfs_orig[[var_name]] <- new_val
                }
            result <- as.matrix(dfs_orig)
        }
        outputList$nextVal <- result

       

    }
    })

        # restoring scrollbar position
        observe({
            if (!is.null(input$plotPanel_scroll)) {
            session$sendCustomMessage("restore_scroll", list(id = "plotPanel", scroll = input$plotPanel_scroll))
            }
        })



        
    ######## METRICS CALCULATION #########

     simData <- reactive({
            req(outputList$nextVal)  
            result <- outputList$nextVal
            dfs <- as.data.frame(result)
            dfs$Date <- as.Date(rownames(result), format = "%d.%m.%Y")
            dfs
    })

     metricsData <- reactive({
        req(simData(), input$yearRange)  
        
        # Get measurement data (if any) and mapping
        meas_df <- measurementData()
        mapping <- mappingRV()
        
        # If no measurement data or no mapping is provided, return an empty data frame (so plots are still generated)
        if (is.null(meas_df) || nrow(meas_df) == 0 || is.null(mapping) || length(mapping) == 0) {
            return(data.frame(
            Measurement = character(),
            OutputVariable = character(),
            RMSE = numeric(),
            BIAS = numeric(),
            Correlation = numeric(),
            stringsAsFactors = FALSE
            ))
        }
        
        # Determine selected years
        selectedYears <- if (input$singleYear) {
            input$yearRange
        } else {
            seq(input$yearRange[1], input$yearRange[2])
        }
        
        # Filter measurement and simulation data to the selected years
        meas_df <- meas_df[format(meas_df$Date, "%Y") %in% selectedYears, ]
        
        sim_df <- simData()
        sim_df <- sim_df[format(sim_df$Date, "%Y") %in% selectedYears, ]
        
        # for the good rmse calc
        cols_to_modify <- c("GPP", "TR", "NEE")  
        existing_cols <- intersect(cols_to_modify, names(sim_df))  # Check which exist

        sim_df[existing_cols] <- sim_df[existing_cols] * 1000

        # Merge the two datasets on Date (common columns get suffixes to avoid stinky bugs)
        merged_df <- merge(meas_df, sim_df, by = "Date", suffixes = c("_meas", "_sim"))
        
        # For each mapped measurement, calculate RMSE and correlation
        metrics_list <- lapply(names(mapping), function(meas_col) {
            output_var <- mapping[[meas_col]]
            if (output_var == "None") return(NULL)
            
            # Find the correct columns in merged_df
            x_col <- if (meas_col %in% colnames(merged_df)) {
            meas_col
            } else if (paste0(meas_col, "_meas") %in% colnames(merged_df)) {
            paste0(meas_col, "_meas")
            } else {
            NULL
            }
            
            y_col <- if (output_var %in% colnames(merged_df)) {
            output_var
            } else if (paste0(output_var, "_sim") %in% colnames(merged_df)) {
            paste0(output_var, "_sim")
            } else {
            NULL
            }
            
            # Skip if we can’t find the necessary columns
            if (is.null(x_col) || is.null(y_col)) return(NULL)
            
            x <- merged_df[[x_col]]
            y <- merged_df[[y_col]]
            
            # Remove pairs where either value is NA
            valid <- complete.cases(x, y)
            if (sum(valid) == 0) {
            rmse_val <- NA
            bias_val <- NA
            corr_val <- NA
            } else {
            rmse_val <- sqrt(mean((x[valid] - y[valid])^2))
            bias_val <- mean(x[valid] - y[valid])
            corr_val <- if (length(x[valid]) > 1) cor(x[valid], y[valid]) else NA
            }
            
            data.frame(
            Measurement = meas_col,
            OutputVariable = output_var,
            RMSE = rmse_val,
            BIAS = bias_val,
            Correlation = corr_val,
            stringsAsFactors = FALSE
            )
        })
        
        metrics <- do.call(rbind, metrics_list)
        if (is.null(metrics)) {
            metrics <- data.frame(
            Measurement = character(),
            OutputVariable = character(),
            RMSE = numeric(),
            BIAS = numeric(),
            Correlation = numeric(),
            stringsAsFactors = FALSE
            )
        }
        metrics
        })



        ########## SOIL WATER CONTENT CALCULATION ############
        observeEvent(input$make_output, {
            showModal(modalDialog(
            title = "Create New Output Variable",
            numericInput("min_depth", "Min Depth (cm)", value = 0, min = 0),
            numericInput("max_depth", "Max Depth (cm)", value = 50, min = 0),
            textInput("variable_name", "Variable Name", value = "SWC_0_50"),
            selectInput("base_variable","Base Variable",
                choices = c("VWC","tsoil"),
                selected = "VWC"),
            footer = tagList(
                modalButton("Cancel"),
                actionButton("create_variable", "Create variable")
            )
            ))
        })
  
        # to change the textinput if we switch to tsoil
        observeEvent(input$base_variable, {
            if(input$base_variable == "VWC" && input$variable_name %in% c("Tsoil_0_50", "tsoil_0_50")) {
                updateTextInput(session, "variable_name", value = "SWC_0_50")
            } else if(input$base_variable == "tsoil" && input$variable_name %in% c("SWC_0_50", "swc_0_50")) {
                updateTextInput(session, "variable_name", value = "Tsoil_0_50")
            }
        })




       
observeEvent(input$create_variable, {
  req(input$min_depth, input$max_depth, input$variable_name, input$base_variable)
  #  cat("BEFORE updating picker:\n")
  #cat(" input$selected_vars is:", input$selected_vars, "\n")
  #cat(" newVars$defs keys:", names(newVars$defs), "\n")
  # Check for existing variable name
  if (input$variable_name %in% names(newVars$defs)) {
    showNotification("Variable name already exists. Choose a unique name.", type = "error")
    return()
  }
  
  # Add new variable definition
  newVars$defs[[input$variable_name]] <- list(
    min_depth = input$min_depth,
    max_depth = input$max_depth,
    base_variable = input$base_variable,
    variable_name = input$variable_name
  )
  
  # If output exists, recompute all variables
  if (!is.null(outputList$nextVal)) {
    dfs <- as.data.frame(outputList$nextVal, row.names = rownames(outputList$nextVal))
    
    if(nrow(dfs) > 0){
    # Extract current VWC columns and their indices
  
    # Compute all variables in newVars$defs
    for (var_name in names(newVars$defs)) {
      def <- newVars$defs[[var_name]]

    pattern <- paste0("^", def$base_variable, "\\[")
    base_cols <- grep(pattern, names(dfs), value = TRUE)
    if (length(base_cols) == 0) {
        showNotification(paste("No columns found for base variable", def$base_variable), type = "error")
        next
      }
    pattern_ind <- paste0(def$base_variable, "\\[|\\]")
    base_indices <- as.numeric(gsub(pattern_ind, "", base_cols))
    current_layers <- layers[base_indices + 1]  # Adjust for R's 1-based indexing
    
     

      dfs_num <- dfs[, base_cols, drop = FALSE]

    new_val <- apply(dfs[, base_cols, drop = FALSE], 1, function(r) {
        swc_vals <- as.numeric(r)
        calc_weighted_swc(swc_vals, def$min_depth, def$max_depth, current_layers)
      })
      dfs[[var_name]] <- new_val
    }
    
    outputList$nextVal <- as.matrix(dfs)
  }
  else {
     showNotification("Simulation output is empty. The new variable will be computed on the next model run.", type = "warning")
  }
  } else {
    showNotification("Model hasn't been run yet. The new variable will be computed on the next model run.", type = "warning")
  }
  
  # Update picker input
  new_row <- data.frame(
    index = max(rv$settings$dailyOutputTable$index) + 1,
    code = NA,
    name = input$variable_name
  )

 #   cat("DEBUG: ABOUT TO append new_row = ", new_row$name, "\n")
#cat("DEBUG: dailyOutputTable BEFORE appending:\n")
#print(settings$dailyOutputTable)



  rv$settings$dailyOutputTable <- rbind(rv$settings$dailyOutputTable, new_row)
    #print(settings$dailyOutputTable)

  dailyOutputNames(rv$settings$dailyOutputTable$name)
 updatePickerInput(session, "selected_vars",
  choices  = rv$settings$dailyOutputTable$name,
  selected = input$selected_vars
  #selected = unique(c(input$selected_vars, input$variable_name))
)
 #cat("AFTER updating picker - code ran\n")
  # cat(" input$selected_vars is:", input$selected_vars, "\n")
  #cat(" newVars$defs keys:", names(newVars$defs), "\n")
  #print(settings$dailyOutputTable)
   showNotification(paste("New variable", input$variable_name, "has been added to the current output."))

  removeModal()
})


#observe({
#  invalidateLater(3000)  # check every 1 second
#  cat("DEBUG CHECK — after 1s:\n")
#  cat("   dailyOutputTable$name:", rv$settings$dailyOutputTable$name, "\n")
#  cat("   input$selected_vars:  ", input$selected_vars, "\n")
#})
    
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

        # Settings (so far only for resolution)
        exportSettings <- reactiveValues(width = 1200, height = 900, scale = 5)

          observeEvent(input$settings_btn, {
            showModal(modalDialog(
            title = "Settings",
            # Inputs for resolution settings
            numericInput("export_width", "PNG Export Width (px):", value = exportSettings$width),
            numericInput("export_height", "PNG Export Height (px):", value = exportSettings$height),
            numericInput("export_scale", "PNG Export Scale:", value = exportSettings$scale, min = 1),
            tags$button(
                id = "fullscreen_btn",
                class = "btn btn-default",
                tags$i(class = "fa fa-expand"),  
                title = "Go Fullscreen [F11] (only works in browser)"  
            ),
            
            easyClose = TRUE,
            footer = tagList(
                modalButton("Cancel"),
                actionButton("apply_settings", "Apply")
            )
            ))
        })
        
        # When the user clicks "Apply", update the reactive values and close the modal
        observeEvent(input$apply_settings, {
            exportSettings$width <- input$export_width
            exportSettings$height <- input$export_height
            exportSettings$scale <- input$export_scale
            removeModal()
        })



            ################ PLOTTING ###############
                

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

                    metrics_df <- metricsData()
                    

                    if (!is.null(mapping)) {
                        # Find measurement columns mapped to the current var
                        mappedCols <- names(mapping)[mapping == var]
                       
                        if (length(mappedCols) > 0) {
                        n_meas <- length(mappedCols)
                        meas_colors <- colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Greens")[4:9]))(n_meas)

                            # Add each mapped measurement column
                            for (i in seq_along(mappedCols)) {
                                col <- mappedCols[i]
                                yData <- df_filtered[[col]]
                                
                                # Convert negatives to NA for GPP and TR (might be obsolete if data frame manipulation is added)
                                if (var %in% c("GPP", "TR")) {
                                    yData[yData < 0] <- NA
                                }
                                
                              m_row <- metrics_df[metrics_df$Measurement == col, ]
                                rmse_str <- if (nrow(m_row) > 0 && !is.na(m_row$RMSE)) {
                                    sprintf("RMSE: %.2f", m_row$RMSE)
                                } else {
                                    "RMSE: NA"
                                }
                                bias_str <- if (nrow(m_row) > 0 && !is.na(m_row$BIAS)) {
                                    sprintf("Bias: %.2f", m_row$BIAS)
                                } else {
                                    "Bias: NA"
                                }
                               corr_str <- if (nrow(m_row) > 0 && !is.na(m_row$Correlation)) {
                                    sprintf("R<sup>2</sup>: %.2f", m_row$Correlation)  # R² formatted
                                } else {
                                    "R<sup>2</sup>: NA"
                                }

                                metric_label <- paste(rmse_str, bias_str, corr_str, sep = " | ")

                                p <- add_trace(p,
                                            x = df_filtered$Date,
                                            y = yData,
                                            type = 'scatter',
                                            mode = 'markers',
                                            #name = paste0(col, " Measurement<br>", metric_label),
                                            name = paste0(col, " Measurement\n", metric_label),
                                            marker = list(symbol = "circle", size = 7, color =meas_colors[i]))
                                            
                            }
                        }
                    }
                    # for alignment issues when measurements are applied (the legend would still screw the alignment but it can be toggled off!)
                    common_x_range <- range(filteredDates, na.rm = TRUE)

                #p <- p %>% plotly::layout(
                        #title = list(text = paste("Plot of", var),
                        #    font = list(size = 16, color = "black")),
                        #xaxis = list(title = "Date"),
                   #     yaxis = list(title = var)
                   #     )

                    p <- p %>% plotly::layout(
                        xaxis = list(range = common_x_range),
                        yaxis = list(title = var),
                        showlegend = legendVisible()  # Conditionally show/hide legend
                    )

                    p <- p %>% plotly::config(toImageButtonOptions = list(
                    format = "png", 
                    width = exportSettings$width, 
                    height = exportSettings$height, 
                    scale = exportSettings$scale))
                    
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
tuneMuso <- function(directory = NULL, ...){ 
    shinyOptions(workdir = getwd())
    if(is.null(directory)){
        shinyOptions(musoRoot = ".")
    } else {
        shinyOptions(musoRoot = normalizePath(directory))
    }
    #shinyApp(ui = tuneMusoUI(), server = tuneMusoServer, options = c(list(launch.browser = TRUE), list(...)))
    shinyApp(ui = tuneMusoUI(), server = tuneMusoServer, options = list(...))
}

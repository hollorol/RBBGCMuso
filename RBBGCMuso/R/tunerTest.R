#' tuneMusoUI 
#' 
#' This is a simple parameter tuner function which works great in a flat directory system
#'
#' @param parameterFile optional, the parameter csv file
#' @importFrom shinyjs useShinyjs toggle show hide disable enable removeEvent runjs 
#' @importFrom dplyr filter %>% select full_join left_join mutate across arrange first lag
#' @importFrom shinyjqui jqui_resizable 
#' @importFrom lubridate year month day 
#' @importFrom data.table fread fwrite
#' @importFrom jsonlite fromJSON
#' @importFrom httr POST
#' @importFrom colourpicker colourInput updateColourInput
#' @importFrom waiter use_waiter use_hostess waiter_hide Hostess Waiter waiter_show_on_load hostess_loader spin_3 waiterShowOnLoad
#' @importFrom future plan future multisession value
#' @importFrom DT dataTableOutput datatable renderDataTable
#' @importFrom shinyWidgets pickerInput updatePickerInput confirmSweetAlert
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot geom_line geom_point scale_x_date theme element_text element_blank labs ggtitle scale_y_continuous coord_cartesian annotate
#' @importFrom shiny tags actionButton numericInput HTML checkboxInput titlePanel radioButtons textAreaInput fluidPage sidebarLayout sidebarPanel mainPanel getShinyOption tabsetPanel tabPanel tagList selectInput sliderInput renderUI div fileInput uiOutput updateSliderInput observe observeEvent validate need showNotification icon textInput isRunning reactiveVal reactiveValues isolate debounce bindEvent fluidRow column checkboxGroupInput showModal modalDialog modalButton removeModal h4 downloadButton downloadHandler verbatimTextOutput onFlushed stopApp renderPlot
#' @usage ...
#' @export 
tuneMusoUI2 <- function(parameterFile = NULL, ...) {
    setwd(getShinyOption("musoRoot"))
    workdir <- getwd()
    dir.create("bck", showWarnings = FALSE)
    file.copy("n.ini", "bck/n.ini", overwrite = FALSE)
    
    if (is.null(parameterFile)) {
        parameterFile <- "parameters.csv"
    }
    
    parameters <- read.csv(parameterFile, stringsAsFactors = FALSE)
    settings <- setupMuso(...)
    
    # keeping the scrollbar at the same position after refresh upon model run (or other processes)
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
            let standardSliders = $('#standardSliders');
            let dependentContainers = $('.dependentSliderContainer');
            let parametersLayout = $('#parametersLayout');
            let runLoggerLayout = $('#runLoggerLayout');
            let hotkeyContainer = $('#hotkeyContainer');
            
            plotPanel.toggle();
            if (plotPanel.is(':visible')) {
                button.text('Hide Plot Area');
                button.find('i').removeClass('eye').addClass('eye-slash');
                controlPanel.css({'flex': '0 1 auto', 'max-width': '450px'});
                standardSliders.removeClass('expanded');
                dependentContainers.removeClass('expanded');
                parametersLayout.removeClass('expanded');
                runLoggerLayout.removeClass('expanded');
                $('#hotkeyOriginal').append(hotkeyContainer); // Move back to original spot
                hotkeyContainer.find('#runMusoExtra').show();
                Shiny.setInputValue('plotHidden', false);
            } else {
                button.text('Show Plot Area');
                button.find('i').removeClass('eye-slash').addClass('eye');
                controlPanel.css({'flex': '1 1 100%', 'max-width': 'none'});
                standardSliders.addClass('expanded');
                dependentContainers.addClass('expanded');
                parametersLayout.addClass('expanded');
                runLoggerLayout.addClass('expanded');
                $('#parametersLayout .colRight').append(hotkeyContainer); // Scope to Parameters' colRight
                hotkeyContainer.find('#runMusoExtra').hide();
                Shiny.setInputValue('plotHidden', true);
            }
            $(window).trigger('resize');
        });
    });
        "

    # expanded window, yes
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

    Notifications <- "
        Shiny.addCustomMessageHandler('checkNotification', function(notification_message) {
            var lastRead = localStorage.getItem('last_notification');
            if (lastRead !== notification_message) {
            // If different, mark as new: add class and badge.
            $('#show_notification').addClass('new-notification');
            if ($('#badge').length === 0) {
                $('#show_notification').append('<span id=\"badge\">1</span>');
            }
            } else {
            // If already read, ensure no badge is visible.
            $('#show_notification').removeClass('new-notification');
            $('#badge').remove();
            }
        });
    "

    disableSpellCheck <- "
        document.addEventListener('DOMContentLoaded', function() {
        document.querySelectorAll('textarea, input').forEach(function(el) {
            el.setAttribute('spellcheck', 'false');
        });
        });
    "

    ToggleEpcSoil <- "
      Shiny.addCustomMessageHandler('setSwitchState', function(message) {
      if(message.switched){
        $('#switch_mode').addClass('switched');
      } else {
        $('#switch_mode').removeClass('switched');
      }
    });
    "

    PersistentNotif <- "
        $(document).on('mouseenter', '.shiny-notification', function() {
            // Cancel any pending fade-out timeout
            clearTimeout($(this).data('timeout'));
            $(this).stop(true, true).css('opacity', '1'); 
        }).on('mouseleave', '.shiny-notification', function() {
            var $this = $(this);
            // Wait 4 seconds after leaving before starting fade-out over 4 seconds
            var timeout = setTimeout(function() {
            $this.fadeOut(4000);
            }, 4000);
            $this.data('timeout', timeout);
        });
    "

    waiting <- "
        $(document).on('shiny:busy', function() {
        $('#loader-message').text('Processing calculations...');
        });
        $(document).on('shiny:idle', function() {
        $('#loader-message').text('Finalizing...');
        });
    "

    Titlemanagement <- '
function wrapText(elementId, openTag, closeTag) {
        var input = document.getElementById(elementId);
        var start = input.selectionStart;
        var end = input.selectionEnd;
        var text = input.value;
        var selectedText = text.substring(start, end);
        
        if (start === end) {
          input.value = text.substring(0, start) + openTag + closeTag + text.substring(end);
          input.selectionStart = input.selectionEnd = start + openTag.length;
        } else {
          input.value = text.substring(0, start) + openTag + selectedText + closeTag + text.substring(end);
          input.selectionStart = start;
          input.selectionEnd = end + openTag.length + closeTag.length + selectedText.length;
        }
        // Trigger Shiny input update
        Shiny.setInputValue(elementId, input.value);
      }

      // Use event delegation to handle dynamically created buttons
      $(document).on("click", "#bold_btn", function() { wrapText("y_title", "<b>", "</b>"); });
      $(document).on("click", "#italic_btn", function() { wrapText("y_title", "<i>", "</i>"); });
      $(document).on("click", "#underline_btn", function() { wrapText("y_title", "<u>", "</u>"); });
      $(document).on("click", "#sup_btn", function() { wrapText("y_title", "<sup>", "</sup>"); });
      $(document).on("click", "#sub_btn", function() { wrapText("y_title", "<sub>", "</sub>"); });
    '
  


 fluidPage(
  useShinyjs(),
  use_waiter(),
  use_hostess(),
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
          height: calc(100vh - 40px);
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

    .dependentSliderContainer {
    display: flex;
    flex-wrap: wrap;
    /* Optionally remove whitespace issues by setting font-size: 0 if needed */
    font-size: 0;
    }
      /* Dependent sliders layout */
      .dependentSliderContainer .slider-col {
           font-size: 14px; /* Reset font-size for children */
    width: 48%;
    margin-right: 4%;
    box-sizing: border-box;
      }
      .dependentSliderContainer.expanded .slider-col {
          width: 23%;
          margin-right: 1%;
      }
      .dependentSliderContainer .slider-col:nth-child(2n) {
          margin-right: 0%;
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

        .new-notification {
        position: relative;
        animation: shake 0.5s;
        animation-iteration-count: 1;
      }
      @keyframes shake {
        0% { transform: translate(1px, 1px) rotate(0deg); }
        10% { transform: translate(-1px, -2px) rotate(-1deg); }
        20% { transform: translate(-3px, 0px) rotate(1deg); }
        30% { transform: translate(3px, 2px) rotate(0deg); }
        40% { transform: translate(1px, -1px) rotate(1deg); }
        50% { transform: translate(-1px, 2px) rotate(-1deg); }
        60% { transform: translate(-3px, 1px) rotate(0deg); }
        70% { transform: translate(3px, 1px) rotate(-1deg); }
        80% { transform: translate(-1px, -1px) rotate(1deg); }
        90% { transform: translate(1px, 2px) rotate(0deg); }
        100% { transform: translate(1px, -2px) rotate(-1deg); }
      }
      #badge {
        position: absolute;
        top: -5px;
        right: -5px;
        background: red;
        color: white;
        border-radius: 50%;
        padding: 2px 5px;
        font-size: 10px;
      }
    /* Increase z-index for pickerInput dropdown */
    .bootstrap-select .dropdown-menu {
      z-index: 12000 !important;
    }

    .switch-container {
      position: relative;
      display: inline-block;
      width: 40px;
      height: 20px;
      overflow: visible;
    }
    .switch-icon {
      position: absolute;
      transition: transform 0.5s ease-in-out;
      font-size: 18px;
      line-height: 1;
    }
    .green-up {
      color: green;
      top: 0; left: 0;
    }
    .brown-down {
      color: brown;
      top: 0; left: 20px;
    }

    /* Default (not switched) state */
    .green-up { transform: translateX(0) rotate(0deg); }
    .brown-down { transform: translateX(0) rotate(0deg); }

    /* Toggled (switched) state: arrows swap places & rotate 180 */
    #switch_mode.switched .green-up {
      transform: translateX(20px) rotate(180deg);
    }
    #switch_mode.switched .brown-down {
      transform: translateX(-20px) rotate(180deg);
    }
         .waiter-overlay .waiter-spinner svg circle {
      stroke: green !important;
      fill: green !important;
    }
    .swal2-container {
      z-index: 99999 !important;
    }
    #logger_table {
    width: 100%;
    max-height: 500px;
    overflow-y: auto;
    }

    .apply-btn {
        padding: 6px 12px; /* Default padding */
        font-size: 14px;   /* Default font size */
    }
    .apply-btn:hover {
        transform: scale(1.1); /* Increase size by 10% on hover */
        background-color: #218838; /* Slightly darker green on hover */
    }
    #runLoggerLayout.expanded {
    display: flex;
    flex-wrap: wrap;
    gap: 20px;
    }

    #runLoggerLayout.expanded .colLeft,
    #runLoggerLayout.expanded .colRight {
        flex: 1;
        min-width: 300px;
    }
    
    #runLoggerLayout .colLeft .form-group.shiny-input-container {
    margin-right: 5px; /* Reduce the margin between switches */
    margin-left: 0; /* Ensure no extra left margin */
    display: inline-block; /* Ensure they stay inline */
    }

    /* Optional: Reduce space between label and switch */
    #runLoggerLayout .colLeft .control-label {
        margin-right: 5px; /* Reduce space between label and switch */
    }

    /* Ensure the flex container has no extra gap */
    #runLoggerLayout .colLeft .switch-container-flex {
        gap: 0px; /* Already set, but ensure it’s applied */
    }

  "))),

     waiterShowOnLoad(
      html = tagList(
        hostess_loader(
          "loader",
          preset = "fan",
          text_color = "#f2f2f2",
          class = "label-center",
          center_page = TRUE
        ),
        #br(),
        div(id = "loader-message",
            style = "margin-left:0px; color:#f2f2f2;",
            "Initializing the app..."
        )
      ),
      color = "#343a40"
    ),
      #br()),
    
    # year slider for the hover area
    #div(
    #id = "hoverSliderContainer",
    #div(id = "yearSliderContent", uiOutput("yearRangeUI"))
    #),

    tags$head(tags$script(HTML(paste(scrollbar_position_retainer, hide_plot_area, expandedWindow, fullscreen, Notifications, disableSpellCheck, ToggleEpcSoil, waiting ,Titlemanagement,sep = "\n"))),
    #tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
    tags$title("Biome-BGCMuSo Parameter Tuner")
    ),


    # moving the title to the right so the toggleui button has space
    #titlePanel(div(style = "margin-left: 100px;", "B-BGCMuSo Parameter Tuner")),
    titlePanel(
        div(
            style = "display: flex; align-items: center; margin-left: 100px;",
            tags$span("RBBGCMuso Parameter Tuner", style = "font-size: 24px; font-weight: bold; margin-right: 20px;"),
            tags$span(paste0(workdir), style = "font-size: 14px; color: #666;")
        )
    ),
    # Floating toggle button to collapse/restore the control panel
    div(
      id = "toggleUIButton",
      actionButton("toggleUI", label = "Toggle UI", icon = icon("bars"))
    ),
    
    
    div(
        style = "position: absolute; top: 10px; right: 10px; z-index: 1000; display: flex; gap: 10px;",
        actionButton("calib", label = "Calibrate"),
        actionButton("settings_btn", label = NULL, icon = icon("cog")),
        actionButton("toggle_plot_field", "Hide Plot Area"),
        actionButton("toggle_legend", "Hide Legend", icon = icon("eye-slash")),
        actionButton("exit", "Exit", 
               style = "background-color: red; color: white; border-color: darkred; 
                        font-weight: bold; font-size: 16px; 
                        border-radius: 5px;"),
         actionButton("show_notification", label=NULL, icon = icon("bell"))#, style = "display: none;")
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
                                            div(style = "margin-top: 10px; display: flex; align-items: center; gap: 10px;",
                                                actionButton("runModel", "Run Muso",
                                                            style = "background-color: red; color: white; border-color: red;"),
                                                div(
                                                    style = "margin-top: 6px;",
                                                    checkboxInput("runSpinup", "Include Spinup Run", value = FALSE)
                                                )
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
                                            div(style = "display: flex; gap: 10px;",
                                                radioButtons("plotType", "Plot Type:",
                                                            choices = c("Line Plot" = "line", "Scatter Plot" = "scatter"),
                                                            selected = "line",
                                                            inline = TRUE)
                                            ),
                                            actionButton("show_popup", "View Special Plots"),
                                            checkboxInput(
                                                "lastRun", "Show Previous Model Run", value = FALSE
                                            ),
                                            div(style = "display: flex; align-items: center; gap: 0px;",
                                                checkboxInput("showPheno", "Show Phenophases", value = FALSE),
                                                checkboxInput("showHarvest", "Show Harvest Dates", value = TRUE)
                                            ),
                                            div(style = "display: flex; align-items: center; gap: 0px;",
                                                checkboxInput("singleYear", "Single year mode", value = FALSE),
                                                checkboxInput("auto_epc_selection", "Auto EPC selection in single year mode", value = TRUE)
                                            ),
                                            uiOutput("yearRangeUI")
                                        )
                                    ),
                                    div(
                                        style = "display: flex; align-items: center; gap: 10px;",
                                        div(
                                            style = "width: 300px;",
                                            uiOutput("selectEPC")
                                        ),
                                        div(
                                            style = "margin-top: 9.5px;",
                                            title = "Switch between EPC and Soil Sliders",
                                            actionButton(
                                                "switch_mode",
                                                label = tags$span(
                                                    class = "switch-container",
                                                    icon("arrow-up", class = "switch-icon green-up"),
                                                    icon("arrow-down", class = "switch-icon brown-down")
                                                ),
                                                style = "border: none; background: none; padding: 0;"
                                            )
                                        )
                                    ),
                                    div( style = "display: flex; align-items: center; gap: 5px;",
                                        div(style = "width: 300px;",
                                        selectInput("compare_files",
                                        tags$span(style = "color: green;", "Compare EPC from different files"), 
                                        choices = c("None"), 
                                        selected = "None"),
                                        ),    
                                        div(style = "margin-top: 9.5px;",                               
                                            actionButton("reset_compare", "", icon = icon("redo"), title = "Clear all bonds")
                                        ),
                                        div(style = "margin-top: 9.5px;",                               
                                            actionButton("apply_bond_to_sliders", "", icon = icon("link"), title = "Apply bonded EPC values to current sliders")
                                        )
                                    
                                    ),
                                    # Reset buttons
                                    tags$div(
                                        style = "display: flex; align-items: center; gap: 10px;",
                                        actionButton("resetRun", "Reset to Previous Run"),
                                        div(
                                            style = "margin-top: 0px;",
                                            actionButton("restoreParams","Reset to Last Good Values")
                                        )
                                    ),
                                    tags$div(
                                        style = "display: flex; align-items: center; gap: 10px;",
                                        actionButton("resetParams", "Reset to originals"),
                                        div(
                                            style = "margin-top: 6px;",
                                            checkboxInput("restoreOnExit", "Restore originals on exit", value = FALSE)
                                        )
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
                                    )
                            ),
                            tabPanel("Plot Manager",
                                    selectInput("customize_var", "Select Variable to Customize",
                                                choices = NULL),
                                    uiOutput("plot_manager")
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
                                                actionButton("editMeasurementTransforms", "Edit Measurement Data"),
                                                downloadButton("exportData", "Export Data"),
                                                uiOutput("yearRangeMeas"),
                                                checkboxInput("avoid_negative", "Hide negative measurement values on the plot for GPP and TR", value = TRUE),
                                                checkboxInput("keepMapping", "Keep mapping upon export", value = TRUE),
                                                checkboxInput("saveValid","Keep only valid measurement dates upon export", value = FALSE)

                                        ),
                                            
                                        
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
                            ),
                            tabPanel("Output Manager",
                                    fluidRow(
                                        column(12, DT::dataTableOutput("outputTable"))
                                    ),
                                    fluidRow(
                                        div(style = "display: inline-block; margin-right: 5px;",
                                            downloadButton("exportDataSim", "Export Data", width = "auto")),
                                        div(style = "display: inline-block; margin-right: 5px;",
                                            actionButton("editOutputTransforms", "Edit Simulation Data", width = "auto")),
                                        div(style = "display: inline-block; margin-right: 5px;",
                                            actionButton("AppendSim", "Append To Measurement", width = "auto")),
                                        div(style = "display: inline-block; margin-right: 5px;",
                                            actionButton("resetOutputMods", "Reset Edits", width = "auto")),
                                        div(style = "display: inline-block; margin-right: 5px;",
                                            actionButton("make_output", "Create Special Output Variable", width = "auto")),
                                        div(style = "display: inline-block; margin-right: 5px;",
                                            pickerInput("exportCols", "Select columns for export/reset/append",
                                                        choices = NULL, multiple = TRUE, width = "250px", options = list(`actions-box` = TRUE))),
                                        div(style = "display: block; margin-top: 5px;",
                                            checkboxInput("appendSimSuffix", "Append '_sim' to column names upon export/append", value = FALSE)),
                                        div(style = "display: block; margin-top: 5px;",
                                            checkboxInput("autoMulti", "Multiply GPP, TR, NEE, NEP, NBP, MR, GR, HR, SR by 1000 to get gC", value = TRUE))
                                    )
                            ),
                            tabPanel("Run Logger",
                                    fluidRow(
                                        column(12,
                                                div(id = "runLoggerLayout",
                                                    div(class = "colLeft",
                                                        div(style = "margin-bottom: 10px;",
                                                            pickerInput(
                                                                inputId = "selected_runs",
                                                                label = "Select Run(s)",
                                                                choices = NULL,
                                                                multiple = TRUE,
                                                                options = list(
                                                                    `actions-box` = TRUE,
                                                                    `selected-text-format` = "count > 2",
                                                                    `count-selected-text` = "{0} runs selected",
                                                                    `live-search` = TRUE,
                                                                    `dropdown-align-right` = FALSE,
                                                                    `multiple-separator` = ", "
                                                                ),
                                                                width = "300px"
                                                            )
                                                        ),
                                                        div(style = "margin-bottom: 10px; display: flex; align-items: center;",
                                                            actionButton(
                                                                inputId = "applyRunParams",
                                                                label = "Apply",
                                                                class = "apply-btn",
                                                                style = "color: #fff; background-color: #28a745; border-color: #28a745; margin-right: 8px; transition: transform 0.2s;"
                                                            ),
                                                            shiny::span("parameters of run"),
                                                            uiOutput("apply_run_ui", style = "margin: 0 8px;"),
                                                            shiny::span("to current slider values")
                                                        ),
                                                        div(class = "switch-container-flex", 
                                                            style = "margin-bottom: 10px; display: flex; align-items: center; gap: 0px;",
                                                            shinyWidgets::switchInput(
                                                                inputId = "show_comparisons",
                                                                label = "Show Comparisons",
                                                                value = FALSE,
                                                                onLabel = "ON",
                                                                offLabel = "OFF",
                                                                size = "small"
                                                            ),
                                                            shinyWidgets::switchInput(
                                                                inputId = "show_changes_only",
                                                                label = "Changes Only",
                                                                value = FALSE,
                                                                onLabel = "ON",
                                                                offLabel = "OFF",
                                                                size = "small",
                                                                disabled = TRUE
                                                            )
                                                        )
                                                    
                                                    ),
                                                    div(class = "colRight",
                                                        div(style = "margin-bottom: 10px;",
                                                            pickerInput(
                                                                inputId = "selected_params",
                                                                label = "Select Files to Display",
                                                                choices = NULL,
                                                                multiple = TRUE,
                                                                options = list(
                                                                    `actions-box` = TRUE,
                                                                    `selected-text-format` = "count > 3",
                                                                    `count-selected-text` = "{0} files selected",
                                                                    `live-search` = TRUE,
                                                                    `dropdown-align-right` = FALSE,
                                                                    `multiple-separator` = ", "
                                                                ),
                                                                width = "400px"
                                                            )
                                                        ),
                                                        div(style = "margin-bottom: 10px;",
                                                            pickerInput(
                                                                inputId = "selected_param_names",
                                                                label = "Select Parameters",
                                                                choices = NULL,
                                                                multiple = TRUE,
                                                                options = list(
                                                                    `actions-box` = TRUE,
                                                                    `selected-text-format` = "count > 3",
                                                                    `count-selected-text` = "{0} parameters selected",
                                                                    `live-search` = TRUE,
                                                                    `dropdown-align-right` = FALSE,
                                                                    `multiple-separator` = ", "
                                                                ),
                                                                width = "400px"
                                                            )
                                                        )

                                                    )
                                                ),
                                                div(style = "margin-bottom: 10px;",
                                                    shiny::htmlOutput("run_summary")
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
        ),
          shinyjs::hidden(
                div(
                id = "popup_wrapper", # We will show/hide this wrapper div
                
                # We replaced absolutePanel with a standard div and wrapped it
                # in both jqui_draggable and jqui_resizable for full control.
                shinyjqui::jqui_draggable(
                    shinyjqui::jqui_resizable(
                    div(
                        id = "popup_window",
                        # Panel Content 
                        # Define a specific drag handle area
                        div(
                        id = "popup_drag_handle",
                        style = "height: 30px; 
                                cursor: move; 
                                padding: 5px 15px; 
                                border-bottom: 1px solid #ccc; 
                                background: #f0f0f0; 
                                border-radius: 8px 8px 0 0;
                                flex-shrink: 0;", 
                        
                        # Close button (moved inside the handle)
                        actionButton("close_popup", "X", 
                                    style = "float: right; font-weight: bold; color: #555; background: transparent; border: none; padding: 0 5px; margin-top: -2px;"),
                        
                        # Title (moved inside the handle)
                        h4("Special Plots", style="margin: 0; line-height: 20px;")
                        ),
                        # END of handle area
                        
                        # Content area wrapper 
                        # We wrap the content in its own div to apply padding
                        div(
                            style = "padding: 15px;overflow-y: auto; flex-grow: 1;",
                            
                            # Dynamic content
                            #p("This content is dynamic and updates from the main page slider."),
                            uiOutput("dynamic_content_area")
                        ),

                        # handle for the bottom dragging
                        div(
                            id = "popup_drag_handle_bottom",
                            style = "height: 20px; 
                                     cursor: move; 
                                     border-top: 1px solid #ccc; 
                                     background: #f0f0f0; 
                                     border-radius: 0 0 8px 8px;
                                     flex-shrink: 0;"
                        ),
                        
                        # Styling
                        style = "
                                background: #f9f9f9;
                                border: 1px solid #ccc;
                                border-radius: 8px;
                                box-shadow: 0 5px 15px rgba(0,0,0,.3);
                                padding: 0;
                                overflow: hidden;   
                                width: 650px; 
                                height: 450px;
                                display: flex;      
                                flex-direction: column; 
                                "
                    ),
                    # jqui_resizable options
                    # 'handles: "all"' gives all sides and corners
                    options = list(handles = "all")
                    ),
                    # jqui_draggable options
                    # using the ID of the new div as the handle
                    options = list(handle = "#popup_drag_handle, #popup_drag_handle_bottom")
                ),
                
                # apply the positioning to the outer wrapper
                style = "
                            position: absolute;
                            top: 150px;
                            right: 50px;
                            z-index: 1000;
                        "
                )
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

tuneMusoServer2 <- function(input, output, session){
    workdir <- getwd()
    
    # startup animation, waiting for observers to stop calculating before allowing actions
    hostess_instance <- Hostess$new("loader")
    #hostess_instance$start()  # Start the spinner
      session$onFlushed(function() {
            #hostess_instance$close()
            waiter_hide()  # Hide loading screen
         
        }, once = TRUE)  # Run this only once
        
        
    # running the model (calibMuso) in a separate process so if it crashes the shiny app won't
    plan(multisession)
    #for some reason it can't find this function from setupMuso even though it's exported and within namespace, will check later why
    searchBellow <- function(inFile, key, stringP = TRUE,  n=1, management = FALSE){
        
            if(stringP){
                unlist(strsplit(inFile[grep(key,inFile, perl=TRUE)+n],split = "\\s+", useBytes = TRUE))[1]
            } else {
                as.numeric(unlist(strsplit(inFile[grep(key,inFile,perl=TRUE)+n],split = "\\s+", useBytes = TRUE))[1])
            }
    }

    
    settings <- setupMuso()

    soilValues <- reactiveValues(values = NULL)
   

    #epcIni <- settings$epcInput[2]
    #dates <- as.Date(musoDate(settings$startYear, numYears=settings$numYears, leapYearHandling = TRUE)) 
    dates <- as.Date(musoDate(settings$startYear, numYears=settings$numYears, leapYearHandling = TRUE), "%d.%m.%Y")
    rv <- reactiveValues(settings = setupMuso(), epc_files = character(0), epc_labels = character(0), epc_dates = data.frame(), epc_num_labels = character(0), epc_bonds = list())



    # This is going to be ugly but calculating the woody_flag within the observer below and making it reactive is actually hard to deal with when we process the parameters.csv
    if (file.exists(settings$iniInput[2])) {
    iniContent <- readLines(settings$iniInput[2])
    management_file <- searchBellow(iniContent, "MANAGEMENT_FILE", stringP = TRUE, n = 1)
    
    if (file.exists(management_file)) {
        managementContent <- readLines(management_file)
        planting_file <- searchBellow(managementContent, "PLANTING", stringP = TRUE, n = 2)
        
        if (file.exists(planting_file)) {
        
        woody_flag <- 0
        } else {

        single_epc_dat <- readLines(settings$epcInput[2])
        woody_flag <- as.numeric(searchBellow(single_epc_dat, "FLAG", n = 1))
        }
    } else {
      
        single_epc_dat <- readLines(settings$epcInput[2])
        woody_flag <- as.numeric(searchBellow(single_epc_dat, "FLAG", n = 1))
    }
    } else {
    stop("INI file not found!")
    }

    # looking for the planting file if there is any, else use the epc file within the ini file
    observe({
        req(file.exists(settings$iniInput[2]))  # Ensure the INI file exists before reading
        iniContent <- readLines(settings$iniInput[2])
        management_file <- searchBellow(iniContent, "MANAGEMENT_FILE", stringP = TRUE, n = 1)

        if (file.exists(management_file)) {
            managementContent <- readLines(management_file)
            planting_file <- searchBellow(managementContent, "PLANTING", stringP = TRUE, n = 2)
            harvest_file <- searchBellow(managementContent,"HARVESTING", stringP = TRUE, n = 2)

            if (file.exists(planting_file)) {
                #planting_data <- read.table(planting_file, header = TRUE, sep = "", stringsAsFactors = FALSE)
                planting_data <- read.table(planting_file, header = FALSE, sep = "", stringsAsFactors = FALSE, fill = TRUE)[,c(1,6)]
                colnames(planting_data) <- as.character(unlist(planting_data[1, ]))
                planting_data <- planting_data[-1, ]
                colnames(planting_data) <- c("DATE", "CROP(file)")

                epc_files <- unique(unlist(strsplit(paste(planting_data$`CROP(file)`, collapse = " "), " +")))
                
                planting_data$DATE <- as.Date(planting_data$DATE, format = "%Y.%m.%d")
                epc_dates <- planting_data
                #planting_dates <- 
                #print("EPC files found:")
                #print(epc_files)
                if (file.exists(harvest_file)){
                    #browser()
                    harvest_data <- read.table(harvest_file, sep="", header=FALSE, fill=TRUE, stringsAsFactors=FALSE)[,1]
                    harvest_data <- as.Date(harvest_data[-1], format = "%Y.%m.%d")
                    epc_dates$HarvestDates <- harvest_data
                }
                # Ensure that the update happens safely
                isolate({
                    rv$epc_files <- epc_files
                    #rv$woody_flag <- 0
                    rv$epc_labels <- paste0(seq_along(epc_files), ") ", epc_files)
                    rv$epc_num_labels <- paste0(seq_along(rv$epc_files), ")")

                    rv$epc_dates <- epc_dates
                })
            } else {
                warning("Planting file not found: ", planting_file)
                print(paste0("Using EPC file from INI file ", settings$epcInput[2]))
                showNotification(paste0("Using EPC file from INI file ", settings$epcInput[2]), type="message")
                isolate({
                    rv$epc_files <- settings$epcInput[2]

                    single_epc_dat <- readLines(rv$epc_files)
                    #rv$woody_flag <- as.numeric(searchBellow(single_epc_dat, "FLAG",n=1))

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
                showNotification(paste0("Management file not found. Using epc from ini file: ", settings$epcInput[2]), type="message")
                rv$epc_files <- settings$epcInput[2]
                single_epc_dat <- readLines(rv$epc_files)
                #rv$woody_flag <- as.numeric(searchBellow(single_epc_dat, "FLAG",n=1))
                rv$epc_labels <- paste0(seq_along(rv$epc_files), ") ", rv$epc_files)
                rv$epc_num_labels <- paste0(seq_along(rv$epc_files), ")")
                #rv$epc_dates <- as.Date(musoDate(settings$startYear, numYears=1),"%d.%m.%Y")[1]   
                rv$epc_dates <- NULL
            })
        }
    })

    parameters <- read.csv("parameters.csv", stringsAsFactors=FALSE)
   
    


    herbaceous_main <- c(132, 133, 134, 135)
    woody_main      <- c(136, 137, 138, 139)
    # Must be done, otherwise it can't access it since it's reactive
    
    if( woody_flag == 1) {
    main_indices <- c(herbaceous_main, woody_main)
    allocationNames <- c("132" = "Leaf",
                        "133" = "Fine Root",
                        "134" = "Fruit",
                        "135" = "Soft Stem",
                        "136" = "Live Woody Stem",
                        "137" = "Dead Woody Stem",
                        "138" = "Live Coarse Root",
                        "139" = "Dead Coarse Root")
    allocation_pattern <- "^(132|133|134|135|136|137|138|139)\\."
    message("Biome type flag: 'woody' found, including woody allocation parameters")
    } else {
    main_indices <- herbaceous_main
    allocationNames <- c("132" = "Leaf",
                        "133" = "Fine Root",
                        "134" = "Fruit",
                        "135" = "Soft Stem")
    allocation_pattern <- "^(132|133|134|135)\\."


      parametersFixed <- sprintf("%.2f", parameters$INDEX)
        woody_rows <- grepl("^(136|137|138|139)\\.", parametersFixed)
        if(any(woody_rows)) {
            warning("Biome type flag is non-woody, the following parameters.csv lines are not included: ",
                    paste(parameters$INDEX[woody_rows], collapse=", "))
            parameters <- parameters[!woody_rows, ]
        }
    }

    parameters <- parameters[!is.na(parameters$ABREVIATION) & parameters$ABREVIATION != "", ]

    ## Format the INDEX values to preserve trailing zeros for extraction
    parametersFixed <- sprintf("%.2f", parameters$INDEX)

    ## Compute the group (i.e. the digits after the decimal) only for the relevant indices:
    parameters$group <- ifelse(grepl(allocation_pattern, parametersFixed),
                            sub("^(132|133|134|135|136|137|138|139)\\.", "", parametersFixed),
                            NA)

    required_main <- main_indices

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

        sliderRanges <- reactiveValues(epcmin = parameters[,3], epcmax = parameters[,4], soimin = NULL, soimax = NULL)

       
        soil_file <- reactiveVal(NULL)
        soil_parameters <- reactiveVal(NULL)

        observe({
            req(settings$iniInput[2])
            iniContent <- readLines(settings$iniInput[2])
            sf <- searchBellow(iniContent, "SOIL_FILE", stringP=TRUE, n=1)
            soil_file(sf)
            
            req(file.exists("parameters_soil.csv"))
                sp <- read.csv("parameters_soil.csv", stringsAsFactors=FALSE)
                soil_parameters(sp)
                #print(soil_parameters()[,2])
        })
          # if model crashes, we'll store the last good values here to reset later
        lastGoodValues <- reactiveValues(
            epc = list(),  
            soil = NULL    
        )

        prevGoodValues <- reactiveValues(
            epc = list(),  
            soil = NULL    
        )
        InitialDefaultsSoil <- reactiveValues(values = NULL)
          observeEvent(soil_parameters(), {
            # Fetch defaults only once
            req(soil_file(), soil_parameters())
            defaults <- musoGetValues(soil_file(), soil_parameters()[, 2])
            soilValues$values <- defaults
            lastGoodValues$soil <- defaults
            prevGoodValues$soil <- defaults
            InitialDefaultsSoil$values <- defaults
            sliderRanges$soimin <- soil_parameters()[, 3]
            sliderRanges$soimax <- soil_parameters()[, 4]
        }, once = TRUE)  
        

      

        currentMode <- reactiveVal("epc")
        #observeEvent(input$switch_mode, {
        #    
            
        #    print(paste0("Initital mode: ", currentMode()))
        #})


     dailyOutputNames <- reactiveVal(settings$dailyOutputTable$name)


    find_duplicates <- function(df) {
    
        # Find all dates that appear more than once
        duplicated_dates <- df$Date[duplicated(df$Date) | duplicated(df$Date, fromLast = TRUE)]
        
        if (length(duplicated_dates) == 0) {
            return(list(clean_df = df, mergable = NULL, conflicts = NULL))
        }
        
        # Get the actual duplicated rows from the original dataframe
        dupe_rows <- df[df$Date %in% unique(duplicated_dates), ]
        
        # Dataframe without the problematic rows for now
        clean_df <- df[!(df$Date %in% unique(duplicated_dates)), ]
        
        mergable_list <- list()
        conflicts_list <- list()
        
        # Process each duplicated date
        for (d in unique(dupe_rows$Date)) {
            
            rows_for_date <- dupe_rows[dupe_rows$Date == d, ]
            
            # 1. Check for PERFECT duplicates (all values in the row are the same)
            # We can silently remove these.
            distinct_rows <- distinct(rows_for_date)
            
            if (nrow(distinct_rows) == 1) {
                # If only one unique row exists for this date, it was a perfect duplicate.
                # We can just add one instance of it back to our clean_df.
                clean_df <- rbind(clean_df, distinct_rows)
                next # Move to the next duplicated date
            }
            
            # 2. Check for MERGABLE duplicates 
            # We can merge these if the user agrees.
            # The logic here is: for each column, if there's only one non-NA value, it's mergable.
            can_merge <- TRUE
            merged_row <- distinct_rows[1, , drop = FALSE]
            
            for (col in names(distinct_rows)[-1]) { # Assuming first col is "Date"
                non_na_values <- na.omit(distinct_rows[[col]])
                if (length(unique(non_na_values)) > 1) {
                    can_merge <- FALSE
                    break
                }
                # Coalesce the values (take the first non-NA)
                merged_row[[col]] <- coalesce(!!!distinct_rows[[col]])
            }
            
            if (can_merge) {
                mergable_list[[as.character(d)]] <- list(original = distinct_rows, merged = merged_row)
            } else {
                # 3. If it's not a perfect dupe and not mergable, it's a CONFLICT
                conflicts_list[[as.character(d)]] <- distinct_rows
            }
        }
        
        return(list(
            clean_df = clean_df,
            mergable = if(length(mergable_list) > 0) mergable_list else NULL,
            conflicts = if(length(conflicts_list) > 0) conflicts_list else NULL
        ))
    }


    # Reading and processing of measurement files
    measurementData <- reactiveVal(NULL)
    initialMeasurementData <- reactiveVal(NULL)

    observeEvent(input$measurementFile, {
        req(input$measurementFile)
        files <- input$measurementFile
        
        new_data_list <- lapply(seq_len(nrow(files)), function(i) {
            df <- read.table(files$datapath[i], header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
            df$Date <- as.Date(paste(df[[1]], df[[2]], df[[3]], sep = "-"), format = "%Y-%m-%d")
            df[df == -9999] <- NA
            meas <- df[ , -(1:3), drop = FALSE]
            meas <- meas[, !(colnames(meas) %in% c("Date")), drop = FALSE]
            data.frame(Date = df$Date, meas, stringsAsFactors = FALSE, check.names = FALSE)
        })
        
        new_data_combined <- Reduce(function(x, y) dplyr::full_join(x, y, by = "Date"), new_data_list)
        
        req(settings)
        min_year <- as.numeric(format(min(dates), "%Y"))
        max_year <- as.numeric(format(max(dates), "%Y"))
        
        all_dates <- as.Date(unlist(lapply(seq(min_year, max_year), function(yr) {
            start_date <- as.Date(paste0(yr, "-01-01"))
            end_date   <- as.Date(paste0(yr, "-12-31"))
            dates_year <- seq.Date(start_date, end_date, by = "day")
            if (any(format(dates_year, "%m-%d") == "02-29")) {
                dates_year <- dates_year[format(dates_year, "%m-%d") != "12-31"]
            }
            return(dates_year)
        })), origin = "1970-01-01")

        base_df <- data.frame(Date = all_dates)
        sim_start <- min(base_df$Date)
        sim_end   <- max(base_df$Date)
        new_data_complete <- dplyr::left_join(base_df, new_data_combined, by = "Date")
        
        mapping_cols <- grep("_MAPPING$", names(new_data_complete), value = TRUE)
        mapping <- list()
        
        if (length(mapping_cols) > 0) {
            for (map_col in mapping_cols) {
                output_var <- sub("_MAPPING$", "", map_col, fixed = FALSE) # Changed TRUE to FALSE for regex
                meas_cols_str <- unique(na.omit(new_data_complete[[map_col]])) 
                
                if (length(meas_cols_str) > 0) {
                    # Handle cases where meas_cols_str might be a single string with multiple columns
                    all_meas_cols_for_output_var <- unlist(strsplit(meas_cols_str, ","))
                    for (col in all_meas_cols_for_output_var) {
                        col <- trimws(col) # Trim whitespace
                        if (col %in% names(new_data_complete)) {
                            mapping[[col]] <- output_var
                        }
                    }
                }
            }
            new_data_complete <- new_data_complete[, !names(new_data_complete) %in% mapping_cols, drop = FALSE]
        }
        
        if (is.null(measurementData())) {
            measurementData(new_data_complete)
            initialMeasurementData(new_data_complete) # Store initial state
        } else {
            current_meas_data <- measurementData()
            # Identify new columns in new_data_complete not in current_meas_data
            newly_added_cols <- setdiff(colnames(new_data_complete), colnames(current_meas_data))
            
            # Merge, prioritizing new data for existing columns if necessary, though full_join handles this
            combined <- dplyr::full_join(current_meas_data, new_data_complete, by = "Date")
            combined <- dplyr::filter(combined, Date >= sim_start & Date <= sim_end)
            combined <- combined[combined$Date %in% base_df$Date, ]
            measurementData(combined)

            # Update initialMeasurementData carefully
            init_data <- initialMeasurementData()
            # For columns that were newly introduced by this file upload
            new_initials_df <- new_data_complete[, c("Date", newly_added_cols[newly_added_cols %in% colnames(new_data_complete) & !(newly_added_cols %in% colnames(init_data))]), drop = FALSE]
            if(ncol(new_initials_df) > 1) { # If there are actual new columns
                 init_data <- dplyr::full_join(init_data, new_initials_df, by = "Date")
            }
           
            # This part is tricky cause the current logic might overwrite initial state if a column is re-uploaded
            # A more robust initialMeasurementData would store each column as it was *first* loaded
            # For simplicity now we'll merge but this could be refined
            initialMeasurementData(dplyr::full_join(initialMeasurementData(), new_data_complete[, c("Date", setdiff(colnames(new_data_complete), "Date")), drop=FALSE], by = "Date"))


        }
        
        if (length(mapping) > 0) mappingRV(mapping)
    })


        # DUPLICATING CODE FOR THE SECOND MEASUREMENT READ BUTTON I KNOW IT'S HORRIBLE BUT I actually don't see a trivial way to do this (cause the trivial way didn't work), I'll edit it later when I can get my head around it
       observeEvent(input$measurementFile2, {
        req(input$measurementFile2)
        files <- input$measurementFile2
        
        new_data_list <- lapply(seq_len(nrow(files)), function(i) {
            df <- read.table(files$datapath[i], header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
            df$Date <- as.Date(paste(df[[1]], df[[2]], df[[3]], sep = "-"), format = "%Y-%m-%d")
            df[df == -9999] <- NA
            meas <- df[ , -(1:3), drop = FALSE]
            meas <- meas[, !(colnames(meas) %in% c("Date")), drop = FALSE]
            data.frame(Date = df$Date, meas, stringsAsFactors = FALSE, check.names = FALSE)
        })
        
        new_data_combined <- Reduce(function(x, y) dplyr::full_join(x, y, by = "Date"), new_data_list)
        
        req(settings)
        min_year <- as.numeric(format(min(dates), "%Y"))
        max_year <- as.numeric(format(max(dates), "%Y"))

        all_dates <- as.Date(unlist(lapply(seq(min_year, max_year), function(yr) {
            start_date <- as.Date(paste0(yr, "-01-01"))
            end_date   <- as.Date(paste0(yr, "-12-31"))
            dates_year <- seq.Date(start_date, end_date, by = "day")
            if (any(format(dates_year, "%m-%d") == "02-29")) {
                dates_year <- dates_year[format(dates_year, "%m-%d") != "12-31"]
            }
            return(dates_year)
        })), origin = "1970-01-01")

        base_df <- data.frame(Date = all_dates)
        sim_start <- min(base_df$Date)
        sim_end   <- max(base_df$Date)
        new_data_complete <- dplyr::left_join(base_df, new_data_combined, by = "Date")
        
        mapping_cols <- grep("_MAPPING$", names(new_data_complete), value = TRUE)
        mapping <- list() # Initialize mapping for this upload
        
        current_mapping <- isolate(mappingRV()) # Get existing global mapping
        if(is.null(current_mapping)) current_mapping <- list()

        if (length(mapping_cols) > 0) {
            for (map_col in mapping_cols) {
                output_var <- sub("_MAPPING$", "", map_col, fixed = FALSE)
                meas_cols_str <- unique(na.omit(new_data_complete[[map_col]]))
                
                if (length(meas_cols_str) > 0) {
                    all_meas_cols_for_output_var <- unlist(strsplit(meas_cols_str, ","))
                    for (col in all_meas_cols_for_output_var) {
                        col <- trimws(col)
                        if (col %in% names(new_data_complete)) {
                            current_mapping[[col]] <- output_var # Update global mapping
                        }
                    }
                }
            }
            new_data_complete <- new_data_complete[, !names(new_data_complete) %in% mapping_cols, drop = FALSE]
        }
        
        if (is.null(measurementData())) {
            measurementData(new_data_complete)
            initialMeasurementData(new_data_complete)
        } else {
            current_meas_data <- measurementData()
            newly_added_cols <- setdiff(colnames(new_data_complete), colnames(current_meas_data))
            
            combined <- dplyr::full_join(current_meas_data, new_data_complete, by = "Date")
            combined <- dplyr::filter(combined, Date >= sim_start & Date <= sim_end)
            combined <- combined[combined$Date %in% base_df$Date, ]
            measurementData(combined)
            
            init_data <- initialMeasurementData()
            new_initials_df <- new_data_complete[, c("Date", newly_added_cols[newly_added_cols %in% colnames(new_data_complete) & !(newly_added_cols %in% colnames(init_data))]), drop = FALSE]
            if(ncol(new_initials_df) > 1) {
                 init_data <- dplyr::full_join(init_data, new_initials_df, by = "Date")
            }
            initialMeasurementData(dplyr::full_join(initialMeasurementData(), new_data_complete[, c("Date", setdiff(colnames(new_data_complete), "Date")), drop=FALSE], by = "Date"))

        }
        
        if (length(current_mapping) > 0) mappingRV(current_mapping) # Set the updated global mapping
    })



            observe({
            req(measurementData())
            # Get all column names except Date
            cols <- setdiff(colnames(measurementData()), "Date")
            updateCheckboxGroupInput(session, "colsToDelete", choices = cols, selected = character(0))
            })

            
            output$measurementTable <- DT::renderDataTable({
                req(measurementData())
                df <- measurementData()     
                df_display <- df
                


                cols_to_rename <- setdiff(names(df), "Date")
            
                numeric_cols <- sapply(df_display, is.numeric)
                df_display[numeric_cols] <- lapply(df_display[numeric_cols], round, digits = 5)
                df_display[is.na(df_display)] <- "NA"
                
      DT::datatable(df_display,
                editable = FALSE,
                options = list(
                  pageLength = 50,
                  scrollX = FALSE,          # Enables horizontal scrolling, disabled though because for some reason the data table gets small
                  lengthMenu = list(c(10, 25, 50, 100, 500, 1000),
                                    c("10", "25", "50", "100", "500", "1000")),
                  scrollY = "400px",
                  autoWidth = TRUE,
                  stateSave = TRUE
                ),
                rownames = FALSE)

        
        })
        

            # When the user clicks the delete button, remove the selected columns
        observeEvent(input$deleteCols, {
            req(measurementData()) # Ensure measurementData is not NULL before proceeding
            
            current_df <- measurementData()
            colsToRemove <- input$colsToDelete
            
            if (length(colsToRemove) > 0) {
                # Ensure "Date" column is not in colsToRemove
                colsToRemove <- setdiff(colsToRemove, "Date")
                
                if (length(colsToRemove) > 0) {
                    current_mappings <- isolate(mappingRV()) # Get current mappings
                    updated_mappings <- current_mappings
                    was_mapping_changed <- FALSE

                    for (col_to_remove in colsToRemove) {
                        if (col_to_remove %in% names(current_mappings)) {
                            updated_mappings[[col_to_remove]] <- NULL # Remove the mapping for this column
                            was_mapping_changed <- TRUE
                            # Optional: Notify user that a mapping was removed
                             showNotification(paste("Mapping for measurement column '", col_to_remove, "' has been removed as the column was deleted."), type = "message", duration = 7)
                        }
                    }

                    if (was_mapping_changed) {
                        mappingRV(updated_mappings) # Update the reactiveVal with cleaned mappings
                    }
                    

                    remaining_cols <- setdiff(colnames(current_df), colsToRemove)
                    
                    if (length(remaining_cols) == 0 || (length(remaining_cols) == 1 && remaining_cols[1] == "Date")) {
                        measurementData(NULL) 
                        showNotification("All data columns deleted. Measurement data is now empty.", type = "warning", duration = 8)
                    } else {
                        current_df <- current_df[, remaining_cols, drop = FALSE]
                        measurementData(current_df)
                    }
                }
            }
            
            # Update the checkbox group input
            current_data_for_checkbox <- measurementData()
            choices_for_delete <- if(is.null(current_data_for_checkbox) || ncol(current_data_for_checkbox) <=1) {
                                    character(0)
                                } else {
                                    setdiff(colnames(current_data_for_checkbox), "Date")
                                }
            updateCheckboxGroupInput(session, "colsToDelete", choices = choices_for_delete, selected = character(0))
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
                            #choices = c("None", dailyOutputNames()),
                            choices = c("None", rv$settings$dailyOutputTable$name),
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
            
       
            mappingRV(mapping)
            
          
            #print(mappingRV())
        
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

    output$yearRangeMeas <- renderUI({
        req(settings)
        min_year <- as.numeric(format(min(dates), "%Y"))
        max_year <- as.numeric(format(max(dates), "%Y"))

        sliderInput(
            "yearRangeMeasurement",
            label = "Select Year Range for Export",
            min = min_year,
            max = max_year,
            value = c(min_year, max_year),
            step = 1,
            sep = ""
        )
        
    })

    # exporting our data frame
    output$exportData <- downloadHandler(
        filename = function() {
            paste("tuneMusoExport_measurementData-", Sys.Date(), ".txt", sep = "")
        },
        content = function(file) {
            export_df <- measurementData()
            
            # Filter data based on selected year range
            req(input$yearRangeMeasurement)
            min_year <- input$yearRangeMeasurement[1]
            max_year <- input$yearRangeMeasurement[2]
            
            export_df <- export_df[lubridate::year(export_df$Date) >= min_year & lubridate::year(export_df$Date) <= max_year, ]
            
            # Filter empty rows if checkbox is selected
            if (isTRUE(input$saveValid)) {
                # Identify measurement columns,everything currently in export_df that isn't "Date" is a measurement
                meas_cols <- setdiff(names(export_df), "Date")
                
                if (length(meas_cols) > 0) {
                    # rowSums(!is.na(...)) counts how many non-NA values exist in that row.We keep rows where this count is > 0
                    # drop = FALSE ensures this works even if there is only 1 measurement column.
                    has_valid_data <- rowSums(!is.na(export_df[, meas_cols, drop = FALSE])) > 0
                    export_df <- export_df[has_valid_data, ]
                }
            }
            
            # Create Year, Month, and Day columns from the Date column
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
            
            # Reorder columns to match required format
            other_cols <- setdiff(colnames(export_df), c("Date", "Year", "Month", "Day"))
            export_df <- export_df[, c("Year", "Month", "Day", other_cols)]
            
            # Replace NA values with -9999 for export
            export_df[is.na(export_df)] <- -9999
            
            # Write to file
            fwrite(export_df, file, row.names = FALSE, sep = "\t")
        }
    )



    ########## EPC HANDLING ############
    epcValues <- reactiveValues()  # Store EPC values


    # making the first output variable the initial selected variable upon opening the app    
    observe({
        req(settings$dailyOutputTable$name)  
        updatePickerInput(
            session, 
            inputId = "selected_vars", 
            selected = settings$dailyOutputTable$name[1]  
        )
    })



        #ui for epc&soil selection
        output$selectEPC <- renderUI({
        if(currentMode() == "epc") {
        req(length(rv$epc_files) > 0)  

        selectInput(
            "selected_epc",
            tags$span("Select EPC File", style = "color: green; font-weight: bold;"),
            choices = setNames(rv$epc_files, rv$epc_labels),
            selected = rv$epc_files[1],
            width = "100%"
        )
        }
        else {
            selectInput(
                "selected_soil", 
                tags$span("Soil File", style = "color: brown; font-weight: bold;"),
                choices = list(basename(soil_file())),
                selected = basename(soil_file()),
                width = "100%"
                ) #%>% shiny::tagAppendAttributes(disabled = "disabled")
        }
        })

        output$soilFileName <- renderText({
            req(soil_file())
            if(is.null(soil_file()) || !file.exists(soil_file())) {
                return("No soil file found")
            }
            basename(soil_file())
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

    observeEvent(rv$epc_files, {
        isolate({
            req(rv$epc_files)
                for (epc in rv$epc_files) {
                    # Only initialize if not already present
                    #if (is.null(InitialDefaults[[epc]])) {
                        # Call musoGetValues to get the default parameters for this EPC
                        #This is done only once per EPC
                        InitialDefaults[[epc]] <- as.numeric(musoGetValues(epc, parameters[, 2]))
                        lastGoodValues$epc[[epc]] <<- InitialDefaults[[epc]]
                        prevGoodValues$epc[[epc]] <<- InitialDefaults[[epc]]
                        epcValues[[epc]] <<- InitialDefaults[[epc]]
                    #}
                }
        })
        }, once = TRUE)

    # InitialDefaultsSoil <- reactiveValues(values = NULL)
    # observe({
    # req(soil_file(), soil_parameters())
    # if(is.null(InitialDefaultsSoil$values)){

    #  InitialDefaultsSoil$values <- musoGetValues(soil_file(), soil_parameters()[, 2])
    # ##message("testVals length: ", length(InitialDefaultsSoil$values))
    # }
    # })


    #soilDefaultValues <- reactive({
    #    musoGetValues(soil_file(), soil_parameters()[, 2])
    #})

    # Reset the sliders to the default values for the selected EPC
      observeEvent(input$resetParams, { 
        if(currentMode() == "epc"){
            req(input$selected_epc)
            epc <- input$selected_epc

            if(isTRUE(all.equal(epcValues[[epc]], InitialDefaults[[epc]]))) {
                myShowNotification(paste0("Sliders already at initial (boot-up) values for: ", epc), type = "message", duration = 5)
                return()
            }
            
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
            myShowNotification(paste0("Sliders reset to initials (boot-up) for: ", epc), type = "message", duration = 5)
        }
        else {
          req(soil_parameters())

        if(isTRUE(all.equal(soilValues$values, InitialDefaultsSoil$values))) {
            myShowNotification(paste0("Sliders already at initial (boot-up) values for: ", soil_file()), type = "message", duration = 5)
            return()
        }

        soilValues$values <- InitialDefaultsSoil$values
        
        lapply(1:nrow(soil_parameters()), function(i) {
            updateSliderInput(session, paste0("soil_param_", i), 
                            value = InitialDefaultsSoil$values[i])
        })
        myShowNotification(paste0("Sliders reset to initials (boot-up) for: ", soil_file()), type = "message", duration = 5)
       
        }
    })



  observeEvent(input$restoreParams, { 
        if(currentMode() == "epc"){
            req(input$selected_epc)
            epc <- input$selected_epc

            if(isTRUE(all.equal(epcValues[[epc]], lastGoodValues$epc[[epc]]))) {
                myShowNotification(paste0("Sliders already at last good values values for: ", epc), type = "message", duration = 5)
                return()
            }
            
            defaults <- lastGoodValues$epc[[epc]]
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
            myShowNotification(paste0("Sliders reset to last good values for: ", epc), type = "message", duration = 5)
        }
        else {
          req(soil_parameters())

        if(isTRUE(all.equal(soilValues$values, lastGoodValues$soil))) {
            myShowNotification(paste0("Sliders already at last good values values for: ", soil_file()), type = "message", duration = 5)
            return()
        }

        soilValues$values <- lastGoodValues$soil
        
        lapply(1:nrow(soil_parameters()), function(i) {
            updateSliderInput(session, paste0("soil_param_", i), 
                            value = lastGoodValues$soil[i])
        })
        myShowNotification(paste0("Sliders reset to last good values for: ", soil_file()), type = "message", duration = 5)
       
        }
    })


    observeEvent(input$resetRun, { 
        if(currentMode() == "epc"){
            req(input$selected_epc)
            epc <- input$selected_epc

            if(isTRUE(all.equal(epcValues[[epc]], prevGoodValues$epc[[epc]]))) {
                myShowNotification(paste0("Sliders already at previous run's good values values for: ", epc), type = "message", duration = 5)
                return()
            }
            
            defaults <- prevGoodValues$epc[[epc]]
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
            myShowNotification(paste0("Sliders reset to previous run's good values for: ", epc), type = "message", duration = 5)
        }
        else {
          req(soil_parameters())

        if(isTRUE(all.equal(soilValues$values, prevGoodValues$soil))) {
            myShowNotification(paste0("Sliders already at previous run's good values values for: ", soil_file()), type = "message", duration = 5)
            return()
        }

        soilValues$values <- prevGoodValues$soil
        
        lapply(1:nrow(soil_parameters()), function(i) {
            updateSliderInput(session, paste0("soil_param_", i), 
                            value = prevGoodValues$soil[i])
        })
        myShowNotification(paste0("Sliders reset to previous run's values for: ", soil_file()), type = "message", duration = 5)
       
        }
    })


    # original values for pc
  #  defaultValues <- reactive({
  #  req(input$selected_epc)
  #  musoGetValues(input$selected_epc, parameters[, 2])
  #})


    #currentValues <- reactive({
    #    req(input$selected_epc)
    #    epc <- input$selected_epc
    #    if (!is.null(epcValues[[epc]])) {
    #    epcValues[[epc]]
    #    } else {
    #    InitialDefaults[[epc]]
    #    }
    #})




    #exit box not quite working as intended, we'll store its state directly (now it works)
        restoreFlag <- reactiveVal(FALSE)

        observeEvent(input$restoreOnExit, {
            restoreFlag(input$restoreOnExit)
        })

    # file value change method (no overwrite file with original file)
    session$onSessionEnded(function() {
        if (isolate(restoreFlag())) {  
            cat("Restoring all EPC files to original...\n")
            
            isolate({  # Ensuring all reactive values are accessed to avoid buggies
                for (epc in rv$epc_files) {
                    if (!is.null(InitialDefaults[[epc]])) {
                        paramVal <- InitialDefaults[[epc]]  
                        #if(identical(paramVal, epcValues[[epc]])) next
                        settings$epcInput[["normal"]] <- epc
                        prettyChangeMuso(settings, paramVal, calibrationPar = parameters[, 2], 
                                fileToChange = "epc", fixAlloc = FALSE)

                           cat(paste0("Restored ", epc, " to original values.\n"))
                    }
                }
               
               if(!is.null(soil_parameters())){
                
               cat("Restoring SOIL file to original...\n")
                #if(currentMode() == "soil") {
                    paramVal <- InitialDefaultsSoil$values
                    paramVal <- format(paramVal, scientific = FALSE, trim = TRUE)
                    #if(!identical(paramVal, soilValues$values)) {
                    prettyChangeMuso(settings, paramVal, 
                     calibrationPar = soil_parameters()[,2],
                     fileToChange = "soil", fixAlloc = FALSE)
                #}  
                cat(paste0("Restored ", soil_file(), " to original values.\n"))
                    #}
                    #else{
                    #    cat(paste0(soil_file()," values were unchanged.\n"))
                    #}
               }

            })
        }
        else if (isolate(modelCrashed())) {
           if(length(isolate(lastGoodValues$epc)) > 0){
                isolate({
                    for(epc in names(lastGoodValues$epc)) {
                        # Restore reactive storage for this epc file
                        paramVal <- lastGoodValues$epc[[epc]]
                        settings$epcInput[["normal"]] <- epc
                            prettyChangeMuso(settings, paramVal, calibrationPar = parameters[, 2], 
                                    fileToChange = "epc", fixAlloc = FALSE)
                    }
                })
           }
                cat("Restored EPC files to last good values to avoid saving files that cause model crash\n")
                if(length(isolate(lastGoodValues$soil)) > 0){
                    if(!is.null(isolate(soil_parameters()))){ # checking whether we should check for soil file
                        isolate({
                            paramVal <- lastGoodValues$soil
                            paramVal <- format(paramVal, scientific = FALSE, trim = TRUE)
                                prettyChangeMuso(settings, paramVal, 
                                calibrationPar = soil_parameters()[,2],
                                fileToChange = "soil", fixAlloc = FALSE)
                        })
                cat("Restored SOIL files to last good values to avoid saving files that cause model crash\n")
                    }
                
                }
            else {
                cat("No last good values found, unable to restore EPC and/or Soil files to avoid saving files that cause model crash\n")
            }
        }
    })

    # dosen't work for some reason
     #   tempdir <- "C:/muso/Polkovice/czpol70/temp_epc/"
        
    #    backupPaths <- list()
    #    for (epc in isolate(rv$epc_files)) {
    #        tempPath <- file.path(tempdir(), basename(epc))
    #        if (file.copy(epc, tempPath, overwrite = TRUE)) {
    #            backupPaths[[epc]] <- tempPath
    #            cat(paste("Backup created for", epc, "at", tempPath, "\n"))
    #        } else {
    #            cat(paste("Failed to backup", epc, "\n"))
    #        }
    #    }

    #session$onSessionEnded(function() {
    #    if (isolate(restoreFlag())) {
    #        cat("Restoring all EPC files to original...\n")
    #        for (epc in rv$epc_files) {
    #        backupPath <- backupPaths[[epc]]
    #        if (!is.null(backupPath) && file.exists(backupPath)) {
    #            if (file.copy(backupPath, epc, overwrite = TRUE)) {
    #            cat(paste("Restored", epc, "from backup.\n"))
    #            } else {
    #            cat(paste("Failed to restore", epc, "\n"))
    #            }
    #        }
    #        }
    #    }
    #})


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

       autoCalcStates <- reactiveValues()

         screenUI <- div(
            style="color:green;",
            spin_3(),
            h3("Rendering UI, please wait...")
        )

        wSlider <- Waiter$new(
            html = screenUI,
            color = "transparent"
        )

        #sliderRanges <- reactiveValues(epcmin = parameters[,3], epcmax = parameters[,4], soimin = soil_parameters()[,3], soimax = isolatesoil_parameters()[,4])
        #sliderRanges <- reactiveValues(epcmin = parameters[,3], epcmax = parameters[,4])
        
        # making the slider ui (both standard and dependent)
        output$param_sliders <- renderUI({
            #wSlider$show()
            if(currentMode() == "epc"){
            req(input$selected_epc)
            epc <- input$selected_epc
            #vals <- currentValues()
            vals <-  isolate(epcValues[[epc]])
            if(length(vals) < nrow(parameters) || any(is.na(vals))) {
                vals <- InitialDefaults[[epc]]
            }
            
            dep_indices <- which(!is.na(parameters$group))
            non_dep_indices <- setdiff(seq_len(nrow(parameters)), dep_indices)
            
            #maxEpc <- sliderRanges$epcmax
            #minEpc <- sliderRanges$epcmin
            # min <- if (is.null(min_custom)) parameters[i,3] else min_custom
            # max <- if (is.null(max_custom)) parameters[i,4] else max_custom
            standard_sliders <- lapply(non_dep_indices, function(i) {
                safe_value <- if (is.null(vals[i]) || is.na(vals[i])) parameters[i, 3] else vals[i]
                sliderInput(
                paste0("param_", i),
                label = parameters$ABREVIATION[i],
                min   = sliderRanges$epcmin[i],                       #minEpc[i], 
                max   = sliderRanges$epcmax[i],                       #maxEpc[i], 
                value = safe_value,
                step  = ( sliderRanges$epcmax[i] - sliderRanges$epcmin[i] ) / 100
                )
            })
            
            dep_groups <- unique(parameters$group[dep_indices])
            
            dependent_sliders <- lapply(dep_groups, function(g) {
                group_rows <- which(!is.na(parameters$group) &
                                    parameters$group == g &
                                    as.numeric(sub("\\..*", "", parameters$INDEX)) %in% main_indices)
                group_rows <- group_rows[order(as.numeric(sub("\\..*", "", parameters$INDEX[group_rows])))]
                
                slider_list <- lapply(group_rows, function(i) {
                safe_value <- if (is.null(vals[i]) || is.na(vals[i])) parameters[i, 3] else vals[i]
                slider_id <- paste0("dep_", parameters$INDEX[i])
                lock_btn_id <- paste0("lock_", parameters$INDEX[i])
                div(
                    #style = "width: 300px;",
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
                 autoCalcVal <- if (is.null(autoCalcStates[[g]])) FALSE else autoCalcStates[[g]]
                # condition whether hide plot area is active or not (so upon epc switching the sliders will retain their aligments)
                containerClass <- if (!is.null(input$plotHidden) && input$plotHidden) "dependentSliderContainer expanded" else "dependentSliderContainer"
                
                tagList(
                h4(groupLabel),
                    div(style = "margin-bottom: 10px;",
                        div(style = "display: inline-block; vertical-align: middle;",
                            checkboxInput(inputId = paste0("autoCalc_", g), label = "Auto‑calc", value = autoCalcVal)
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
                #wSlider$hide()
            }
            else {
            req(currentMode() == "soil")
            req(soil_parameters(), soilValues$values)
            df <- soil_parameters()
            #df <- list(sliderRanges$soimin, sliderRanges$soimax)
            #minSoil <- sliderRanges$soimin
            #maxSoil <- sliderRanges$soimax

            step_threshold <- 1e-5 
            vals <- soilValues$values
            div(
                id = "standardSliders",
                class = if (!is.null(input$plotHidden) && input$plotHidden) "expanded" else "",
                lapply(seq_len(nrow(df)), function(i) {


                    param_min <- sliderRanges$soimin[i]
                    param_max <- sliderRanges$soimax[i]
                    param_val <- vals[i] # The current value
                    param_name <- df$ABREVIATION[i]
                    input_id <- paste0("soil_param_", i)

                    potential_step <- if (param_max > param_min) {
                        (param_max - param_min) / 100
                    } else {
                        0 # Or some other default if min >= max
                    }

                ui_element <- if (potential_step > 0 && potential_step < step_threshold) {
                
                # --- Step is too small: Use sliderTextInput ---
                
                # Generate choices (Example: Logarithmic scale, good for small positive ranges)
                if (param_min > 0 && param_max > param_min) {
                     num_steps <- 10 # Adjust number of steps
                     log_steps <- seq(log10(param_min), log10(param_max), length.out = num_steps)
                     choices_vec <- signif(10^log_steps, digits = 2) # Use signif for cleaner steps
                } else {
                     # Fallback: Linear scale if min <= 0 or max <= min
                     num_steps <- 10
                     choices_vec <- round(seq(param_min, param_max, length.out = num_steps), 8) # Round appropriately
                }


                choices_vec <- unique(sort(c(param_min, choices_vec, param_max)))

                # Specific choices for known problematic parameters 
                #  if (param_name == "RateScalarRSOC") {
                #      choices_vec <- c(1e-6, 2e-6, 5e-6, 1e-5, 2e-5, 5e-5, 1e-4) 
                #  }


                selected_val <- if (param_val %in% choices_vec) {
                    param_val
                } else {
                    # If not exact match, pick the closest choice
                    choices_vec[which.min(abs(choices_vec - param_val))] 
                }


                #  shinyWidgets::sliderTextInput(
                #     inputId = input_id,
                #     label   = param_name,
                #     choices = choices_vec,
                #     selected = selected_val, 
                #     grid = TRUE # Show grid marks
                #     # force_edges = TRUE # Optional: Ensures slider snaps to min/max easily
                # )

                numericInput(
                    inputId = input_id,
                    label   = param_name,
                    value   = param_val, # Use the closest choice
                    min     = param_min,
                    max     = param_max,
                    step    = potential_step # Use the calculated step
                )
                
            } else {
                
                # --- Step is large enough: Use standard sliderInput ---
                sliderInput(
                    inputId = input_id,
                    label   = param_name,
                    min     = param_min,
                    max     = param_max,
                    value   = param_val,
                    step    = potential_step # Use the calculated step
                )
            }
                    
            div(
                class = "slider-col",
                # style = "width: 300px;", # Optional styling
                ui_element # This will be either the sliderInput or sliderTextInput
            )


                })
            )
           
            
        }
         #wSlider$hide()
         #ui
        }) 

        lapply(unique(parameters$group[!is.na(parameters$group)]), function(g) {
            observeEvent(input[[paste0("autoCalc_", g)]], {
                autoCalcStates[[g]] <- input[[paste0("autoCalc_", g)]]
            }, ignoreInit = TRUE)
        })
    
     sliderHistory <- reactiveValues()

# Set time threshold for oscillation detection
# oscillationThreshold <- 3  # Seconds
# debounceTime <- 300         # Reduce debounce time

# observe({
#     req(input$selected_epc, parameters)
    
#     lapply(seq_len(nrow(parameters)), function(i) {
#         sliderId <- if (is.na(parameters$group[i])) {
#             paste0("param_", i)
#         } else {
#             paste0("dep_", parameters$INDEX[i])
#         }

#         debouncedSliderVal <- reactive({ input[[sliderId]] }) %>% debounce(debounceTime)
        
#         observeEvent(debouncedSliderVal(), {
#             isolate({
#                 epc <- input$selected_epc
#                 currentVal <- debouncedSliderVal()
                
#                 # Initialize history tracking
#                 if (is.null(sliderHistory[[sliderId]])) {
#                     sliderHistory[[sliderId]] <- list(values = numeric(), timestamps = numeric())
#                 }
                
#                 # Store last 10 values with timestamps
#                 sliderHistory[[sliderId]]$values <- c(sliderHistory[[sliderId]]$values, currentVal)
#                 sliderHistory[[sliderId]]$timestamps <- c(sliderHistory[[sliderId]]$timestamps, Sys.time())
                
#                 # Keep only last 10 values for performance
#                 if (length(sliderHistory[[sliderId]]$values) > 10) {
#                     sliderHistory[[sliderId]]$values <- tail(sliderHistory[[sliderId]]$values, 10)
#                     sliderHistory[[sliderId]]$timestamps <- tail(sliderHistory[[sliderId]]$timestamps, 10)
#                 }
                
#                 # Detect oscillation
#                 if (length(sliderHistory[[sliderId]]$values) > 5) {
#                     recentValues <- sliderHistory[[sliderId]]$values
#                     recentTimes <- sliderHistory[[sliderId]]$timestamps
#                     timeDiffs <- diff(recentTimes) # Check time intervals between updates
                    
#                     # Check if values are switching between two numbers
#                     uniqueVals <- unique(recentValues)
#                     if (length(uniqueVals) == 2 && all(timeDiffs < oscillationThreshold / length(timeDiffs))) {
#                         myShowNotification(paste0("Oscillation detected in ", sliderId, ". Resetting to last good values."), 
#                                            type = "warning", duration = 5)

#                         updateSliderInput(session, sliderId, value = lastGoodValues$epc[[epc]][i])
                        
#                         # Reset history for this slider
#                         sliderHistory[[sliderId]] <- list(values = numeric(), timestamps = numeric())
#                     }
#                 }
#             })
#         }, ignoreInit = TRUE)
#     })
# })



    
        # saving the slider values as we move them for the soilValues
        observe({
            req(soil_parameters())
            lapply(seq_len(nrow(soil_parameters())), function(i) {
                observeEvent(input[[paste0("soil_param_", i)]], {
                isolate({
                    current <- soilValues$values
                    current[i] <- input[[paste0("soil_param_", i)]]
                    soilValues$values <- current
                })
                }, ignoreInit = TRUE)
            })
        })

        # same as the above but for epcValues
        observe({
            req(input$selected_epc, parameters)
            
            lapply(seq_len(nrow(parameters)), function(i) {
                # Non-dependent sliders
                 if (is.na(parameters$group[i])) {
                    sliderValsDebounced <- reactive({ 
                            input[[paste0("param_", i)]]
                        }) %>% debounce(200)

                observeEvent(sliderValsDebounced(), {
                    isolate({
                    
                    current <- epcValues[[input$selected_epc]]
                   
                    current[i] <- input[[paste0("param_", i)]]
                   
                    epcValues[[input$selected_epc]] <- current
                    })
                }, ignoreInit = TRUE)
                } else {
                # dependent sliders
                observeEvent(input[[paste0("dep_", parameters$INDEX[i])]], {
                    isolate({
                    current <- epcValues[[input$selected_epc]]
                    current[i] <- input[[paste0("dep_", parameters$INDEX[i])]]
                    epcValues[[input$selected_epc]] <- current
                    })
                }, ignoreInit = TRUE)
               }
            })
        })

        lastSelectedEPC <- reactiveVal(NULL)
        observeEvent(input$switch_mode, {
            #print(currentMode())
        # Save current state before switching
           if (currentMode() == "epc") {

            #if (!is.null(input$selected_epc)) {
            #    lastSelectedEPC(input$selected_epc)
            #}
               if (is.null(soil_parameters())) {
                    if (file.exists("parameters_soil.csv")) {
                        sp <- tryCatch(
                        read.csv("parameters_soil.csv", stringsAsFactors = FALSE),
                        error = function(e) NULL
                        )
                        soil_parameters(sp)
                    }
                    # If still NULL after trying to read, notify and revert
                    if (is.null(soil_parameters())) {
                        showNotification("parameters_soil.csv not found. You can try pressing it again once its made", type = "error")
                        session$sendCustomMessage("setSwitchState", list(switched = FALSE))
                        return(NULL)
                    }
                }
                #updateCurrentEPCValues()
            } else {
                updateCurrentSoilValues()
                  
            }
            
        # Toggle mode
            #print(currentMode())
            #new_mode <- ifelse(currentMode() == "epc", "soil", "epc")
          
            new_mode <- ifelse(currentMode() == "epc", "soil", "epc")
            currentMode(new_mode)
            #new_label <- ifelse(currentMode() == "epc", "Switch to Soil", "Switch to EPC")
            
            session$sendCustomMessage("setSwitchState", list(switched = (new_mode == "soil")))
            #updateActionButton(session, "switch_mode")
            #,
                #label = new_label)
            
        })


        #observeEvent(currentMode(), {
        #    if (currentMode() == "epc") {
        #        if (!is.null(lastSelectedEPC()) && lastSelectedEPC() %in% rv$epc_files) {
                # Add a slight delay to ensure the UI has re-rendered, if needed
        #        invalidateLater(100, session)
        #        updateSelectInput(session, "selected_epc", selected = lastSelectedEPC())
        #        }
        #    }
        #})

        # reactive value that will track the locked or unlock state of the allocation locking button
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
        # observer for lock evenets
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
             #wSlider$show()
            req(input$selected_epc)
            
            tol <- 1e-6  # small tolerance to avoid oscillation
            
            # Get the unique groups
            dep_groups <- unique(parameters$group[!is.na(parameters$group)])
            
            lapply(dep_groups, function(g) {

                # For group g, get the rows and slider IDs
                group_rows <- which(!is.na(parameters$group) &
                                    parameters$group == g &
                                    as.numeric(sub("\\..*", "", parameters$INDEX)) %in% main_indices)
                group_rows <- group_rows[order(as.numeric(sub("\\..*", "", parameters$INDEX[group_rows])))]
                ids <- paste0("dep_", parameters$INDEX[group_rows])
                
                observeEvent(input[[paste0("autoCalc_", g)]], {
                    autoCalcStates[[g]] <- input[[paste0("autoCalc_", g)]]
                })
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
                        if (isTRUE(isolate(autoCalcStates[[g]]))) {
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
            #wSlider$hide()
        })
        
        

    # immediately recalculate when user presses the auto-calc button
   observe({
        req(input$selected_epc)

        dep_groups <- unique(parameters$group[!is.na(parameters$group)])

        lapply(dep_groups, function(g) {
            observeEvent(input[[paste0("autoCalc_", g)]], {
                # When autoCalc is toggled ON, INSTANTLY perform a recalculation for group g 
                if (isTRUE(isolate(autoCalcStates[[g]]))) {
                    group_rows <- which(!is.na(parameters$group) & parameters$group == g)
                    ids <- paste0("dep_", parameters$INDEX[group_rows])

                    # Calculate total locked and available for unlocked sliders
                    locked_vals <- unlist(lapply(ids, function(x) {
                        if (isTRUE(lockStates[[x]])) {
                            if (is.null(input[[x]])) 0 else as.numeric(input[[x]])
                    } else 0
                    }))
                    L <- sum(locked_vals)
                    available_total <- 1 - L

                    # Identify unlocked sliders
                    unlocked_ids <- ids[!sapply(ids, function(x) isTRUE(lockStates[[x]]))]
                    current_unlocked <- unlist(lapply(unlocked_ids, function(x) {
                        if (is.null(input[[x]])) 0 else as.numeric(input[[x]])
                    }))
                    total_unlocked <- sum(current_unlocked)

                    if (length(unlocked_ids) > 0) {
                        new_unlocked <- if (total_unlocked == 0) {
                            rep(available_total / length(unlocked_ids), length(unlocked_ids))
                        } else {
                            unname(available_total * (current_unlocked / total_unlocked))
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
                                as.numeric(sub("\\..*", "", parameters$INDEX)) %in% main_indices)
            group_rows <- group_rows[order(as.numeric(sub("\\..*", "", parameters$INDEX[group_rows])))]
            ids <- paste0("dep_", parameters$INDEX[group_rows])
            
            output[[paste0("sumCounter_", g)]] <- renderText({
            vals <- unlist(lapply(ids, function(x) {
                if (is.null(input[[x]])) 0 else as.numeric(input[[x]])
            }))
            total <- sum(vals)
            if (total > 1) {
                paste0("Total sum: ", round(total, 2), " (Warning: Sum > 1!)")
            } 
            else if (total < 1){
                paste0("Total sum: ",round(total, 2), " (Warning: Sum < 1!)")
            }
            else {
                paste0("Total sum: ", round(total, 2))
            }
            })
        })
        
    })
    




    # creating tracker that will avoid auto-update from running the model upon epc switching (not yet used later)
    #updatingEPC <- reactiveVal(FALSE)
    
        last_year <- reactiveVal(NULL)

        debounced_yearRange2 <- reactive({ input$yearRange }) %>% debounce(500)

        observeEvent(
        list(currentMode(),debounced_yearRange2(), input$auto_epc_selection, input$singleYear), 
        {
            req(!is.null(input$auto_epc_selection),
                !is.null(input$singleYear),
                rv$epc_dates,
                currentMode() == "epc"
                )
            
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
                rv$epc_dates[["CROP(file)"]] %in% rv$epc_files, 
            ]
            
            if (nrow(filtered) > 0) {
                # Get the available EPC file names for this year.
                choices <- unique(filtered[["CROP(file)"]])
                # Get corresponding labels 
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
                earliest_epc <- earliest_row[["CROP(file)"]]
                updateSelectInput(session, "selected_epc", selected = earliest_epc)
                }
                
                # Store the current year.
                last_year(selected_year)
            }
            
            } else {
            current_selection <- isolate(input$selected_epc)
            # When auto-selection is disabled, update to show the full list with labels.
            full_choices <- setNames(rv$epc_files, rv$epc_labels)
            updateSelectInput(session, "selected_epc", choices = full_choices, selected = current_selection)
            }
        })




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
            myShowNotification(paste("Updated", col, "with NA transformation"), type = "message", duration = 5)
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
        if(currentMode() == "epc"){
        updateCurrentEPCValues() # HAHA! I knew this function would be useful!! (I hope)
        }
        else{
        updateCurrentSoilValues()
        }
        session$sendCustomMessage("toggle_plot_visibility", list())
    })



    # a function that will update the current epc values in case we need to eplxicitly call this
    updateCurrentEPCValues <- function() {
        if(currentMode() == "epc"){
        req(input$selected_epc)
        epc <- input$selected_epc
        # Retrieving the current vector, if missing, fall back to defaultValues
        updated <- epcValues[[epc]]
        if (is.null(updated) || length(updated) < nrow(parameters))
            updated <- InitialDefaults[[epc]]
        
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
    }


    updateCurrentSoilValues <- function() {
        if(currentMode() == "soil"){
            req(soil_file())
            updated <- soilValues$values
            
            # Let's see the lengths first:
            #cat("length(updated) =", length(updated), "\n")
            #cat("nrow(soil_parameters()) =", nrow(soil_parameters()), "\n")
            
            for (i in seq_len(nrow(soil_parameters()))) {
                sliderVal <- input[[paste0("soil_param_", i)]]
                #cat("i=", i, " sliderVal=", sliderVal, "\n")
                
                # If sliderVal is NULL, you can't do updated[i] <- NULL
                if (!is.null(sliderVal)) {
                updated[i] <- sliderVal
                }
            }
            
            soilValues$values <- updated
            #cat("All done, length(updated)=", length(updated), "\n")
        }
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
        # for the linear approximation. We assumre
        #midpoints <- list(
        #    1.5, 6.5, 20, 45, 75, 105, 135, 175, 300, 700
        #)
        
        # we will check the available layers and for them we'll calc the midpoints
        get_midpoints <- function(layers) {
            sapply(layers, function(x) mean(x))
        }

        midpoint_trend_swc <- function(swc_values, depth, layers) {
            midpoints <- get_midpoints(layers)

            # Find the closest midpoints below and above the given depth
            below_index <- max(which(midpoints <= depth))
            above_index <- min(which(midpoints >= depth))

            # Ensure valid indices
            if (is.na(below_index) || is.na(above_index) || below_index == above_index) {
                return(NA)  # Return NA if no valid interpolation can be performed
            }

            # Get SWC values at the two midpoints
            SWC_below <- swc_values[below_index]
            SWC_above <- swc_values[above_index]

            # Get the actual depths of the midpoints
            depth_below <- midpoints[below_index]
            depth_above <- midpoints[above_index]

            # Compute the weights based on distance
            weight_above <- (depth - depth_below) / (depth_above - depth_below)
            weight_below <- 1 - weight_above

            # Compute interpolated SWC
            interpolated_swc <- weight_below * SWC_below + weight_above * SWC_above

            return(interpolated_swc)
        }

         depth_weighted_average <- function(values, min_depth, max_depth, layers) {
            total_value <- 0
            total_thickness <- 0

            if (min_depth >= max_depth) {
                return(NA) # Invalid range
            }

            for (i in 1:length(values)) {
                layer <- layers[[i]]
                value <- values[i]
                
                layer_start <- layer[1]
                layer_end <- layer[2]

                # Find the overlap
                eff_start <- max(layer_start, min_depth)
                eff_end <- min(layer_end, max_depth)

                # Calculate thickness of the overlap
                thickness <- eff_end - eff_start

                if (thickness > 0) {
                    total_value <- total_value + (value * thickness)
                    total_thickness <- total_thickness + thickness
                }
            }

            if (total_thickness > 0) {
                return(total_value / total_thickness)
            } else {
                return(NA) # No overlap found
            }
        }


        # calc_midpoint_swc <- function(swc_values, min_depth, max_depth, layers) {
        #     midpoints <- get_midpoints(layers)
            
        #     # Create a linear interpolation function of SWC vs. depth (using midpoints)
        #     swc_profile <- approxfun(midpoints, swc_values, rule = 2)
            
        #     # Integrate the interpolated function over the desired depth range
        #     integrated_value <- integrate(swc_profile, lower = min_depth, upper = max_depth)$value
            
        #     # Compute the average SWC over that depth range
        #     average_swc <- integrated_value / (max_depth - min_depth)
        #     return(average_swc)
        # }




    #calc_weighted_swc <- function(swc_values, min_depth, max_depth, layers) {
        # Use only as many layers as are available in swc_values:
    #    n <- length(swc_values)
    #    total_weight <- 0
    #    weighted_sum <- 0
    #    for (i in seq_len(n)) {
    #        layer_min <- layers[[i]][1]
    #        layer_max <- layers[[i]][2]
    #        overlap <- max(0, min(max_depth, layer_max) - max(min_depth, layer_min))
    #        if (overlap > 0) {
    #            weighted_sum <- weighted_sum + swc_values[i] * overlap
    #            total_weight <- total_weight + overlap
    #        }
    #    }
    #    if (total_weight > 0) return(weighted_sum / total_weight) else return(NA)
    #}

        updateLastGoodValues <- function() {
            # Cycle through all epc files and store their current slider values
            for(epc in names(lastGoodValues$epc)) {
                lastGoodValues$epc[[epc]] <<- as.numeric(epcValues[[epc]])
            }
            if(!is.null(soil_parameters())) {
                lastGoodValues$soil <<- soilValues$values
            }
            
        }

        updatePrevGoodValues <- function() {

            for(epc in names(prevGoodValues$epc)) {
                prevGoodValues$epc[[epc]] <<- as.numeric(lastGoodValues$epc[[epc]])
            }
            if(!is.null(soil_parameters())) {
                prevGoodValues$soil <<- lastGoodValues$soil
            }
        }


        screen <- div(
            style="color:green;",
            spin_3(),
            h3("Calculating model results...")
        )

        w <- Waiter$new(
            html = screen,
            color = "transparent"
        )

        runLogs <- reactiveVal(list())



        # Gathering all the rows in the epc file used in prettyChangeMuso and musoGetValues
        rows_epc <- 131:142
        cols_epc <- 0:6
        col_rows_epc <- c(expand.grid(rows_epc, 60 + cols_epc)[, 2] / 100 + expand.grid(rows_epc, 60 + cols_epc)[, 1])
        params_epc <- c(4:6, 9:77, 80:96, 99:113, 116:127, col_rows_epc)

        # same for soil
        rows_soil <- 88:104
        cols_soil <- 0:9
        col_rows_soil <- c(expand.grid(rows_soil, 60 + cols_soil)[, 2] / 100 + expand.grid(rows_soil, 60 + cols_soil)[, 1])
        params_soil <- c(4:11, 14:40, 43:59, 62:76, 79:85, col_rows_soil)

        # gather all the epc files in the working directory and exclude the ones used in the general pool
        compare_epc <- reactive({
            req(rv$epc_files)
            all_epc_files <- list.files(pattern = "\\.epc$", full.names = FALSE)
            setdiff(all_epc_files, rv$epc_files)
        })

        # gather all the soil files in the working directory and exclude the one used 
        compare_soil <- reactive({
            req(soil_file())
            all_soil_files <- list.files(pattern = "\\.soil$", full.names = FALSE)
            setdiff(all_soil_files, soil_file())
        })    

    # updating the bonds that are selected when switchin from main epc pool
    observeEvent(input$selected_epc, {
            req(input$selected_epc, compare_epc())
            
            # Get all possible comparison files
            available_comps <- compare_epc()
            
            # Get the currently saved bond for this EPC
            current_bonds <- rv$epc_bonds
            selected_bond <- current_bonds[[input$selected_epc]]
            
            # If no bond is set, default to "None"
            if (is.null(selected_bond)) {
                selected_bond <- "None"
            }
            
            # Update the UI
            updateSelectInput(session, "compare_files",
                choices = c("None", available_comps),
                selected = selected_bond
            )
    }, ignoreNULL = TRUE, ignoreInit = TRUE)


    # updating the bonds when the user selects a different comparison file
    observeEvent(input$compare_files, {
        # We req both so we know *what* to bond and *which* file to bond it to
        req(input$selected_epc, !is.null(input$compare_files))
        
        current_bonds <- rv$epc_bonds
        
        # Only update if the value is different, prevents loops
        if (!identical(current_bonds[[input$selected_epc]], input$compare_files)) {
            current_bonds[[input$selected_epc]] <- input$compare_files
            rv$epc_bonds <- current_bonds
            
            # Optional: notify user that bond is set
            if(input$compare_files != "None") {
                myShowNotification(paste("Bond set:", input$selected_epc, "->", input$compare_files), 
                                type = "message", duration = 4)
            } else {
                myShowNotification(paste("Bond removed for:", input$selected_epc), 
                                type = "message", duration = 4)
            }
        }
    
    }, ignoreNULL = TRUE, ignoreInit = TRUE)


    # reset bonds
    observeEvent(input$reset_compare, {
        
        # Reset the reactive value back to an empty list
        rv$epc_bonds <- list()
        
        # Also update the currently displayed selectInput to "None"
        updateSelectInput(session, "compare_files", selected = "None")
        
        myShowNotification("All EPC file bonds have been cleared.", 
                         type = "message", 
                         duration = 5)
    })


        observeEvent(input$apply_bond_to_sliders, {
        req(input$selected_epc)
        
        current_bonds <- rv$epc_bonds
        bond_file <- current_bonds[[input$selected_epc]]
        
        # Check that a bond exists
        if (is.null(bond_file) || bond_file == "None") {
            myShowNotification("No bond is set for the currently selected EPC.", type = "warning", duration = 7)
            return()
        }
        
        comp_epc_path <- file.path(workdir, bond_file)
        
        # Check that the bonded file exists
        if (!file.exists(comp_epc_path)) {
            myShowNotification(paste("Bonded file not found:", bond_file), type = "error", duration = 7)
            return()
        }

        myShowNotification(paste("Applying slider values from", bond_file, "..."), type = "message", duration = 5)

        tryCatch({
            # Get Values from Bonded File 
            # Get ONLY the parameter rows that correspond to sliders
            param_rows_to_get <- parameters[, 2]
            comp_values <- musoGetValues(comp_epc_path, param_rows_to_get)
            
            if (length(comp_values) != nrow(parameters)) {
                myShowNotification("Parameter mismatch. Could not apply values.", type = "error", duration = 7)
                return()
            }

            #  Update Stored Value (epcValues)
            # Use <<- to update the global reactive list
            epcValues[[input$selected_epc]] <<- comp_values
            
            # Update UI Sliders
            # Loop through all parameters and update their corresponding UI inputs
            
            non_dep_indices <- which(is.na(parameters$group))
            for (i in non_dep_indices) {
                inputId <- paste0("param_", i)
                # Assuming sliderInput; change to updateNumericInput if needed
                updateSliderInput(session, inputId, value = comp_values[i])
            }
            
            dep_indices <- which(!is.na(parameters$group))
            for (i in dep_indices) {
                inputId <- paste0("dep_", parameters$INDEX[i])
                # Assuming sliderInput; change to updateNumericInput if needed
                updateSliderInput(session, inputId, value = comp_values[i])
            }
            
            myShowNotification(paste("Successfully applied values from", bond_file), type = "message", duration = 5)
            
        }, error = function(e) {
            myShowNotification(paste("Error applying values:", e$message), type = "error", duration = 10)
        })
        
    })

    # Helper function to check a bond list for any *active* bonds, this will be used in plotting
    hasActiveBonds <- function(bond_list) {
        # Check if it's null or has no items
        if (is.null(bond_list) || length(bond_list) == 0) {
            return(FALSE)
        }
        
        # Check if any value in the list is not "None"
        # This will return TRUE if it finds even one active bond
        any(sapply(bond_list, function(val) {
            !is.null(val) && val != "None"
        }))
    }
        

        modelCrashed <- reactiveVal(FALSE)
        firstRun <- reactiveVal(TRUE)
        copiedEPCs <- reactiveVal(list())
        copiedSoil <- reactiveVal(FALSE)
        lastRunBonds <- reactiveVal(list())
        bondsForPrevPlot <- reactiveVal(list())
        bondsForCurrentPlot <- reactiveVal(list())


        #### MODEL RUN ####
    observeEvent(list(input$runModel, input$runMusoExtra), {
        req(input$selected_epc)
        #epc <- input$selected_epc
        original_epc_values <- list()
        on.exit({
            if (length(original_epc_values) > 0) {
                #myShowNotification("Restoring EPC file values...", type = "message", duration = 5)
                
                # Use the correct `settings` object
                restore_settings <- isolate(rv$settings) 
                
                for (epc_file in names(original_epc_values)) {
                    try({
                        restore_settings$epcInput[["normal"]] <- epc_file
                        prettyChangeMuso(restore_settings, 
                                         original_epc_values[[epc_file]], 
                                         calibrationPar = params_epc, # Use the full params_epc
                                         fileToChange = "epc", 
                                         fixAlloc = FALSE)
                        myShowNotification(paste("Restored original values for:", epc_file), type = "message", duration = 5)
                    }, silent = TRUE) # Use try() so one failed restore doesn't stop others
                }
            }
        }) # End of on.exit block


        # starting waiter animation
        w$show()
        # updating current epc values MIGHT BE OBSOLETE since we update epcValues on slider change anyway
        #updateCurrentEPCValues
        
        # saving scroll position
        session$sendCustomMessage("save_scroll", list(id = "plotPanel"))
        modifiedEpcList <- 0
        runSpinup <- input$runSpinup

        current_settings <- rv$settings
        current_bonds <- rv$epc_bonds

        previous_bonds <- lastRunBonds()
        bondsChanged <- !isTRUE(all.equal(current_bonds, previous_bonds))

        if (bondsChanged && !firstRun()) {
            myShowNotification("Bond configuration changed, model will re-run.", type = "message", duration = 7)
        }
        #print("Writing parameter values to file before model run:...")
        #myShowNotification(paste0("Parameter slider values written into modified EPC files:"), type = "default", duration = 7)
        for (epc in rv$epc_files) {
            
            bond_file <- current_bonds[[epc]]
            if (!is.null(bond_file) && bond_file != "None") {
                # Define file paths
                epc_path <- file.path(workdir, epc)
                comp_epc_path <- file.path(workdir, bond_file)
                
                if (!file.exists(comp_epc_path)) {
                    myShowNotification(paste("Comparison file not found:", bond_file), type = "error", duration = 7)
                    next # Skip this EPC
                }

                tryCatch({
                    # 1. Get and save original values
                    original_values <- musoGetValues(epc_path, params_epc)
                    original_epc_values[[epc]] <- original_values
                    
                    # 2. Get comparison values
                    paramVal <- musoGetValues(comp_epc_path, params_epc)
                    
                    # 3. Overwrite main EPC file
                    current_settings$epcInput[["normal"]] <- epc
                    prettyChangeMuso(current_settings, paramVal, 
                            calibrationPar = params_epc, # Use params_epc for full overwrite
                            fileToChange = "epc", 
                            fixAlloc = FALSE)
                            
                    myShowNotification(paste(epc, "temporarily overwritten with", bond_file), type = "message", duration = 7)
                    modifiedEpcList <- modifiedEpcList + 1

                }, error = function(e) {
                    myShowNotification(paste("Error processing comparison for", epc, ":", e$message), type = "error", duration = 10)
                    # Remove from original_epc_values if we failed before writing
                    if (!is.null(original_epc_values[[epc]])) {
                        original_epc_values[[epc]] <- NULL
                    }
                })
                
            } # End of bond handling

            else {
            paramVal <- epcValues[[epc]]
            if (is.null(paramVal)) {
                paramVal <- InitialDefaults[[epc]]  # Fallback to initial defaults if needed
            }
            if (isTRUE(all.equal(paramVal, lastGoodValues$epc[[epc]]))) next # Skip writing if no changes
            
                # Check and create backup if first time writing to this EPC file
                current_copied_epcs <- copiedEPCs()
                if (is.null(current_copied_epcs[[epc]])) {
                    current_copied_epcs[[epc]] <- FALSE
                }
                if (!current_copied_epcs[[epc]]) {
                    timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
                    
                    orig_file <- file.path(workdir, epc)
                    base_name <- tools::file_path_sans_ext(epc)
                    ext <- tools::file_ext(epc)
                    backup_file <- file.path(workdir, paste0(base_name, "_", timestamp, "_ORIGINAL",".", ext))
                    file.copy(orig_file, backup_file)
                    current_copied_epcs[[epc]] <- TRUE
                    copiedEPCs(current_copied_epcs)
                    myShowNotification(paste0("Backup created for ", epc, " as ", basename(backup_file)), type = "message", duration = 7)
                }
            
            
            settings$epcInput[["normal"]] <- epc
                
            prettyChangeMuso(settings, paramVal, 
                    calibrationPar = parameters[, 2], 
                    fileToChange = "epc", 
                    fixAlloc = FALSE)
            #print(paste0("Written for: ", epc))
            myShowNotification(paste0(epc), " written", type = "message", duration = 7)
            modifiedEpcList <- modifiedEpcList + 1
            }
        }
        if (modifiedEpcList == 0 && !firstRun()) {
            myShowNotification("No changes in EPC values detected since last good run, no files were written", type = "warning", duration = 7)
        }
        
        soilChanged <- FALSE
        if (!is.null(soil_parameters())){
            #updateCurrentSoilValues()
            paramVal <- soilValues$values
            soilChanged <- !isTRUE(all.equal(paramVal, lastGoodValues$soil))

            #if (!identical(paramVal, lastGoodValues$soil)) {
            if(soilChanged){

                if (!copiedSoil()) {
                    timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
                    orig_file <- file.path(workdir, soil_file())
                    base_name <- tools::file_path_sans_ext(soil_file())
                    ext <- tools::file_ext(soil_file())
                    backup_file <- file.path(workdir, paste0(base_name, "_", timestamp, "_ORIGINAL",".", ext))
                    file.copy(orig_file, backup_file)
                    copiedSoil(TRUE)
                    myShowNotification(paste0("Backup created for ", soil_file(), " as ", basename(backup_file)), type = "message", duration = 7)
                }


                req(soil_file(), soil_parameters())
                paramValChanged <- format(paramVal, scientific = FALSE, trim = TRUE)
                prettyChangeMuso(settings, 
                                paramValChanged, 
                                calibrationPar = soil_parameters()[,2],
                                fileToChange = "soil", 
                                fixAlloc = FALSE)
                myShowNotification(paste0(soil_file(), " written"), type = "message", duration = 7)
            }
            else if (!firstRun()){
                myShowNotification("No changes in SOIL parameters detected since last good run, soil file wasn't written", type = "warning", duration = 8)
            }
        }

            if (!firstRun() && (modifiedEpcList == 0 && (is.null(soil_parameters()) || !soilChanged) && !bondsChanged)) {
                myShowNotification("No changes in EPC and/or SOIL parameters or comparison bonds detected since last good run. Not running the model", 
                                type = "message", duration = 9)
                w$hide()
                return()  
            }

        if(runSpinup){
            showNotification("Running the model with SPINUP run...", type = "message", duration = 5)
        }
        else{
            showNotification("Running the model...", type = "message", duration = 5)
        }

        #result <- calibMuso(settings = settings, calibrationPar = parameters[,2], parameters = paramVal, silent = TRUE)
            model_future <- future({
                calibMuso(settings = settings, silent = TRUE, skipSpinup = !runSpinup, doBackup = FALSE)
            })

            result <- tryCatch({
                value(model_future)
                }, error = function(e) {
                    modelCrashed(TRUE)
                    w$hide()
                # If there's an error (model crash), trigger a non-intrusive toast confirmation
                if(isTRUE(exportSettings$auto_reset)){
                    resetToLastGoodValues()
                    #showNotification(paste("Model error:", e$message, "\nResetting to last good values..."), type = "error")
                }
                else {
                    confirmSweetAlert(
                        session = session,
                        inputId = "resetConfirm",
                        title = "Model Crash!",
                        text = "The model crashed. Would you like to reset parameters to the last good values?",
                        type = "warning",
                        btn_labels = c("No", "Yes"),
                        closeOnClickOutside = TRUE,
                        timer = 0,         # No auto-dismiss
                        toast = TRUE,      # Makes it a non-blocking toast-style popup
                        position = "top-right"
                    )
                }
                    return(NULL)
                })

        if (length(result) == 0) {
        #if(is.na(result) || is.null(result) || nrow(result) == 0) {
            myShowNotification("Model did not return results! The parameters chosen are likely causing instability in the model!", type = "error", duration = 10)
             if(isTRUE(exportSettings$auto_reset)) myShowNotification("Resetting to last good values...", type = "message", duration = 8)
        } else {
        modelCrashed(FALSE)
        if(!firstRun()) {
            outputList$prev <- isolate(outputData())
            #prevMetricsData(NULL)
        }
        #print("Model ran successfully")
        #showNotification("Model ran successfully", type = "message")
        updatePrevGoodValues()
        updateLastGoodValues()
        bondsForPrevPlot(isolate(bondsForCurrentPlot()))
        bondsForCurrentPlot(isolate(rv$epc_bonds))
        lastRunBonds(isolate(rv$epc_bonds))

        dfs_orig <- as.data.frame(result, check.names = FALSE)  # 'result' is the simulation output matrix
        # Detect the VWC columns from the original output:
        #vwc_cols <- grep("^VWC\\[", names(dfs_orig), value = TRUE)
        #print("Detected VWC columns:")
        #print(vwc_cols)

        if (length(newVars$defs) > 0) {
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
                    current_layers <- layers[base_indices + 1]  
                    
                    # conditional calculation
                    new_val <- apply(dfs_orig[, base_cols, drop = FALSE], 1, function(r) {
                        vals <- as.numeric(r)
                        # Fallback for variables defined before this update
                        calc_type <- ifelse(is.null(def$calc_type), "interpolate", def$calc_type) 
                        
                        if (calc_type == "average") {
                            depth_weighted_average(vals, def$min_depth, def$max_depth, current_layers)
                        } else { # Default to interpolation
                            midpoint_trend_swc(vals, def$max_depth, current_layers)
                        }
                    })

                    dfs_orig[[var_name]] <- new_val
                }
            result <- as.matrix(dfs_orig)
        }
        outputList$nextVal <- result

        #logging
        current_logs <- runLogs()
        run_number <- length(current_logs) + 1
        
        epc_vals <- isolate({
            lapply(rv$epc_files, function(epc) {
            vals <- epcValues[[epc]]
            if (is.null(vals) || length(vals) != nrow(parameters)) {
                vals <- parameters[, 3]
            }
            setNames(vals, parameters$ABREVIATION)
            })
        })
        names(epc_vals) <- rv$epc_files
        
        soil_vals <- isolate({
            if (!is.null(soil_parameters()) && !is.null(soilValues$values)) {
            setNames(soilValues$values, soil_parameters()$ABREVIATION)
            } else {
            NULL
            }
        })
        
        year_range <- isolate({
            if (input$singleYear) {
            as.character(input$yearRange)
            } else {
            paste(input$yearRange[1], input$yearRange[2], sep = " - ")
            }
        })
        
        new_log <- list(
            run_number = run_number,
            timestamp = Sys.time(),
            epc_values = epc_vals,
            soil_values = soil_vals,
            metrics = NULL,
            year_range = year_range
        )
        
        current_logs[[run_number]] <- new_log
        runLogs(current_logs)
        
        run_choices <- sapply(current_logs, function(log) {
            sprintf("Run %d - %s", log$run_number, format(log$timestamp, "%Y-%m-%d %H:%M:%S"))
        })
        
        # Store current selections and update with new run
        selected_runs <- isolate(input$selected_runs)
        selected_params <- isolate(input$selected_params)
        selected_param_names <- isolate(input$selected_param_names)
        
        selectedRunsStore(selected_runs)
        selectedParamsStore(selected_params)
        selectedParamNamesStore(selected_param_names)
        
        new_selected_runs <- c(selected_runs, run_choices[run_number])
        updatePickerInput(session, "selected_runs",
                            choices = run_choices,
                            selected = new_selected_runs)
        selectedRunsStore(new_selected_runs) # Update store with new run included

        w$hide()
        if (firstRun()) firstRun(FALSE) #after successful model run, we set the actual first model run to false

    }
    })




    selectedRunsStore <- reactiveVal(NULL)
    selectedParamsStore <- reactiveVal(NULL)
    selectedParamNamesStore <- reactiveVal(NULL)

    observe({
        shinyWidgets::updateSwitchInput(session, "show_changes_only",
                            disabled = !input$show_comparisons)
        if (!input$show_comparisons && input$show_changes_only) {
            shinyWidgets::updateSwitchInput(session, "show_changes_only", value = FALSE)
        }
    })


    "%||%" <- function(x, y) if (is.null(x)) y else x  # complying that this function definition is necessary for the well-being of the app
    # lOGGING TABLE
    # Render EPC parameter table
    observe({
        logs <- runLogs()
        if (length(logs) == 0) return()
        
        epc_files <- names(logs[[1]]$epc_values)
        soil_name <- soil_file()
        file_choices <- c(epc_files, if (!is.null(logs[[1]]$soil_values)) soil_name else NULL)
        updatePickerInput(session, "selected_params",
                            choices = file_choices,
                            selected = if (is.null(selectedParamsStore())) file_choices else selectedParamsStore())
        
        epc_param_names <- names(logs[[1]]$epc_values[[1]])
        soil_param_names <- if (!is.null(logs[[1]]$soil_values)) names(logs[[1]]$soil_values) else character(0)
        param_choices <- unique(c(epc_param_names, soil_param_names))
        updatePickerInput(session, "selected_param_names",
                            choices = param_choices,
                            selected = if (is.null(selectedParamNamesStore())) param_choices[1:4] else selectedParamNamesStore())
    })

    observeEvent(input$yearRange, {
    
        selected_runs <- isolate(input$selected_runs)
        selected_params <- isolate(input$selected_params)
        selected_param_names <- isolate(input$selected_param_names)
        
        
        selectedRunsStore(selected_runs)
        selectedParamsStore(selected_params)
        selectedParamNamesStore(selected_param_names)
        
        
        updatePickerInput(
            session,
            inputId = "selected_runs",
            selected = selected_runs
        )
    })


    output$apply_run_ui <- renderUI({
        logs <- runLogs()
        if (length(logs) == 0) {
            return(tags$select(
            id = "apply_run",
            style = "width: 50px; height: 30px; padding: 2px; font-size: 14px; border-radius: 4px;",
            tags$option(value = "", "N/A")
            ))
        }
        
        run_numbers <- sapply(logs, function(log) log$run_number)
        selected_run <- if (length(run_numbers) > 0) run_numbers[length(run_numbers)] else NULL
        
        tags$select(
            id = "apply_run",
            style = "width: 50px; height: 30px; padding: 2px; font-size: 14px; border-radius: 4px;",
            lapply(run_numbers, function(num) {
            tags$option(
                value = num,
                selected = if (!is.null(selected_run) && num == selected_run) "selected" else NULL,
                num
            )
            })
        )
    })

    observeEvent(input$applyRunParams, {
        req(input$apply_run)
        logs <- runLogs()
        if (length(logs) == 0 || input$apply_run == "") {
            myShowNotification("No valid run selected to apply.", type = "warning", duration = 5)
            return()
        }
        
        run_idx <- as.numeric(gsub("Run (\\d+) - .*", "\\1", input$apply_run))
        run_data <- logs[[run_idx]]
        if (is.null(run_data)) {
            myShowNotification("Selected run data not found.", type = "error", duration = 5)
            return()
        }
        
        # Apply EPC parameters
        epc_vals <- run_data$epc_values
        req(epc_vals)
        
        selected_epc <- input$selected_epc
        req(selected_epc)  # Ensure an EPC is selected
        
        param_vals <- epc_vals[[selected_epc]]
        if (is.null(param_vals) || length(param_vals) != nrow(parameters)) {
            myShowNotification(paste0("Invalid parameter values for ", selected_epc, "."), type = "warning", duration = 5)
            return()
        }
        
        # Update only the selected EPC
        epcValues[[selected_epc]] <- as.numeric(param_vals)  # Ensure numeric type
        
        # Update sliders for the selected EPC
        for (i in seq_len(nrow(parameters))) {
            param_name <- parameters$ABREVIATION[i]
            val <- param_vals[[param_name]]
            if (is.null(val) || is.na(val)) {
            myShowNotification(paste0("No value for parameter ", param_name, " in ", selected_epc), type = "warning", duration = 5)
            next
            }
            if (is.na(parameters$group[i])) {
            updateSliderInput(session, paste0("param_", i), value = val)
            } else {
            updateSliderInput(session, paste0("dep_", parameters$INDEX[i]), value = val)
            }
        }
        myShowNotification(paste0("Parameters applied for ", selected_epc, "."), type = "message", duration = 10)
        
        # Apply Soil parameters
        soil_vals <- run_data$soil_values
        if (!is.null(soil_vals) && !is.null(soil_parameters())) {
            if (length(soil_vals) != nrow(soil_parameters())) {
            myShowNotification("Soil parameter mismatch. Not applied.", type = "warning", duration = 5)
            return()
            }
            soilValues$values <- as.numeric(soil_vals)  # Ensure numeric type
            
            lapply(1:nrow(soil_parameters()), function(i) {
            param_name <- soil_parameters()$ABREVIATION[i]
            val <- soil_vals[[param_name]]
            if (!is.null(val) && !is.na(val)) {
                updateSliderInput(session, paste0("soil_param_", i), value = val)
            } else {
                myShowNotification(paste0("No value for soil parameter ", param_name), type = "warning", duration = 5)
            }
            })
            myShowNotification("Soil parameters applied from selected run if applicable", type = "message", duration = 10)
        }
    })

    format_table <- function(row_names, col_names, values) {
        if (length(values) == 0 || length(col_names) == 0) return("No data to display")
        
        # Compute column widths: max of header length or longest *visible* formatted value
        col_widths <- sapply(seq_along(col_names), function(j) {
            header_len <- nchar(col_names[j])
            value_lens <- sapply(values, function(row) {
            val <- row[[j]]
            if (is.null(val) || is.na(val)) 0 else nchar(strip_html(val))
            })
            max(header_len, max(value_lens, na.rm = TRUE), na.rm = TRUE)
        })
        
        # Pad the first column (row names, e.g., "Metric", "RMSE")
        row_name_width <- max(nchar(row_names))
        row_name_format <- sprintf("%%-%ds", row_name_width)
        
        # Format the header
        header_parts <- mapply(function(name, width) sprintf("%-*s", width, name), col_names, col_widths)
        header <- sprintf("%s | %s", sprintf(row_name_format, " "), paste(header_parts, collapse = " | "))
        
        # Separator
        separator <- paste(rep("-", nchar(header)), collapse = "")
        
        # Format each row
        rows <- mapply(function(row_name, row_vals) {
            formatted_vals <- mapply(function(val, width) {
            if (is.null(val) || is.na(val)) sprintf("%-*s", width, "") else sprintf("%-*s", width, strip_html(val))
            }, row_vals, col_widths)
            # Reattach HTML tags to the padded string
            final_vals <- mapply(function(val, formatted) {
            if (is.null(val) || is.na(val)) formatted else gsub(strip_html(val), formatted, val, fixed = TRUE)
            }, row_vals, formatted_vals)
            sprintf("%s | %s", sprintf(row_name_format, row_name), paste(final_vals, collapse = " | "))
        }, row_names, values)
        
        paste(header, "<br>", separator, "<br>", paste(rows, collapse = "<br>"), sep = "")
    }

    strip_html <- function(text) {
        if (is.null(text) || is.na(text)) return("")
        # Remove HTML tags, e.g., <span style='color:red'>...</span>
        gsub("<[^>]+>", "", text)
    }

    # Render HTML summary
    output$run_summary <- renderUI({
        req(input$selected_runs, input$selected_params)
        logs <- runLogs()
        if (length(logs) == 0) return(HTML("No runs logged yet."))
        
        run_indices <- as.numeric(gsub("Run (\\d+) - .*", "\\1", input$selected_runs))
        selected_params <- input$selected_params
        selected_param_names <- input$selected_param_names
        latest_run <- length(logs)
        
        run_choices <- sapply(logs, function(log) {
            sprintf("Run %d - %s", log$run_number, format(log$timestamp, "%Y-%m-%d %H:%M:%S"))
        })
        
        live_year_range <- if (input$singleYear) {
            as.character(input$yearRange)
        } else {
            paste(input$yearRange[1], input$yearRange[2], sep = " - ")
        }
        
        color_value <- function(value, prev_value, metric) {
            if (!input$show_comparisons || is.null(prev_value) || is.na(prev_value) || is.na(value)) {
                return(ifelse(is.na(value), "NA", sprintf("%.3f", value)))
            }
            val_rounded <- round(value, 3)
            prev_rounded <- round(prev_value, 3)
            delta <- val_rounded - prev_rounded
            
            if (abs(delta) < 0.001) {
                return(sprintf("%.3f", val_rounded))
            }
            
            is_good_change <- switch(metric,
                "RMSE" = delta < 0,              # Lower RMSE is better
                "Bias" = abs(val_rounded) < abs(prev_rounded),  # Smaller absolute bias is better
                "R2"   = delta > 0,              # Higher R2 is better
                "NSE"  = delta > 0,              # Higher NSE is better (closer to 1)
                FALSE
            )
            
            if (is_good_change) {
                sprintf("<span style='color:green'>%.3f %+.3f</span>", val_rounded, delta)
            } else {
                sprintf("<span style='color:red'>%.3f %+.3f</span>", val_rounded, delta)
            }
        }
        
        strip_html <- function(text) {
            if (is.null(text) || is.na(text)) return("")
            gsub("<[^>]+>", "", text)
        }
        
        format_table <- function(row_names, col_names, values) {
            if (length(values) == 0 || length(col_names) == 0) return("No data to display")
            
            col_widths <- sapply(seq_along(col_names), function(j) {
                header_len <- nchar(col_names[j])
                value_lens <- sapply(values, function(row) {
                    val <- row[[j]]
                    if (is.null(val) || is.na(val)) 0 else nchar(strip_html(val))
                })
                max(header_len, max(value_lens, na.rm = TRUE), na.rm = TRUE)
            })
            
            row_name_width <- max(nchar(row_names))
            row_name_format <- sprintf("%%-%ds", row_name_width)
            
            header_parts <- mapply(function(name, width) sprintf("%-*s", width, name), col_names, col_widths)
            header <- sprintf("%s | %s", sprintf(row_name_format, " "), paste(header_parts, collapse = " | "))
            
            separator <- paste(rep("-", nchar(header)), collapse = "")
            
            rows <- mapply(function(row_name, row_vals) {
                formatted_vals <- mapply(function(val, width) {
                    if (is.null(val) || is.na(val)) sprintf("%-*s", width, "") else sprintf("%-*s", width, strip_html(val))
                }, row_vals, col_widths)
                final_vals <- mapply(function(val, formatted) {
                    if (is.null(val) || is.na(val)) formatted else gsub(strip_html(val), formatted, val, fixed = TRUE)
                }, row_vals, formatted_vals)
                sprintf("%s | %s", sprintf(row_name_format, row_name), paste(final_vals, collapse = " | "))
            }, row_names, values)
            
            paste(header, "<br>", separator, "<br>", paste(rows, collapse = "<br>"), sep = "")
        }
        
        summary_html <- lapply(run_indices, function(run_idx) {
            run_data <- logs[[run_idx]]
            if (is.null(run_data)) return(sprintf("<p>Run %d: Data not found</p>", run_idx))
            
            year_range <- if (run_idx == latest_run) live_year_range else run_data$year_range
            meta <- sprintf("<b>Run %d - %s (Year Range: %s)</b>",
                            run_data$run_number,
                            format(run_data$timestamp, "%Y-%m-%d %H:%M:%S"),
                            year_range)
            
            compare_input_id <- paste0("compare_run_", run_idx)
            compare_choices <- run_choices[-run_idx]
            default_compare <- if (run_idx > 1) run_choices[run_idx - 1] else (if (length(compare_choices) > 0) compare_choices[1] else NULL)
            current_selection <- input[[compare_input_id]]
            
            compare_selector <- if (length(compare_choices) > 0) {
                sprintf('<div style="float:right;"><span style="margin-right: 10px;font-weight: bold;">This run will be compared with</span><select id="%s" onchange="Shiny.setInputValue(\'%s\', this.value)">%s</select></div>',
                        compare_input_id, compare_input_id,
                        paste0(c(sprintf('<option value="">None</option>'),
                                sapply(compare_choices, function(ch) {
                                    current_selection <- input[[compare_input_id]] %||% ""
                                    default_compare <- if (run_idx > 1 && run_idx <= length(run_choices)) run_choices[run_idx - 1] else if (length(compare_choices) > 0) compare_choices[1] else ""
                                    is_selected <- if (current_selection == "") {
                                        if (is.null(input[[compare_input_id]])) (ch == default_compare) else (ch == "")
                                    } else {
                                        (ch == current_selection)
                                    }
                                    selected <- if (is_selected) ' selected' else ''
                                    sprintf('<option value="%s"%s>%s</option>', ch, selected, ch)
                                })), collapse = ""))
            } else {
                ""
            }
            
            compare_run <- if (!is.null(current_selection) && current_selection %in% c("", compare_choices)) current_selection else default_compare
            prev_idx <- if (!is.null(compare_run) && compare_run != "") which(run_choices == compare_run) else NULL
            prev_data <- if (!is.null(prev_idx)) logs[[prev_idx]] else NULL
            
            # EPC Parameters
            epc_vals <- run_data$epc_values
            selected_epcs <- intersect(names(epc_vals), selected_params)
            if (length(selected_epcs) == 0) {
                epc_text <- "EPC Parameters: None selected"
            } else {
                epc_param_names <- intersect(names(epc_vals[[1]]), selected_param_names)
                if (length(epc_param_names) == 0) {
                    epc_text <- "EPC Parameters: No parameters selected"
                } else {
                    if (input$show_comparisons && input$show_changes_only && !is.null(prev_data)) {
                        changed_epcs <- sapply(selected_epcs, function(epc) {
                            current <- epc_vals[[epc]]
                            prev <- prev_data$epc_values[[epc]]
                            any(sapply(epc_param_names, function(p) {
                                abs(round(current[[p]], 3) - round(prev[[p]], 3)) >= 0.001
                            }))
                        })
                        selected_epcs <- selected_epcs[changed_epcs]
                        if (length(selected_epcs) == 0) {
                            epc_text <- "EPC Parameters: No changes detected"
                        } else {
                            formatted_vals <- lapply(epc_param_names, function(param) {
                                sapply(selected_epcs, function(epc) {
                                    val <- epc_vals[[epc]][[param]]
                                    prev_val <- prev_data$epc_values[[epc]][[param]]
                                    delta <- round(val, 3) - round(prev_val, 3)
                                    if (abs(delta) >= 0.001) color_value(val, prev_val, "EPC") else NULL
                                }, simplify = FALSE)
                            })
                            
                            has_significant_change <- sapply(seq_along(epc_param_names), function(i) {
                                any(sapply(formatted_vals[[i]], function(val) !is.null(val)))
                            })
                            epc_param_names <- epc_param_names[has_significant_change]
                            formatted_vals <- lapply(formatted_vals[has_significant_change], function(row) {
                                row[sapply(row, function(x) !is.null(x))]
                            })
                            selected_epcs <- selected_epcs[sapply(formatted_vals[[1]], function(x) !is.null(x))]
                            
                            if (length(epc_param_names) == 0 || length(selected_epcs) == 0) {
                                epc_text <- "EPC Parameters: No changes detected"
                            } else {
                                epc_text <- paste("EPC Parameters:<br>", format_table(epc_param_names, selected_epcs, formatted_vals), sep = "")
                            }
                        }
                    } else {
                        formatted_vals <- lapply(epc_param_names, function(param) {
                            sapply(selected_epcs, function(epc) {
                                val <- epc_vals[[epc]][[param]]
                                prev_val <- if (!is.null(prev_data)) prev_data$epc_values[[epc]][[param]] else NULL
                                color_value(val, prev_val, "EPC")
                            }, simplify = FALSE)
                        })
                        epc_text <- paste("EPC Parameters:<br>", format_table(epc_param_names, selected_epcs, formatted_vals), sep = "")
                    }
                }
            }
            
            # Soil Parameters
            soil_vals <- run_data$soil_values
            soil_name <- soil_file()
            has_soil <- soil_name %in% selected_params && !is.null(soil_vals)
            if (!has_soil) {
                soil_text <- "Soil Parameters: None selected or available"
            } else {
                soil_param_names <- intersect(names(soil_vals), selected_param_names)
                if (length(soil_param_names) == 0) {
                    soil_text <- "Soil Parameters: No parameters selected"
                } else {
                    if (input$show_comparisons && input$show_changes_only && !is.null(prev_data)) {
                        formatted_vals <- lapply(soil_param_names, function(param) {
                            val <- soil_vals[[param]]
                            prev_val <- prev_data$soil_values[[param]]
                            delta <- round(val, 3) - round(prev_val, 3)
                            if (abs(delta) >= 0.001) list(color_value(val, prev_val, soil_name)) else list(NULL)
                        })
                        
                        has_significant_change <- sapply(formatted_vals, function(val) !is.null(val[[1]]))
                        soil_param_names <- soil_param_names[has_significant_change]
                        formatted_vals <- formatted_vals[has_significant_change]
                        
                        if (length(soil_param_names) == 0) {
                            soil_text <- "Soil Parameters: No changes detected"
                        } else {
                            soil_text <- paste("Soil Parameters:<br>", format_table(soil_param_names, soil_name, formatted_vals), sep = "")
                        }
                    } else {
                        formatted_vals <- lapply(soil_param_names, function(param) {
                            val <- soil_vals[[param]]
                            prev_val <- if (!is.null(prev_data)) prev_data$soil_values[[param]] else NULL
                            list(color_value(val, prev_val, soil_name))
                        })
                        soil_text <- paste("Soil Parameters:<br>", format_table(soil_param_names, soil_name, formatted_vals), sep = "")
                    }
                }
            }
            
            # Metrics
            metrics <- if (run_idx == latest_run) metricsData() else run_data$metrics
            if (is.null(metrics) || nrow(metrics) == 0) {
                metrics_text <- "Metrics: Not available"
            } else {
                var_names <- sprintf("%s (%s)", metrics$Measurement, metrics$OutputVariable)
                metric_types <- c("RMSE", "Bias", "R2", "NSE")
                prev_metrics <- if (!is.null(prev_data)) {
                    if (prev_idx == latest_run) metricsData() else prev_data$metrics
                } else NULL
                
                if (!is.null(prev_metrics) && nrow(prev_metrics) > 0) {
                    prev_map <- list(
                        RMSE = setNames(prev_metrics$RMSE, sprintf("%s (%s)", prev_metrics$Measurement, prev_metrics$OutputVariable)),
                        Bias = setNames(prev_metrics$BIAS, sprintf("%s (%s)", prev_metrics$Measurement, prev_metrics$OutputVariable)),
                        R2 = setNames(prev_metrics$Correlation, sprintf("%s (%s)", prev_metrics$Measurement, prev_metrics$OutputVariable)),
                        NSE = setNames(prev_metrics$NSE, sprintf("%s (%s)", prev_metrics$Measurement, prev_metrics$OutputVariable))
                    )
                    
                    if (input$show_comparisons && input$show_changes_only) {
                        formatted_vals_all <- lapply(metric_types, function(metric) {
                            values <- switch(metric, 
                                            "RMSE" = metrics$RMSE, 
                                            "Bias" = metrics$BIAS, 
                                            "R2" = metrics$Correlation,
                                            "NSE" = metrics$NSE)
                            prev_values <- sapply(var_names, function(vn) prev_map[[metric]][vn])
                            mapply(function(v, pv) {
                                if (is.na(v) || is.na(pv)) return(NULL)
                                delta <- round(v, 3) - round(pv, 3)
                                if (abs(delta) >= 0.001) color_value(v, pv, metric) else NULL
                            }, values, prev_values, SIMPLIFY = FALSE)
                        })
                        
                        has_significant_change <- sapply(seq_along(var_names), function(i) {
                            any(sapply(formatted_vals_all, function(metric_vals) !is.null(metric_vals[[i]])))
                        })
                        
                        var_names <- var_names[has_significant_change]
                        if (length(var_names) == 0) {
                            metrics_text <- "Metrics: No changes detected"
                        } else {
                            formatted_vals <- lapply(metric_types, function(metric) {
                                values <- switch(metric, 
                                                "RMSE" = metrics$RMSE, 
                                                "Bias" = metrics$BIAS, 
                                                "R2" = metrics$Correlation,
                                                "NSE" = metrics$NSE)
                                prev_values <- sapply(var_names, function(vn) prev_map[[metric]][vn])
                                mapply(function(v, pv) {
                                    if (is.na(v) || is.na(pv)) return(NULL)
                                    delta <- round(v, 3) - round(pv, 3)
                                    if (abs(delta) >= 0.001) color_value(v, pv, metric) else NULL
                                }, values[has_significant_change], prev_values, SIMPLIFY = FALSE)
                            })
                            
                            metrics_text <- paste("Metrics:<br>", format_table(metric_types, var_names, formatted_vals), sep = "")
                        }
                    } else {
                        formatted_vals <- lapply(metric_types, function(metric) {
                            values <- switch(metric, 
                                            "RMSE" = metrics$RMSE, 
                                            "Bias" = metrics$BIAS, 
                                            "R2" = metrics$Correlation,
                                            "NSE" = metrics$NSE)
                            prev_values <- sapply(var_names, function(vn) prev_map[[metric]][vn])
                            mapply(function(v, pv) color_value(v, pv, metric), values, prev_values, SIMPLIFY = FALSE)
                        })
                        metrics_text <- paste("Metrics:<br>", format_table(metric_types, var_names, formatted_vals), sep = "")
                    }
                } else {
                    formatted_vals <- lapply(metric_types, function(metric) {
                        values <- switch(metric, 
                                        "RMSE" = metrics$RMSE, 
                                        "Bias" = metrics$BIAS, 
                                        "R2" = metrics$Correlation,
                                        "NSE" = metrics$NSE)
                        lapply(values, function(v) sprintf("%.3f", v))
                    })
                    metrics_text <- paste("Metrics:<br>", format_table(metric_types, var_names, formatted_vals), sep = "")
                }
            }
            
            paste("<pre>", meta, compare_selector, "<br><br>", epc_text, "<br><br>", soil_text, "<br><br>", metrics_text, "</pre>", sep = "")
        })
        HTML(paste(summary_html, collapse = "<hr>"))
    })

        # Render metrics table
    observeEvent(metricsData(), {
        current_logs <- runLogs()
        if (length(current_logs) == 0) return()
        
        run_number <- length(current_logs)
        metrics <- metricsData()

        year_range <- if (isTRUE(input$singleYear)) {
            validate(
            need(is.finite(input$yearRange), "Year not available yet")
            )
            as.character(input$yearRange)
        } else {
            validate(
            need(length(input$yearRange) == 2 &&
                is.finite(input$yearRange[1]) &&
                is.finite(input$yearRange[2]),
                "Year range not available yet")
            )
            paste(input$yearRange[1], input$yearRange[2], sep = " - ")
        }
        
        if (!is.null(metrics) && nrow(metrics) > 0) {
            current_logs[[run_number]]$metrics <- metrics
            current_logs[[run_number]]$year_range <- year_range  # Update year_range here
            runLogs(current_logs)
            #print("Updated runLogs with metrics and year_range for run:", run_number)
        } #else {
            #print("No valid metrics to store")
        #}
    
        #print("Finished observeEvent for metricsData")
    })



        # restoring scrollbar position
        observe({
            if (!is.null(input$plotPanel_scroll)) {
            session$sendCustomMessage("restore_scroll", list(id = "plotPanel", scroll = input$plotPanel_scroll))
            }
        })


           resetToLastGoodValues <- function() {
                if(length(lastGoodValues$epc) > 0) {
                    for(epc in names(lastGoodValues$epc)) {
                    # Restore reactive storage for this epc file
                    #browser()
                    epcValues[[epc]] <<- lastGoodValues$epc[[epc]]
                    }
                    # updating the visible sliders
                    selected_epc <- input$selected_epc
                        epc_vals <- lastGoodValues$epc[[selected_epc]]
                        for(i in seq_len(nrow(parameters))) {
                            if (is.na(parameters$group[i])) {
                                updateSliderInput(session,
                                                inputId = paste0("param_", i),
                                                value = epc_vals[i])
                            } else {
                                updateSliderInput(session,
                                                inputId = paste0("dep_", parameters$INDEX[i]),
                                                value = epc_vals[i])
                            }
                       }
                    
                }
                else {
                    showNotification("No last good values found for EPC files.", type = "warning")
                }
                # restore soil values
                if (length(lastGoodValues$soil) > 0 && !is.null(soil_parameters())) {
                  soilValues$values <<- lastGoodValues$soil
        
                    for (i in seq_along(lastGoodValues$soil)) {
                    updateSliderInput(session,
                                        inputId = paste0("soil_param_", i),
                                        value = lastGoodValues$soil[i])
                    }
                }
                else if (!is.null(soil_parameters())) {
                    showNotification("No last good values found for soil file.", type = "warning")
                }
           }

        # resetting all the epc and soil sliders to their last good values
        observeEvent(input$resetConfirm, {
            if (isTRUE(input$resetConfirm)) {
                resetToLastGoodValues()
                
                if(length(lastGoodValues$epc) > 0) {
                    myShowNotification("All parameter sets restored to the last good values.", type = "message", duration = 5)
                }
            } else {
                myShowNotification("Parameters remain unchanged.", type = "message", duration = 5)
            }
        })
        


    simTableDat <- reactive({
        req(outputList$nextVal)
        result <- outputList$nextVal
        
        # Convert result to a data frame without altering column names
        dfs <- as.data.frame(result, check.names = FALSE)
        
        # Convert rownames (which are in "dd.mm.YYYY" format) to Date objects
        sim_dates <- as.Date(rownames(result), format = "%d.%m.%Y")
        
      
        
        # Create a new data frame with a single Date column in front (Date objects display as "YYYY-mm-dd")
        sim_df <- data.frame(Date = sim_dates, dfs, stringsAsFactors = FALSE, check.names = FALSE)
        sim_df
    })

    output$outputTable <- DT::renderDataTable({
        req(outputData())
        df <- outputData()

         cols_to_rename <- setdiff(names(df), "Date")
        df_renamed <- df
          
        numeric_cols <- sapply(df_renamed, is.numeric)
        df_renamed[numeric_cols] <- lapply(df_renamed[numeric_cols], round, digits = 5)

        #names(df_renamed)[names(df_renamed) %in% cols_to_rename] <-
         #   paste0(names(df_renamed)[names(df_renamed) %in% cols_to_rename], "_sim")
          
                # Replace NA values for display
                df_renamed[is.na(df_renamed)] <- "NA"
           DT::datatable(df_renamed,
                editable = FALSE,
                options = list(
                  pageLength = 50,
                  scrollX = TRUE,          # Enables horizontal scrolling (headers move with the data)
                  fixedHeader = TRUE,      # Optional: fixes header when scrolling vertically
                  lengthMenu = list(c(10, 25, 50, 100, 500, 1000),
                                    c("10", "25", "50", "100", "500", "1000")),
                  scrollY = "400px",
                  autoWidth = TRUE,
                  stateSave = TRUE
                ),
                rownames = FALSE)

    })

    output$exportDataSim <- downloadHandler(
        filename = function() {
            paste("tuneMusoExport_simData-", Sys.Date(), ".txt", sep = "")
        },
        content = function(file) {
            # Make a copy for export
            export_df <- outputData()
            
            # Create Year, Month, and Day columns from the Date column 
            # (so we have the same file format required for our measurement inputs)
            export_df$Year  <- format(export_df$Date, "%Y")
            export_df$Month <- format(export_df$Date, "%m")
            export_df$Day   <- format(export_df$Date, "%d")
            
        
            
            # Rearrange columns so that Year, Month, and Day come first
           
            other_cols <- setdiff(colnames(export_df), c("Date", "Year", "Month", "Day"))
            export_df <- export_df[, c("Year", "Month", "Day", other_cols)]
            
                # If the user has selected specific columns to export, subset accordingly
           
                if (!is.null(input$exportCols) && length(input$exportCols) > 0) {
                # Get only the columns that exist in export_df
                valid_cols <- intersect(input$exportCols, names(export_df))
                # Always include the date columns
                valid_cols <- unique(c("Year", "Month", "Day", valid_cols))
                export_df <- export_df[, valid_cols, drop = FALSE]
                }
            # If the user requested the "_sim" suffix, append it to non-date columns
            if (isTRUE(input$appendSimSuffix)) {
            # Here, "other_cols" are all columns except Year, Month, Day
            names(export_df)[names(export_df) %in% other_cols] <-
                paste0(names(export_df)[names(export_df) %in% other_cols], "_sim")
            }
            

            
            # Replace NA values with -9999 for export (so they don't appear as empty cells)
            export_df[is.na(export_df)] <- -9999
            
           
            fwrite(export_df, file, row.names = FALSE, sep = "\t")
        }
    )

        observe({
            req(simTableDat())
            df <- simTableDat()
           
            available_cols <- setdiff(names(df), "Date")
            
            updatePickerInput(session,
                                inputId = "exportCols",
                                choices = available_cols,
                                selected = NULL)  
        })

         ######## OUTPUT EDITING ############
     # This reactive value holds the main data from the model run + any persistent transformations.
    # It gets reset on every new model run.
    model_output_processed <- reactiveVal()

    # This reactive value holds only the one-time "snapshot" variables.
    # It PERSISTS across model runs.
    snapshot_storage <- reactiveVal(NULL)

    # This is the final, combined data view that the UI will use.
    # It reactively merges the main model data with the persistent snapshots.
    outputData <- reactive({
        req(model_output_processed())
        processed <- model_output_processed()
        snapshots <- snapshot_storage()

        # If there are snapshots, merge them with the main data by Date.
        if (!is.null(snapshots) && ncol(snapshots) > 1) {
            # all.x = TRUE ensures we keep all rows from the main model data.
            merge(processed, snapshots, by = "Date", all.x = TRUE)
        } else {
            processed
        }
    })

    # An observer that updates the column picker whenever the final outputData changes.
    observe({
        req(outputData())
        updatePickerInput(session, "exportCols", choices = colnames(outputData()))
    })

    # A reactiveValues list to track modifications for each column.
    outputTransformsTracker <- reactiveValues(modifications = list())
    outputTransforms <- reactiveValues(transforms = list())

    ## ---- Output Transformation Modal ----
    observeEvent(input$editOutputTransforms, {
        req(simTableDat())
        showModal(modalDialog(
            title = "Output Data Transformations",
            size = "l",
            easyClose = TRUE,
            footer = modalButton("Close"),
            tabsetPanel(
                # Tab for replacing values with NA
                tabPanel("Set Values to NA",
                    fluidRow(
                        column(4,
                            pickerInput("col_to_na_output", "Select column(s):",
                                        choices = setdiff(colnames(outputData()), "Date"),
                                        multiple = TRUE,
                                        options = list(`actions-box` = TRUE))
                        ),
                        column(4,
                            numericInput("na_lower_output", "Lower bound:", value = NA)
                        ),
                        column(4,
                            numericInput("na_upper_output", "Upper bound:", value = NA)
                        )
                    ),
                    fluidRow(
                        column(4,
                            checkboxInput("na_keep_transformation", "Keep transformation upon model run", value = TRUE)
                        ),
                        column(8,
                            div(style = "text-align: right;",
                                actionButton("apply_na_output", "Apply Transformation",
                                            style = "color: white; background-color: #007bff; border-color: #007bff;")
                            )
                        )
                    )
                ),
                # Tab for arithmetic operations
                tabPanel("Arithmetic Operation",
                    fluidRow(
                        column(4,
                            pickerInput("col_arith_output", "Select column(s):",
                                        choices = setdiff(colnames(outputData()), "Date"),
                                        multiple = TRUE,
                                        options = list(`actions-box` = TRUE))
                        ),
                        column(4,
                            selectInput("arith_op_output", "Operation",
                                        choices = c("Add", "Subtract", "Multiply", "Divide"))
                        ),
                        column(4,
                            numericInput("arith_val_output", "Value:", value = 0)
                        )
                    ),
                    fluidRow(
                        column(4,
                            checkboxInput("arith_keep_transformation", "Keep transformation upon model run", value = TRUE),
                            checkboxInput("interaction_newcol_arith", "Create as new variable", value = FALSE),
                            uiOutput("arithNewVarNames")
                        ),
                        column(8,
                            div(style = "text-align: right;",
                                actionButton("apply_arith_output", "Apply Transformation",
                                            style = "color: white; background-color: #007bff; border-color: #007bff;")
                            )
                        )
                    )
                ),
                # Tab for interactions between columns
                tabPanel("Column Interaction",
                    # Adding info button at top-right
                    div(
                        style = "position: absolute; top: 10px; right: 10px;",
                        tags$button(
                            id = "interaction_info_btn",
                            class = "btn btn-default action-button",
                            tags$i(class = "fa fa-info-circle"),
                            title = "Column Interaction Information"
                        )
                    ),
                    # Adding info overlay
                    div(
                        id = "interaction_info_overlay",
                        style = "display:none; position:absolute; top:44px; right:10px; width:98%; background:#f9f9f9; border:1px solid #ccc; padding:10px; z-index:1050;",
                        tags$p(div(HTML("
                            <p><strong>Column Interaction Information</strong></p>
                            <p>This panel allows you to perform operations between variables (referred to as columns).</p>
                            <ul>
                                <li><strong>Target column</strong>: Select a single column to apply the operation to.</li>
                                <li><strong>Interaction column(s)</strong>: Choose one or more columns to interact with the target column. The interaction columns won't be affected during the operations.</li>
                                <li><strong>Create as new variable</strong>: If checked, the result is stored as a new column (variable) with a custom name. <strong>In this case the target column also remains unchanged.</strong></li>
                            </ul>
                            
                        ")))
                    ),
                    fluidRow(
                        column(4,
                            selectInput("col1_output", "Target column:",
                                        choices = setdiff(colnames(outputData()), "Date"))
                        ),
                        column(4,
                            pickerInput("col2_output", "Interaction column(s):",
                                        choices = setdiff(colnames(outputData()), "Date"),
                                        multiple = TRUE,
                                        options = list(`actions-box` = TRUE))
                        ),
                        column(4,
                            selectInput("interaction_op_output", "Operation",
                                        choices = c("Multiply", "Add", "Subtract", "Divide"))
                        )
                    ),
                    fluidRow(
                        column(4,
                            checkboxInput("interaction_keep_transformation", "Keep transformation upon model run", value = TRUE),
                            checkboxInput("interaction_newcol", "Create as new variable", value = FALSE),
                            conditionalPanel(
                                condition = "input.interaction_newcol == true",
                                textInput("interaction_newcol_name", "New Variable Name:")
                            )
                        ),
                        column(8,
                            div(style = "text-align: right;",
                                actionButton("apply_interaction_output", "Apply Transformation",
                                            style = "color: white; background-color: #007bff; border-color: #007bff;")
                            )
                        )
                    )
                )
            )
        ))
    })

    observeEvent(input$interaction_info_btn, {
        shinyjs::toggle("interaction_info_overlay", anim = TRUE)
    })

    output$arithNewVarNames <- renderUI({
        req(input$interaction_newcol_arith, input$col_arith_output)
        if (isTRUE(input$interaction_newcol_arith) && length(input$col_arith_output) > 0) {
            # For each selected column, create a text input with an ID based on the column name
            lapply(input$col_arith_output, function(col) {
            textInput(inputId = paste0("newName_", col), label = paste("New name for", col, ":"), value = "")
            })
        }
    })



        ##  Observers for Each Transformation  
        # NA Transformation
   observeEvent(input$apply_na_output, {
    req(model_output_processed(), input$col_to_na_output)
    df <- model_output_processed()
    cols <- input$col_to_na_output
    lower_bound <- input$na_lower_output
    upper_bound <- input$na_upper_output

    na_transform <- function(data, col) {
        new_values <- data[[col]]
        if (!is.na(lower_bound) && !is.na(upper_bound)) {
            new_values[new_values >= lower_bound & new_values <= upper_bound] <- NA
        } else if (!is.na(lower_bound)) {
            new_values[new_values >= lower_bound] <- NA
        } else if (!is.na(upper_bound)) {
            new_values[new_values <= upper_bound] <- NA
        }
        return(new_values)
    }

    # Apply the transformation to all selected columns
    for (col in cols) {
        df[[col]] <- na_transform(df, col)
    }

    if (isTRUE(input$na_keep_transformation)) {
        for (col in cols) {
            local({
                currentCol <- col
                outputTransforms$transforms[[currentCol]] <- function(data) na_transform(data, currentCol)
                outputTransformsTracker$modifications[[currentCol]] <-
                    c(outputTransformsTracker$modifications[[currentCol]],
                    paste("Persistent NA transformation on", currentCol, "with lower =", lower_bound, "and upper =", upper_bound))
            })
        }
    } else {
        # For one-time NA transformation, we don't store a persistent function
        # The change is already applied to model_output_processed
        for (col in cols) {
            outputTransforms$transforms[[col]] <- NULL
            outputTransformsTracker$modifications[[col]] <-
            c(outputTransformsTracker$modifications[[col]],
                paste("One-time NA transformation on", col, "with lower =", lower_bound, "and upper =", upper_bound))
        }
    }

    model_output_processed(df)
    showNotification(paste("Updated", paste(cols, collapse = ", "), "with NA transformation"))
})

# Arithmetic Operation
observeEvent(input$apply_arith_output, {
    req(outputData(), input$col_arith_output, input$arith_op_output, input$arith_val_output)
    # Use outputData() here to ensure calculations are based on the complete, merged view
    df_full <- outputData()
    cols <- input$col_arith_output
    op <- input$arith_op_output
    val <- input$arith_val_output
    
    arith_transform <- function(data, col) {
        switch(op,
        "Add"      = data[[col]] + val,
        "Subtract" = data[[col]] - val,
        "Multiply" = data[[col]] * val,
        "Divide"   = {
            if (val == 0) {
                showNotification("Division by zero not allowed", type = "error")
                data[[col]]
            } else {
                data[[col]] / val
            }
        })
    }
    
    # Flag to track whether any new variables have been created for the picker update.
    new_vars_created <- FALSE

    if (isTRUE(input$interaction_newcol_arith)) {
        # CREATE NEW VARIABLE
        for (col in cols) {
            new_name <- input[[paste0("newName_", col)]]
            if (nzchar(new_name)) {
                new_vars_created <- TRUE
                
                if (isTRUE(input$arith_keep_transformation)) {
                    # PERSISTENT new var: store transform and apply to current data
                    outputTransforms$transforms[[new_name]] <- function(data) arith_transform(data, col)
                    df_processed <- model_output_processed()
                    df_processed[[new_name]] <- arith_transform(df_processed, col)
                    model_output_processed(df_processed)
                     outputTransformsTracker$modifications[[new_name]] <-
                        c(outputTransformsTracker$modifications[[new_name]],
                        paste(op, "operation persistent on new column", new_name, "with value", val))
                } else {
                    # SNAPSHOT new var: store in snapshot_storage
                    new_snapshot_col <- arith_transform(df_full, col)
                    new_snapshot_df <- data.frame(Date = df_full$Date, new_snapshot_col)
                    names(new_snapshot_df)[2] <- new_name

                    current_snapshots <- snapshot_storage()
                    if (is.null(current_snapshots)) {
                        snapshot_storage(new_snapshot_df)
                    } else {
                        if(new_name %in% names(current_snapshots)) {
                            showNotification(paste("Snapshot", new_name, "exists. Overwriting."), type = "warning")
                            current_snapshots[[new_name]] <- NULL
                        }
                        snapshot_storage(merge(current_snapshots, new_snapshot_df, by = "Date", all = TRUE))
                    }
                    outputTransformsTracker$modifications[[new_name]] <-
                        c(outputTransformsTracker$modifications[[new_name]],
                        paste(op, "operation one-time on new column", new_name, "with value", val))
                }
                
                # Update the dailyOutputTable with the new variable.
                new_row <- data.frame(
                    index = max(rv$settings$dailyOutputTable$index, 0) + 1,
                    code  = NA,  # Indicates a custom variable
                    name  = new_name,
                    stringsAsFactors = FALSE
                )
                rv$settings$dailyOutputTable <- rbind(rv$settings$dailyOutputTable, new_row)

            } else {
                # If no new name is provided, skip transformation for that column.
                showNotification(paste("No new name provided for", col, "- original column remains unchanged."), 
                                type = "warning")
            }
        }
    } else {
        # --- MODIFY EXISTING VARIABLE ---
        df_processed <- model_output_processed()
        for (col in cols) {
            df_processed[[col]] <- arith_transform(df_processed, col)
            if (isTRUE(input$arith_keep_transformation)) {
                outputTransforms$transforms[[col]] <- function(data) arith_transform(data, col)
                outputTransformsTracker$modifications[[col]] <-
                    c(outputTransformsTracker$modifications[[col]],
                    paste(op, "operation persistent on", col, "with value", val))
            } else {
                outputTransforms$transforms[[col]] <- NULL
                outputTransformsTracker$modifications[[col]] <-
                    c(outputTransformsTracker$modifications[[col]],
                    paste(op, "operation one-time on", col, "with value", val))
            }
        }
        model_output_processed(df_processed)
    }
    
    # If new variables were created, update the pickerInput for 'selected_vars' so they appear.
    if (new_vars_created) {
        updatePickerInput(session, "selected_vars",
                        choices = rv$settings$dailyOutputTable$name,
                        selected = union(input$selected_vars, rv$settings$dailyOutputTable$name[is.na(rv$settings$dailyOutputTable$code)]))
    }
    
    showNotification(paste("Applied", op, "operation to", paste(cols, collapse = ", ")))
})


# Column Interaction
observeEvent(input$apply_interaction_output, {
    req(outputData(), input$col1_output, input$col2_output, input$interaction_op_output)
    df_full <- outputData()
    col1 <- input$col1_output
    cols2 <- input$col2_output
    op <- input$interaction_op_output

    interaction_transform <- function(data) {
        base <- data[[col1]]
        if (op == "Add") {
            combined <- rowSums(data[, cols2, drop = FALSE], na.rm = TRUE)
            base + combined
        } else if (op == "Subtract") {
            combined <- rowSums(data[, cols2, drop = FALSE], na.rm = TRUE)
            base - combined
        } else if (op == "Multiply") {
            combined <- apply(data[, cols2, drop = FALSE], 1, prod, na.rm = TRUE)
            base * combined
        } else if (op == "Divide") {
            combined <- apply(data[, cols2, drop = FALSE], 1, prod, na.rm = TRUE)
            zero_idx <- combined == 0
            if(any(zero_idx)) {
                showNotification("Division by zero encountered in one or more rows; setting those to NA", type = "warning")
                combined[zero_idx] <- NA
            }
            base / combined
        }
    }

    if (isTRUE(input$interaction_newcol)) {
        # --- CREATE NEW VARIABLE ---
        req(input$interaction_newcol_name)
        new_col <- input$interaction_newcol_name
        
        if (isTRUE(input$interaction_keep_transformation)) {
            # PERSISTENT
            outputTransforms$transforms[[new_col]] <- interaction_transform
            df_processed <- model_output_processed()
            df_processed[[new_col]] <- interaction_transform(df_processed)
            model_output_processed(df_processed)
            outputTransformsTracker$modifications[[new_col]] <-
                c(outputTransformsTracker$modifications[[new_col]],
                paste(op, "operation persistent on new column", new_col, "with", paste(cols2, collapse = ", ")))
        } else {
            # SNAPSHOT
            new_snapshot_col <- interaction_transform(df_full)
            new_snapshot_df <- data.frame(Date = df_full$Date, new_snapshot_col)
            names(new_snapshot_df)[2] <- new_col

            current_snapshots <- snapshot_storage()
            if (is.null(current_snapshots)) {
                snapshot_storage(new_snapshot_df)
            } else {
                 if(new_col %in% names(current_snapshots)) {
                    showNotification(paste("Snapshot", new_col, "exists. Overwriting."), type = "warning")
                    current_snapshots[[new_col]] <- NULL
                }
                snapshot_storage(merge(current_snapshots, new_snapshot_df, by = "Date", all = TRUE))
            }
            outputTransformsTracker$modifications[[new_col]] <-
                c(outputTransformsTracker$modifications[[new_col]],
                paste(op, "operation one-time on new column", new_col, "with", paste(cols2, collapse = ", ")))
        }

        # Add new variable to the management table
        new_row <- data.frame(
            index = max(rv$settings$dailyOutputTable$index, 0) + 1,
            code = NA,
            name = new_col,
            stringsAsFactors = FALSE
        )
        rv$settings$dailyOutputTable <- rbind(rv$settings$dailyOutputTable, new_row)

        updatePickerInput(session, "selected_vars",
                        choices = rv$settings$dailyOutputTable$name,
                        selected = union(input$selected_vars, new_col))

    } else {
        # --- MODIFY EXISTING VARIABLE ---
        df_processed <- model_output_processed()
        df_processed[[col1]] <- interaction_transform(df_processed)

        if (isTRUE(input$interaction_keep_transformation)) {
            outputTransforms$transforms[[col1]] <- interaction_transform
             outputTransformsTracker$modifications[[col1]] <-
                c(outputTransformsTracker$modifications[[col1]],
                paste(op, "operation persistent on", col1, "with", paste(cols2, collapse = ", ")))
        } else {
            outputTransforms$transforms[[col1]] <- NULL
             outputTransformsTracker$modifications[[col1]] <-
                c(outputTransformsTracker$modifications[[col1]],
                paste(op, "operation one-time on", col1, "with", paste(cols2, collapse = ", ")))
        }
        model_output_processed(df_processed)
    }

    showNotification(paste("Applied", op, "operation between", col1, "and", paste(cols2, collapse = ", "),
                             if (isTRUE(input$interaction_newcol)) paste("as new column", new_col) else ""))
})


## ---- Reset Transformations UI & Observer ----
observeEvent(input$resetOutputMods, {
    req(simTableDat())
    colsToReset <- input$exportCols
    if (length(colsToReset) > 0) {
        df_processed <- model_output_processed()
        df_initial <- simTableDat()
        snapshots <- snapshot_storage()
        snapshots_modified <- FALSE

        for (col in colsToReset) {
            # Check if it's a snapshot and delete it
            if (!is.null(snapshots) && col %in% names(snapshots)) {
                snapshots[[col]] <- NULL
                snapshots_modified <- TRUE
                outputTransformsTracker$modifications[[col]] <- NULL # Clear tracker
            } else {
                # Otherwise, it's a modification on the main data; reset it
                if (col %in% names(df_initial)) {
                    # Revert to original value if the column was from the model
                    df_processed[[col]] <- df_initial[[col]]
                } else {
                    # Remove the column entirely if it was a persistent new variable
                    df_processed[[col]] <- NULL
                }
                # Remove any stored persistent transformation and its tracker
                outputTransforms$transforms[[col]] <- NULL
                outputTransformsTracker$modifications[[col]] <- NULL
            }
        }

        if (snapshots_modified) {
            # If after removing columns, the snapshot df only has 'Date' left or is empty, set it to NULL
            snapshot_storage(if(ncol(snapshots) <= 1) NULL else snapshots)
        }
        model_output_processed(df_processed)

        # Remove corresponding rows from the management table (dailyOutputTable)
        rv$settings$dailyOutputTable <- rv$settings$dailyOutputTable[
            !(rv$settings$dailyOutputTable$name %in% colsToReset & is.na(rv$settings$dailyOutputTable$code)),
        ]

        # Update the picker input for plotting, ensuring the selection is valid
        existing_selection <- input$selected_vars
        new_selection <- intersect(existing_selection, rv$settings$dailyOutputTable$name)
        updatePickerInput(session, "selected_vars",
            choices = rv$settings$dailyOutputTable$name,
            selected = new_selection)

        showNotification("Selected modification(s) have been reset", type = "message")
    }
})

auto_multi_transform <- function(data) {
    target_cols <- c("GPP", "TR", "NEE", "NEP", "NPP", "NBP", "MR", "GR", "HR","SR")
    data %>% 
        mutate(across(any_of(target_cols), ~ . * 1000))
}

# This observer now ONLY runs when simTableDat() changes (a model run).
# It resets the main data and re-applies only the persistent transformations.
# It DOES NOT touch the snapshot_storage.
observeEvent(simTableDat(), {
    req(simTableDat())
    newData <- simTableDat()

    # Handle the auto-multiplication transformation if the checkbox is checked
    if (isTRUE(input$autoMulti)) {
        outputTransforms$transforms[["autoMulti"]] <- auto_multi_transform
    } else {
        outputTransforms$transforms[["autoMulti"]] <- NULL
    }

    # Apply all persistent transformations in sequence
    # Note: A NULL entry in the list will be skipped.
    for (tranName in names(outputTransforms$transforms)) {
        transform_func <- outputTransforms$transforms[[tranName]]
        if(!is.null(transform_func)) {
             # Special case for whole-dataframe transformations like autoMulti
            if(tranName == "autoMulti") {
                 newData <- transform_func(newData)
            } else {
                 # Standard column-specific transformation
                 newData[[tranName]] <- transform_func(newData)
            }
        }
    }
    
    # This sets the main, non-snapshot data for the new run.
    model_output_processed(newData)
}, ignoreNULL = TRUE, priority = 10) # High priority to run before other reactives

# This observer now just toggles the persistent transform function for auto-multiplication.
# The actual data application happens in the main observeEvent(simTableDat(), ...).
observeEvent(input$autoMulti, {
    # This is the key fix:
    # It ensures this observer doesn't run until model_output_processed() has data.
    # This prevents the error on app launch.
    req(model_output_processed())

    if (isTRUE(input$autoMulti)) {
        # Store transformation only when checked
        outputTransforms$transforms[["autoMulti"]] <- auto_multi_transform
        outputTransformsTracker$modifications[["autoMulti"]] <- "Auto-multiplied GPP, TR, NEE, etc. by 1000"
        
        # Immediately apply the transformation to the current data
        df_processed <- model_output_processed()
        model_output_processed(auto_multi_transform(df_processed))

    } else {
        # Remove the persistent transformation
        outputTransforms$transforms[["autoMulti"]] <- NULL
        outputTransformsTracker$modifications[["autoMulti"]] <- NULL
        
        # Immediately revert the transformation from the current data
        # This is done by re-running the main data processing logic.
        # We re-process from the original simTableDat for this run,
        # making sure to apply any other persistent transforms that might exist.
        
        # Added a req() here for safety, to ensure simTableDat() exists before reverting.
        req(simTableDat())
        newData <- simTableDat()

        # This loop correctly re-applies any other existing transformations.
        for (tranName in names(outputTransforms$transforms)) {
            transform_func <- outputTransforms$transforms[[tranName]]
            if(!is.null(transform_func)) {
                 # Special case for whole-dataframe transformations like autoMulti
                if(tranName == "autoMulti") {
                     newData <- transform_func(newData)
                } else {
                     # Standard column-specific transformation
                     newData[[tranName]] <- transform_func(newData)
                }
            }
        }
        model_output_processed(newData)
    }
})

        
    ######## METRICS CALCULATION #########

   #  simData <- reactive({
   #         req(outputList$nextVal)  
   #         result <- outputList$nextVal
   #         dfs <- as.data.frame(result)
   #         dfs$Date <- as.Date(rownames(result), format = "%d.%m.%Y")
   #         dfs
   # })

   metricsData <- reactive({
        req(outputData(), input$yearRange)  
        
        meas_df <- measurementData() # This will be NULL if all data columns were deleted
        mapping <- mappingRV()
        
        # If meas_df is NULL, or has 0 actual data columns (ncol < 2),
        # or if mapping is NULL/empty, then return an empty metrics structure.
        if (is.null(meas_df) || ncol(meas_df) < 2 || nrow(meas_df) == 0 || is.null(mapping) || length(mapping) == 0) {
            return(data.frame(
                Measurement = character(),
                OutputVariable = character(),
                RMSE = numeric(),
                BIAS = numeric(),
                Correlation = numeric(), 
                NSE = numeric(),
                stringsAsFactors = FALSE
            ))
        }
        
        # Determine selected years
        selectedYears <- if (isTRUE(input$singleYear)) {
            validate(need(is.finite(input$yearRange), "Year not available yet"))
            input$yearRange
        } else {
            validate(need(length(input$yearRange) == 2 && is.finite(input$yearRange[1]) && is.finite(input$yearRange[2]), "Year range not available yet"))
            seq(input$yearRange[1], input$yearRange[2])
        }
        
        # Filter measurement and simulation data to the selected years
        # Ensure meas_df is not NULL before trying to subset
        if (!is.null(meas_df) && "Date" %in% colnames(meas_df)) {
             meas_df_filtered <- meas_df[format(as.Date(meas_df$Date), "%Y") %in% selectedYears, , drop = FALSE]
        } else {
             meas_df_filtered <- NULL # or an empty df with expected structure if preferred
        }

        sim_df_filtered <- outputData()
        if ("Date" %in% colnames(sim_df_filtered)) {
            sim_df_filtered <- sim_df_filtered[format(as.Date(sim_df_filtered$Date), "%Y") %in% selectedYears, , drop = FALSE]
        } else {
            # This case should ideally not happen if outputData() is always structured with a Date column
            return(data.frame(Measurement=character(), OutputVariable=character(), RMSE=numeric(), BIAS=numeric(), Correlation=numeric(), NSE=numeric(), stringsAsFactors=FALSE))
        }

        # If after filtering, meas_df_filtered is NULL or has no rows, return empty metrics
        if (is.null(meas_df_filtered) || nrow(meas_df_filtered) == 0) {
            return(data.frame(Measurement=character(), OutputVariable=character(), RMSE=numeric(), BIAS=numeric(), Correlation=numeric(), NSE=numeric(), stringsAsFactors=FALSE))
        }

        # Merge the two datasets on Date
        # Ensure both data frames have the "Date" column before merging
        if (!("Date" %in% names(meas_df_filtered)) || !("Date" %in% names(sim_df_filtered))) {
             # This indicates a problem upstream with data preparation
            showNotification("Date column missing in measurement or simulation data for metrics calculation.", type = "error")
            return(data.frame(Measurement=character(), OutputVariable=character(), RMSE=numeric(), BIAS=numeric(), Correlation=numeric(), NSE=numeric(), stringsAsFactors=FALSE))
        }
        merged_df <- merge(meas_df_filtered, sim_df_filtered, by = "Date", suffixes = c("_meas", "_simi"))
        
        metrics_list <- lapply(names(mapping), function(meas_col_original_name) {
            output_var <- mapping[[meas_col_original_name]]
            if (output_var == "None") return(NULL)
            
            # Determine actual column names in merged_df (could have _meas or _simi suffix or be original)
            
            x_col_name_in_merged <- paste0(meas_col_original_name, "_meas") 
            y_col_name_in_merged <- paste0(output_var, "_simi")

            # Fallback if suffixes were not added (if names were unique)
            if (!x_col_name_in_merged %in% colnames(merged_df) && meas_col_original_name %in% colnames(merged_df)) {
                x_col_name_in_merged <- meas_col_original_name
            }
            if (!y_col_name_in_merged %in% colnames(merged_df) && output_var %in% colnames(merged_df)) {
                y_col_name_in_merged <- output_var
            }

            if (!x_col_name_in_merged %in% colnames(merged_df) || !y_col_name_in_merged %in% colnames(merged_df)) {
                # This means a mapped column was not found in the merged data, possibly deleted
                # or mapping is stale.
                return(NULL) 
            }
            
            x <- merged_df[[x_col_name_in_merged]]
            y <- merged_df[[y_col_name_in_merged]]
            
            valid <- complete.cases(x, y)
            if (sum(valid) < 2) { # Need at least 2 points for correlation/NSE
                rmse_val <- if(sum(valid) > 0) sqrt(mean((x[valid] - y[valid])^2)) else NA
                bias_val <- if(sum(valid) > 0) mean(y[valid] - x[valid]) else NA
                corr_val <- NA
                nse_val <- NA
            } else {
                rmse_val <- sqrt(mean((x[valid] - y[valid])^2))
                bias_val <- mean(y[valid] - x[valid])
                corr_val <- cor(x[valid], y[valid])^2 
                obs_mean <- mean(x[valid])
                numerator <- sum((x[valid] - y[valid])^2)
                denominator <- sum((x[valid] - obs_mean)^2)
                nse_val <- if (denominator == 0) { if(numerator == 0) 1 else -Inf } else { 1 - numerator / denominator } # Handle perfect fit or zero variance in obs
            }
            
            data.frame(
                Measurement = meas_col_original_name, # Use original name for clarity
                OutputVariable = output_var,
                RMSE = rmse_val,
                BIAS = bias_val,
                Correlation = corr_val,
                NSE = nse_val,
                stringsAsFactors = FALSE
            )
        })
        
        metrics <- do.call(rbind, Filter(NROW, metrics_list)) # Filter out NULLs and ensure NROW > 0 for rbind
        
        if (is.null(metrics) || nrow(metrics) == 0) {
            metrics <- data.frame(
                Measurement = character(),
                OutputVariable = character(),
                RMSE = numeric(),
                BIAS = numeric(),
                Correlation = numeric(),
                NSE = numeric(),
                stringsAsFactors = FALSE
            )
        }
        metrics
    })
        
    #     prevMetricsData <- reactive({
    #     if (firstRun()) return(NULL)
    #     if (!input$lastMetrics) return(data.frame(Measurement = character(),OutputVariable = character(), RMSE = numeric(), BIAS = numeric(), Correlation = numeric(),stringsAsFactors = FALSE)) 
    #     #req(input$lastMetrics)
    #     req(outputList$prev, input$yearRange)  
        
    #     # Get measurement data (if any) and mapping
    #     meas_df <- measurementData()
    #     mapping <- mappingRV()
        
    #     # If no measurement data or no mapping is provided, return an empty data frame (so plots are still generated)
    #     if (is.null(meas_df) || nrow(meas_df) == 0 || is.null(mapping) || length(mapping) == 0) {
    #         return(data.frame(
    #         Measurement = character(),
    #         OutputVariable = character(),
    #         RMSE = numeric(),
    #         BIAS = numeric(),
    #         Correlation = numeric(),
    #         stringsAsFactors = FALSE
    #         ))
    #     }
        
    #     # Determine selected years
    #     selectedYears <- if (input$singleYear) {
    #         input$yearRange
    #     } else {
    #         seq(input$yearRange[1], input$yearRange[2])
    #     }
        
    #     # Filter measurement and simulation data to the selected years
    #     meas_df <- meas_df[format(meas_df$Date, "%Y") %in% selectedYears, ]
        
    #     sim_df <- outputList$prev
    #     sim_df <- sim_df[format(sim_df$Date, "%Y") %in% selectedYears, ]
        
    #     # for the good rmse calc
    #     cols_to_modify <- c("GPP", "TR", "NEE")  
    #     existing_cols <- intersect(cols_to_modify, names(sim_df))  # Check which exist

    #     #sim_df[existing_cols] <- sim_df[existing_cols] * 1000 # COMMENTED OUT BECAUSE OF THE NEW OUTPUT VARIABLE MANAGER

    #     # Merge the two datasets on Date (columns get suffixes to avoid stinky bugs)
    #     merged_df <- merge(meas_df, sim_df, by = "Date", suffixes = c("_meas", "_simi"))
        
    #     # For each mapped measurement, calculate RMSE and correlation
    #     metrics_list <- lapply(names(mapping), function(meas_col) {
    #         output_var <- mapping[[meas_col]]
    #         if (output_var == "None") return(NULL)
            
    #         # Find the correct columns in merged_df
    #         x_col <- if (meas_col %in% colnames(merged_df)) {
    #         meas_col
    #         } else if (paste0(meas_col, "_meas") %in% colnames(merged_df)) {
    #         paste0(meas_col, "_meas")
    #         } else {
    #         NULL
    #         }
            
    #         y_col <- if (output_var %in% colnames(merged_df)) {
    #         output_var
    #         } else if (paste0(output_var, "_simi") %in% colnames(merged_df)) {
    #         paste0(output_var, "_simi")
    #         } else {
    #         NULL
    #         }
            
    #         # Skip if we can’t find the necessary columns
    #         if (is.null(x_col) || is.null(y_col)) return(NULL)
            
    #         x <- merged_df[[x_col]]
    #         y <- merged_df[[y_col]]
            
    #         # Remove pairs where either value is NA
    #         valid <- complete.cases(x, y)
    #         if (sum(valid) == 0) {
    #         rmse_val <- NA
    #         bias_val <- NA
    #         corr_val <- NA
    #         } else {
    #         rmse_val <- sqrt(mean((x[valid] - y[valid])^2))
    #         bias_val <- mean(y[valid] - x[valid])
    #         corr_val <- if (length(x[valid]) > 1) cor(x[valid], y[valid])^2 else NA  #R2
    #         }
            
    #         data.frame(
    #         Measurement = meas_col,
    #         OutputVariable = output_var,
    #         RMSE = rmse_val,
    #         BIAS = bias_val,
    #         Correlation = corr_val,
    #         stringsAsFactors = FALSE
    #         )
    #     })
        
    #     metrics <- do.call(rbind, metrics_list)
    #     if (is.null(metrics)) {
    #         metrics <- data.frame(
    #         Measurement = character(),
    #         OutputVariable = character(),
    #         RMSE = numeric(),
    #         BIAS = numeric(),
    #         Correlation = numeric(),
    #         stringsAsFactors = FALSE
    #         )
    #     }
    #     metrics
        
    # })




        ########## SOIL WATER CONTENT CALCULATION ############
observeEvent(input$make_output, {
    showModal(modalDialog(
        title = "Create New Output Variable",
        # Adding info button at top-right
        div(
            style = "position: absolute; top: 10px; right: 10px;",
            tags$button(
                id = "variable_info_btn",
                class = "btn btn-default action-button",
                tags$i(class = "fa fa-info-circle"),
                title = "Variable Creation Information"
            )
        ),
        # Adding info overlay
        div(
            id = "variable_info_overlay",
            style = "display:none; position:absolute; top:44px; right:10px; width:98%; background:#f9f9f9; border:1px solid #ccc; padding:10px; z-index:1050;",
            tags$p(div(HTML("
            <p><strong>Variable Creation Information</strong></p>
                <p>This panel creates a new variable (for Soil Water Content or Soil Temperature) from model results. You can choose one of two methods:</p>
                <p><strong>1. Specific Depth Interpolation:</strong></p>
                <ul>
                    <li>Calculates the value at a single target depth (e.g., 50 cm).</li>
                    <li>It does this by linearly interpolating between the midpoints of the two closest model layers.</li>
                    <li>Uses only the 'Target Depth' field.</li>
                </ul>
                <p><strong>2. Profile Averaging:</strong></p>
                <ul>
                    <li>Calculates the depth-weighted average value over a soil profile (e.g., 0 cm to 50 cm).</li>
                    <li>It weights the value of each model layer by how much of that layer's thickness falls within your specified 'Min Depth' and 'Max Depth' range.</li>
                    <li>This is useful for comparing with measurements that cover a whole profile.</li>
                </ul>
                <p><strong>Model Layers (Midpoints):</strong></p>
                    <li>Layer 1 [0]: 0-3 cm (midpoint: 1.5 cm)</li>
                    <li>Layer 2 [1]: 3-10 cm (midpoint: 6.5 cm)</li>
                    <li>Layer 3 [2]: 10-30 cm (midpoint: 20 cm)</li>
                    <li>Layer 4 [3]: 30-60 cm (midpoint: 45 cm)</li>
                    <li>Layer 5 [4]: 60-90 cm (midpoint: 75 cm)</li>
                    <li>Layer 6 [5]: 90-120 cm (midpoint: 105 cm)</li>
                    <li>Layer 7 [6]: 120-150 cm (midpoint: 135 cm)</li>
                    <li>Layer 8 [7]: 150-200 cm (midpoint: 175 cm)</li>
                    <li>Layer 9 [8]: 200-400 cm (midpoint: 300 cm)</li>
                    <li>Layer 10 [9]: 400-1000 cm (midpoint: 700 cm)</li>

                </ul>
                <p><strong>Interpolation Formula:</strong></p>
                <p>The value at a given depth is calculated using linear interpolation between the two closest layer midpoints (the assumption is that the model values represent the values at the midpoint of each layer). The formula is:</p>
                <p>Value = (Weight_below * Value_below) + (Weight_above * Value_above)</p>
                <p>Where:</p>
                <ul>
                    <li>Weight_above = (Depth - Depth_below) / (Depth_above - Depth_below)</li>
                    <li>Weight_below = 1 - Weight_above</li>
                    <li>Value_below and Value_above are the values at the midpoints below and above the target depth.</li>
                </ul>
                <p><strong>Example for 50 cm Depth:</strong></p>
                <p>For a depth of 50 cm, the closest midpoints are 45 cm (Layer 4 [3]) and 20 cm (Layer 3 [2]). Suppose the soil water content (<strong>called VWC in the model: 'Volumetric Water Content'</strong>) model values are:</p>
                <ul>
                    <li>VWC at 45 cm = 0.25</li>
                    <li>VWC at 20 cm = 0.30</li>
                </ul>
                <p>Calculate weights:</p>
                <ul>
                    <li>Weight_above = (50 - 45) / (45 - 20) = 5 / 25 = 0.2</li>
                    <li>Weight_below = 1 - 0.2 = 0.8</li>
                </ul>
                <p>Interpolated VWC = (0.8 * 0.25) + (0.2 * 0.30) = 0.2 + 0.06 = 0.26</p>
                <p>Thus, the VWC at 50 cm is 0.26.</p>
            ")))
        ),
       # RadioButtons
        radioButtons("calc_type", "Calculation Method:",
                     choices = c("Specific Depth Interpolation" = "interpolate",
                                 "Profile Averaging" = "average"),
                     selected = "interpolate"),

        # UConditional Min Depth
        conditionalPanel(
            condition = "input.calc_type == 'average'",
            numericInput("min_depth", "Min Depth (cm)", value = 0, min = 0)
        ),
        
        # U Dynamic UI for Max/Target Depth
        uiOutput("depth_input_ui"), 

        textInput("variable_name", "Variable Name", value = "SWC_0_50"),
        selectInput("base_variable", "Base Variable",
                    choices = c("VWC", "Tsoil"),
                    selected = "VWC"),
        easyClose = TRUE,
        footer = tagList(
            modalButton("Cancel"),
            actionButton("create_variable", "Create variable")
        )
    ))
})

    output$depth_input_ui <- renderUI({
        # Ensure input$calc_type is available
        req(input$calc_type) 
        
        if (input$calc_type == "interpolate") {
            numericInput("max_depth", "Target Depth (cm)", value = 50, min = 0)
        } else {
            numericInput("max_depth", "Max Depth (cm)", value = 50, min = 0)
        }
    })


        # Toggle variable info overlay
        observeEvent(input$variable_info_btn, {
            shinyjs::toggle("variable_info_overlay", anim = TRUE)
        })

        # to change the textinput if we switch to tsoil
        observeEvent(input$base_variable, {
            if(input$base_variable == "VWC" && input$variable_name %in% c("Tsoil_0_50", "tsoil_0_50")) {
                updateTextInput(session, "variable_name", value = "SWC_0_50")
            } else if(input$base_variable == "Tsoil" && input$variable_name %in% c("SWC_0_50", "swc_0_50")) {
                updateTextInput(session, "variable_name", value = "Tsoil_0_50")
            }
        })




      observeEvent(input$create_variable, {
        req(input$max_depth, input$variable_name, input$base_variable, input$calc_type)
        
        # Min depth is only required for averaging
        if (input$calc_type == "average") {
            req(input$min_depth)
            # Also check if min depth is less than max depth
            if(input$min_depth >= input$max_depth) {
                showNotification("Min Depth must be less than Max Depth.", type = "error")
                return()
            }
        }
        
        # Check for existing variable name
        if (input$variable_name %in% names(newVars$defs)) {
            showNotification("Variable name already exists. Choose a unique name.", type = "error")
            return()
        }
        
        #  If model output exists, check if base variable is present before adding
        if (!is.null(outputList$nextVal)) {
            dfs_check <- as.data.frame(outputList$nextVal, row.names = rownames(outputList$nextVal))
            if(nrow(dfs_check) > 0) {
                pattern <- paste0("^", input$base_variable, "\\[")
                base_cols <- grep(pattern, names(dfs_check), value = TRUE)
                
                if (length(base_cols) == 0) {
                    showNotification(paste("Cannot create variable:", input$variable_name, ". Base variable '", input$base_variable, "' not found in current model output."), type = "error", duration = 8)
                    return() # Stop execution, do not add variable
                }
            }
        }

        # If we passed the check (or if model hasn't run), proceed to add the variable
        newVars$defs[[input$variable_name]] <- list(
            min_depth = input$min_depth, # Will be 0 if hidden, but that's ok
            max_depth = input$max_depth, # This is the key value (Target or Max)
            base_variable = input$base_variable,
            variable_name = input$variable_name,
            calc_type = input$calc_type # Store the calculation type
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
                    # This notification will still fire for *old* variables if base_cols are missing, which is correct.
                    showNotification(paste("No columns found for base variable", def$base_variable), type = "error")
                    next
                }
                pattern_ind <- paste0(def$base_variable, "\\[|\\]")
                base_indices <- as.numeric(gsub(pattern_ind, "", base_cols))
                current_layers <- layers[base_indices + 1]  # Adjust for R's 1-based indexing
                
                
                # UPDATED: Conditional calculation
                new_val <- apply(dfs[, base_cols, drop = FALSE], 1, function(r) {
                    vals <- as.numeric(r)
                    # Fallback for variables defined before this update
                    calc_type <- ifelse(is.null(def$calc_type), "interpolate", def$calc_type) 
                    
                    if (calc_type == "average") {
                        depth_weighted_average(vals, def$min_depth, def$max_depth, current_layers)
                    } else { # Default to interpolation
                        midpoint_trend_swc(vals, def$max_depth, current_layers)
                    }
                })
                dfs[[var_name]] <- new_val
            }
            
            outputList$nextVal <- as.matrix(dfs)
        }
        else {
            showNotification("Simulation output is empty. The new variable will be computed on the next model run.", type = "message")
        }
        } else {
            showNotification("Model hasn't been run yet. The new variable will be computed on the next model run.", type = "message")
        }
        
        # This code block is now only reached if the new variable was successfully added
        # Update picker input
        new_row <- data.frame(
            index = max(rv$settings$dailyOutputTable$index) + 1,
            code = NA,
            name = input$variable_name
        )

        rv$settings$dailyOutputTable <- rbind(rv$settings$dailyOutputTable, new_row)

        dailyOutputNames(rv$settings$dailyOutputTable$name)
        updatePickerInput(session, "selected_vars",
        choices  = rv$settings$dailyOutputTable$name,
        selected = input$selected_vars
        )
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
    # observe({
    #     req(input$autoupdate, sliderDebounce())

    #        if (isRunning()) return()
    #         isRunning(TRUE)
    #         on.exit(isRunning(FALSE))

    #     isolate({
    #         updateCurrentEPCValues()
    #         #epc <- input$selected_epc
            
    #         for (epc in rv$epc_files) {
    #             paramVal <- epcValues[[epc]]
    #         if (is.null(paramVal)) {
    #             paramVal <- InitialDefaults[[epc]]  # Fallback to initial defaults if needed
    #         }
    #         if (identical(paramVal, InitialDefaults[[epc]])) next # Skip writing if no changes
    #         settings$epcInput[["normal"]] <- epc
    #         prettyChangeMuso(settings, paramVal, 
    #                 calibrationPar = parameters[, 2], 
    #                 fileToChange = "epc", 
    #                 fixAlloc = FALSE)
    #         #print(paste0("Written for: ", epc))
    #         myShowNotification(paste0(epc), type = "message", duration = 5)
            
    #     }

    #     if (!is.null(soil_parameters())){
    #         updateCurrentSoilValues()
    #         paramVal <- soilValues$values
    #         if (!identical(paramVal, InitialDefaultsSoil$values)) {
    #             req(soil_file(), soil_parameters())
    #             prettyChangeMuso(settings, paramVal, calibrationPar = soil_parameters()[,2],
    #                     fileToChange = "soil", fixAlloc = FALSE)
    #             #myShowNotification(paste0("Parameter slider values written into the soil file."), type = "message", duration = 7)
    #         }
    #     }

    #     })
             

    #     isolate({
    #         if(input$destination == "auto") {
    #             outputList$prev <- outputList$nextVal
    #                model_future <- future({
    #             calibMuso(settings = settings, silent = TRUE)
    #         })

    #         result <- tryCatch({
    #             value(model_future)
    #             }, error = function(e) {
    #                 modelCrashed(TRUE)
    #             # If there's an error (model crash), trigger a non-intrusive toast confirmation
    #             if(isTRUE(exportSettings$auto_reset)){
    #                 resetToLastGoodValues()
    #                 #showNotification(paste("Model error:", e$message, "\nResetting to last good values..."), type = "error")
    #             }
    #             else {
    #                 confirmSweetAlert(
    #                     session = session,
    #                     inputId = "resetConfirm",
    #                     title = "Model Crash!",
    #                     text = "The model crashed. Would you like to reset parameters to the last good values?",
    #                     type = "warning",
    #                     btn_labels = c("No", "Yes"),
    #                     closeOnClickOutside = TRUE,
    #                     timer = 0,         # No auto-dismiss
    #                     toast = TRUE,      # Makes it a non-blocking toast-style popup
    #                     position = "top-right"
    #                 )
    #             }
    #                 return(NULL)
    #             })

    #     if (length(result) == 0) {
    #         myShowNotification("Model did not return results! The parameters chosen are likely causing instability in the model!", type = "error", duration = 10)
    #          if(isTRUE(exportSettings$auto_reset)) myShowNotification("Resetting to last good values...", type = "message", duration = 8)
    #     } else {
    #     modelCrashed(FALSE)
    #     print("Model ran successfully")
    #     #showNotification("Model ran successfully", type = "message")
        
    #     updateLastGoodValues()

    #     dfs_orig <- as.data.frame(result, check.names = FALSE)  # 'result' is the simulation output matrix
    

    #     if (length(newVars$defs) > 0) {
          
    #             for (var_name in names(newVars$defs)) {
    #                 def <- newVars$defs[[var_name]]
    #                 pattern <- paste0("^", def$base_variable, "\\[")
    #                 base_cols <- grep(pattern, names(dfs_orig), value = TRUE)
                     
    #                 if (length(base_cols) == 0) {
    #                     showNotification(paste("No columns found for base variable", def$base_variable), type = "error")
    #                     next
    #                 }
    #                 pattern_ind <- paste0(def$base_variable, "\\[|\\]")
    #                 base_indices <- as.numeric(gsub(pattern_ind, "", base_cols))
    #                 current_layers <- layers[base_indices + 1]  
                    
    #                 new_val <- apply(dfs_orig[, base_cols, drop = FALSE], 1, function(r) {
    #                 swc_vals <- as.numeric(r)
    #                 midpoint_trend_swc(swc_vals, def$max_depth, current_layers)
    #                 })
    #                 dfs_orig[[var_name]] <- new_val
    #             }
    #         result <- as.matrix(dfs_orig)
    #     }
    #     outputList$nextVal <- result

    #         }}
            
            
    #          else {
    #             outputList[[input$destination]] <- calibMuso(
    #                 settings = settings,
    #                 silent = TRUE
    #             )
    #         }
    #     })
            
        
    # }) %>% bindEvent(sliderDebounce()) #triggering the event upon slider change


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

    #modalVisible <- reactiveVal(FALSE)
    observeEvent(input$calib, {
        #if(!modalVisible()) {
         #   modalVisible(TRUE)
            showModal(modalDialog(
            title = "Calibration",
             # EPC Mode Section
            div(style = "font-weight: bold; color: #333; margin-bottom: 10px;",
                    paste("It's not yet about calibration and it doesn't look good either (yet!) but you can change the min and max for the sliders")
            ),
        # div(
        #     style = "display: flex; justify-content: flex-end; margin-bottom: 15px;",
        #     actionButton(
        #         "startCalib", 
        #         "Apply", 
        #         style = "background-color: #00cc00; color: white; font-size: 16px;"
        #     )
        # ),
        h4("EPC Mode Parameters",style = "font-weight:bold;"),
        div(
            id = "epcCalibrationInputs",
            lapply(seq_len(nrow(parameters)), function(i) {
                # Skip dependent sliders cause they MUST REMAIN UNCHANGED, THEY SHALL NOT PASS THE CONTRAINTS OF THEIR OWN EXISTENCE
                if (!is.na(parameters$group[i])) return(NULL)
                
                div(
                    class = "calibration-row",
                    style = "margin-bottom: 15px;",
                    div(
                        style = "display: flex; align-items: center; gap: 10px;",
                        h5(parameters$ABREVIATION[i], style = "font-weight: bold; margin: 0;"),
                        actionButton(
                            inputId = paste0("reset_epc_", i),
                            label = NULL,
                            icon = icon("undo"),
                            style = "background-color: #f0f0f0; border: none; padding: 5px;",
                            title = "Reset to default"
                        )
                    ),
                    div(
                        style = "display: flex; gap: 10px;",
                        numericInput(
                            inputId = paste0("epc_min_", i),
                            label = "Min",
                            value = sliderRanges$epcmin[i],
                            step = 1
                        ),
                        numericInput(
                            inputId = paste0("epc_max_", i),
                            label = "Max",
                            value = sliderRanges$epcmax[i],
                            step = 1
                        )
                    )
                )
            })
        ),
             div(style = "font-weight: bold; color: #333; margin-bottom: 10px;",
                    paste("Currently allocation groups remain in the contraints of their own existence. Will free them in the future. Until then they cannot see beyond 1 or 0, but I think they are happy that way")
            ),
        tags$hr(style = "border-top: 5px solid #ccc; margin-top: 30px; margin-bottom: 30px;"),
        # Soil Mode Section
        
        h4("Soil Mode Parameters",style = "font-weight:bold;"),
        if(!is.null(soil_parameters())) {
        div(
            id = "soilCalibrationInputs",
            lapply(seq_len(nrow(soil_parameters())), function(i) {
                div(
                    class = "calibration-row",
                    style = "margin-bottom: 15px;",
                    div(
                        style = "display: flex; align-items: center; gap: 10px;",
                        h5(soil_parameters()$ABREVIATION[i], style = "font-weight: bold; margin: 0;"),
                        actionButton(
                            inputId = paste0("reset_soil_", i),
                            label = NULL,
                            icon = icon("undo"),
                            style = "background-color: #f0f0f0; border: none; padding: 5px;",
                            title = "Reset to default"
                        )
                    ),
                    div(
                        style = "display: flex; gap: 10px;",
                        numericInput(
                            inputId = paste0("soil_min_", i),
                            label = "Min",
                            value = sliderRanges$soimin[i],  
                            step = 1
                        ),
                        numericInput(
                            inputId = paste0("soil_max_", i),
                            label = "Max",
                            value = sliderRanges$soimax[i],  
                            step = 1
                        )
                    )
                )
            })
        )}
        else{
            h4("No parameters_soil.csv found",style = "font-weight:bold;")
        }
        ,       
                    
            easyClose = TRUE,
            footer = tagList(
                modalButton("Cancel"),
                actionButton("startCalib", "Apply", style = "background-color: #00cc00; color: white;")
            )

            ))
        #}
        observeEvent(input$startCalib, {
            # EPC mode updates
            for (i in seq_len(nrow(parameters))) {
                if (is.na(parameters$group[i])) {  # Only non-dependent sliders
                    min_id <- paste0("epc_min_", i)
                    max_id <- paste0("epc_max_", i)
                    if (!is.null(input[[min_id]])) sliderRanges$epcmin[i] <- input[[min_id]]
                    if (!is.null(input[[max_id]])) sliderRanges$epcmax[i] <- input[[max_id]]
                }
            }
            
            # Soil mode updates
        if(!is.null(soil_parameters())) {
            for (i in seq_len(nrow(soil_parameters()))) {
                min_id <- paste0("soil_min_", i)
                max_id <- paste0("soil_max_", i)
                if (!is.null(input[[min_id]])) sliderRanges$soimin[i] <- input[[min_id]]
                if (!is.null(input[[max_id]])) sliderRanges$soimax[i] <- input[[max_id]]
            }
        }
            removeModal()
            #modalVisible(FALSE)
        })
            lapply(seq_len(nrow(parameters)), function(i) {
                    if (is.na(parameters$group[i])) {
                        observeEvent(input[[paste0("reset_epc_", i)]], {
                            updateNumericInput(
                                session,
                                inputId = paste0("epc_min_", i),
                                value = parameters[i, 3]
                            )
                            updateNumericInput(
                                session,
                                inputId = paste0("epc_max_", i),
                                value = parameters[i, 4]
                            )
                        })
                    }
                })
                
                # Reset button logic for Soil
            if(!is.null(soil_parameters())) {
                lapply(seq_len(nrow(soil_parameters())), function(i) {
                    observeEvent(input[[paste0("reset_soil_", i)]], {
                        updateNumericInput(
                            session,
                            inputId = paste0("soil_min_", i),
                            value = soil_parameters()[i, 3]
                        )
                        updateNumericInput(
                            session,
                            inputId = paste0("soil_max_", i),
                            value = soil_parameters()[i, 4]
                        )
                    })
                })
            }
    })



        # Settings (so far only for resolution)
        exportSettings <- reactiveValues(width = 900, height = 500, format = "png",scale = 2, auto_reset = FALSE, muteNotif = FALSE, tickfontx = 12, tickfonty = 12, legendfont = 12, xtitlefont = 14, ytitlefont = 14, legendxanchor = 1.2, legendyanchor = 1, allowLegendMovement = FALSE)
        defaultExportSettings <- list(width = 900, height = 500, format = "png",scale = 2, auto_reset = FALSE, muteNotif = FALSE, tickfontx = 12, tickfonty = 12, legendfont = 12, xtitlefont = 14, ytitlefont = 14, legendxanchor = 1.2, legendyanchor = 1)
        

          observeEvent(input$settings_btn, {
            showModal(modalDialog(
            title = "Settings",
            div(style = "font-weight: bold; color: #333; margin-bottom: 10px;",
                    paste("Current Working Directory:", workdir)
            ),
            selectInput("export_format", "Image Export Format",
                        choices = c("png","jpeg","webp","svg"),
                        selected = exportSettings$format
            ),
            # Inputs for resolution settings
            div(style = "display: flex; align-items: center; gap: 5px;",
                numericInput("export_width", "Image Export Width (px):", value = exportSettings$width),
                    actionButton("reset_export_width", 
                    label = NULL, 
                    icon = icon("undo"), 
                    style = "margin-top: 10px;",
                    title = "Reset to default"
                    )
            ),
            div(style = "display: flex; align-items: center; gap: 5px;",   
                numericInput("export_height", "Image Export Height (px):", value = exportSettings$height),
                  actionButton("reset_export_height", 
                    label = NULL, 
                    icon = icon("undo"), 
                    style = "margin-top: 10px;",
                    title = "Reset to default"
                    )
            ),
            div(style = "display: flex; align-items: center; gap: 5px;",  
                numericInput("export_scale", "Image Export Scale:", value = exportSettings$scale, min = 1),
                actionButton("reset_export_scale", 
                    label = NULL, 
                    icon = icon("undo"), 
                    style = "margin-top: 10px;",
                    title = "Reset to default"
                    )

            ),
            div(style = "display: flex; align-items: center; gap: 5px;",  
                numericInput("tickfontx", "X-Axis Tick Font Size:", value = exportSettings$tickfontx, min = 1),
                actionButton("reset_tickfontx", 
                    label = NULL, 
                    icon = icon("undo"), 
                    style = "margin-top: 10px;",
                    title = "Reset to default"
                    )
            ),

            div(style = "display: flex; align-items: center; gap: 5px;",  
                numericInput("tickfonty", "Y-Axis Tick Font Size:", value = exportSettings$tickfonty, min = 1),
                actionButton("reset_tickfonty", 
                    label = NULL, 
                    icon = icon("undo"), 
                    style = "margin-top: 10px;",
                    title = "Reset to default"
                    )
            ),
            div(style = "display: flex; align-items: center; gap: 5px;",  
                numericInput("xtitlefont", "X-Axis Title Font Size:", value = exportSettings$xtitlefont, min = 1),
                actionButton("reset_xtitlefont", 
                    label = NULL, 
                    icon = icon("undo"), 
                    style = "margin-top: 10px;",
                    title = "Reset to default"
                    )
            ),
            div(style = "display: flex; align-items: center; gap: 5px;",  
                numericInput("ytitlefont", "Y-Axis Title Font Size:", value = exportSettings$ytitlefont, min = 1),
                actionButton("reset_ytitlefont", 
                    label = NULL, 
                    icon = icon("undo"), 
                    style = "margin-top: 10px;",
                    title = "Reset to default"
                    )
            ),
            div(style = "display: flex; align-items: center; gap: 5px;",  
                numericInput("legendfont", "Legend Font Size:", value = exportSettings$legendfont, min = 1),
                actionButton("reset_legendfont", 
                    label = NULL, 
                    icon = icon("undo"), 
                    style = "margin-top: 10px;",
                    title = "Reset to default"
                    )
            ),
            checkboxInput(
                "allowLegendDisplacement", "Allow custom legend position", value = exportSettings$allowLegendMovement
            ),
            div(style = "display: flex; align-items: center; gap: 5px;",
                numericInput("legendxanchor", "Legend x position:", value = exportSettings$legendxanchor),
                actionButton("reset_legendxanchor", 
                    label = NULL, 
                    icon = icon("undo"), 
                    style = "margin-top: 10px;",
                    title = "Reset to default"
                    )
            ),
            div(style = "display: flex; align-items: center; gap: 5px;",
                numericInput("legendyanchor", "Legend y position:", value = exportSettings$legendyanchor),
                actionButton("reset_legendyanchor", 
                    label = NULL, 
                    icon = icon("undo"), 
                    style = "margin-top: 10px;",
                    title = "Reset to default"
                    )
            ),
            checkboxInput("auto_reset", "Auto Reset Sliders Upon Model Crash To Last Successful Values", value = exportSettings$auto_reset),
            checkboxInput("mute_notif", "Mute Common Notifications", value = exportSettings$muteNotif),
            textAreaInput("feedback_message", "Feedback", placeholder = "Report a bug or request a feature."),
                                    actionButton("submit_feedback", "Send Feedback"),
                                    verbatimTextOutput("feedback_status"),
           div(
                style = "position: absolute; top: 10px; right: 10px;",
                  tags$button(
                id = "info_btn",
                class = "btn btn-default action-button",  # Added 'action-button'
                tags$i(class = "fa fa-info-circle"),  
                title = "App Information"
            ),
                tags$button(
                    id = "fullscreen_btn",
                    class = "btn btn-default",
                    tags$i(class = "fa fa-expand"),  
                    title = "Go Fullscreen [F11] (only works in browser)"
                )
            ),
            div(
                id = "info_overlay",
                style = "display:none; position:absolute; top:44px; left:0; width:100%; background:#f9f9f9; border:1px solid #ccc; padding:10px; z-index:1050;",
                tags$p(div(HTML("
                    <p><strong>Version 2.25.0</strong></p>
                    <p>Current known bugs/problems:</p>
                    <ul>
                        <li>Auto-calculation for allocation can make the sliders oscillate between two values due to some latency bugs. If that happens, turn off auto-calc if they can't find values within a few seconds.</li>
                        <li>When deleting a custom variable via reset, plotly will complain it cannot find it (if it was previously plotted), but just ignore it, it's fine (will be fixed so plotly won't complain)</li>
                        <li>Sometimes there will be a notification for an epc modification (in crop rotation) even if we didn't move any of its sliders. In that case, don't worry it didn't change any of its values, it's a type issue probably, will be fixed</li>
                        <li>In the logger the visible decimals are limited to 3. So when viewing differences anything below 0.001 change will not be visible. It's only a visual issue, the exact values of the run can still be applied. Will make a dynamic function to show the decimals beyond for such cases</li>
                    </ul>
                    "))),
                #actionButton("close_info_overlay", "Close")
            ),
            
            easyClose = TRUE,
            footer = tagList(
                modalButton("Cancel"),
                actionButton("apply_settings", "Apply")
            )
            ))
        })


        observeEvent(input$info_btn, {
            shinyjs::toggle("info_overlay", anim = TRUE)  # Toggle visibility
        })

        
        observeEvent(input$close_info_overlay, {
            shinyjs::hide("info_overlay", anim = TRUE)
        })



        #observeEvent(input$close_info_overlay, {
        #    shinyjs::hide("info_overlay", anim = TRUE)
        #})
        
        # When the user clicks "Apply", update the reactive values and close the modal
        observeEvent(input$apply_settings, {
            exportSettings$width <- input$export_width
            exportSettings$height <- input$export_height
            exportSettings$scale <- input$export_scale
            exportSettings$auto_reset <- input$auto_reset
            exportSettings$muteNotif <- input$mute_notif
            exportSettings$tickfontx <- input$tickfontx
            exportSettings$tickfonty <- input$tickfonty
            exportSettings$xtitlefont <- input$xtitlefont
            exportSettings$ytitlefont <- input$ytitlefont
            exportSettings$legendfont <- input$legendfont
            exportSettings$allowLegendMovement <- input$allowLegendDisplacement
            exportSettings$legendxanchor <- input$legendxanchor
            exportSettings$legendyanchor <- input$legendyanchor
            exportSettings$format <- input$export_format
            
            removeModal()
        })

        #
        observeEvent(input$mute_notif, {
            exportSettings$muteNotif <- input$mute_notif
        })

        observeEvent(input$reset_export_width, {
            updateNumericInput(session, "export_width", value = defaultExportSettings$width)
        })

        observeEvent(input$reset_export_height, {
            updateNumericInput(session, "export_height", value = defaultExportSettings$height)
        })

        observeEvent(input$reset_export_scale, {
            updateNumericInput(session, "export_scale", value = defaultExportSettings$scale)
        })

        observeEvent(input$reset_tickfontx, {
            updateNumericInput(session, "tickfontx", value = defaultExportSettings$tickfontx)
        })

        observeEvent(input$reset_tickfonty, {
            updateNumericInput(session, "tickfonty", value = defaultExportSettings$tickfonty)
        })

        observeEvent(input$reset_xtitlefont, {
            updateNumericInput(session, "xtitlefont", value = defaultExportSettings$xtitlefont)
        })

        observeEvent(input$reset_ytitlefont, {
            updateNumericInput(session, "ytitlefont", value = defaultExportSettings$ytitlefont)
        })

        observeEvent(input$reset_legendfont, {
            updateNumericInput(session, "legendfont", value = defaultExportSettings$legendfont)
        })

        observeEvent(input$reset_legendxanchor, {
            updateNumericInput(session, "legendxanchor", value = defaultExportSettings$legendxanchor)
        })

        observeEvent(input$reset_legendyanchor, {
            updateNumericInput(session, "legendyanchor", value = defaultExportSettings$legendyanchor)
        })



        myShowNotification <- function(message, type = "message", duration = NULL, ...) {
            if (!exportSettings$muteNotif) {
                showNotification(message, type = type, duration = duration, ...)
            }
        }

        
 plotCustomizations <- reactiveValues()
    # Staging area for pending changes
    pendingCustomizations <- reactiveValues()
    
    # Helper to convert legacy Plotly linetypes to ggplot2
    fix_linetype <- function(lt) {
      if (is.null(lt)) return("solid")
      switch(lt,
             "dash" = "dashed",
             "dot" = "dotted",
             "dashdot" = "dotdash",
             lt)
    }

    # Update selectInput choices and initialize defaults
    observeEvent(input$selected_vars, {
        req(input$selected_vars)
        updateSelectInput(session, "customize_var", 
                         choices = input$selected_vars,
                         selected = if (is.null(input$customize_var)) input$selected_vars[1] else input$customize_var)

        for (var in input$selected_vars) {
            if (is.null(plotCustomizations[[var]])) {
                plotCustomizations[[var]] <- list(
                    y_min = NULL,
                    y_max = NULL,
                    y_title = var,
                    line_type = "solid",
                    line_color = "red",
                    line_width = 1,
                    title_font_size = exportSettings$ytitlefont,
                    show_legend = TRUE,
                    meas_marker_type = 16, # Default to filled circle (16)
                    meas_marker_color = "#047704",
                    meas_marker_size = 3, # Adjusted default size for ggplot
                    additional_vars = NULL,
                    additional_vars_settings = list(),
                    selected_additional_var = "",
                    show_measurements = TRUE
                )
            }
        }
    })

     output$plot_manager <- renderUI({
        req(input$customize_var)
        custom <- plotCustomizations[[input$customize_var]]
        selected_var <- if (custom$selected_additional_var %in% custom$additional_vars) {
                            custom$selected_additional_var
                        } else {
                            ""
                        }
        tagList(
            pickerInput("additional_vars", "Show Additional Variable(s)", 
                       choices = rv$settings$dailyOutputTable$name,
                       selected = custom$additional_vars,  
                       multiple = TRUE, 
                       options = list(`actions-box` = TRUE)),
            numericInput("y_min", "Y Min", value = custom$y_min, step = 0.1),
            numericInput("y_max", "Y Max", value = custom$y_max, step = 0.1),
            
            # UPDATED: ggplot2 compatible line types
            selectInput(paste0("line_type"), "Line Type",
                        choices = c("Solid" = "solid", "Dashed" = "dashed", "Dotted" = "dotted", "Dot-Dash" = "dotdash"),
                        selected = fix_linetype(custom$line_type)),
                        
            colourInput("line_color", "Line Color", value = custom$line_color, allowTransparent = TRUE),
            numericInput("line_width", "Line Width", value = custom$line_width, min = 0.5, max = 10, step = 0.5),
            textInput("y_title", "Y Title", value = custom$y_title),
            
            tags$div(
                style = "margin-bottom: 10px;",
                tags$button(id = "bold_btn", title = "Bold", tags$i(class = "fas fa-bold")),
                tags$button(id = "italic_btn", title = "Italic", tags$i(class = "fas fa-italic")),
                tags$button(id = "sup_btn", title = "Superscript", tags$i(class = "fas fa-superscript")),
                tags$button(id = "sub_btn", title = "Subscript", tags$i(class = "fas fa-subscript"))
            ),
            numericInput("title_size", "Y Title Size", value = custom$title_font_size, min = 8, max = 24, step = 1),
            checkboxInput("show_legend", "Show Legend (not functional, use the top right button)", value = custom$show_legend),
            checkboxInput("show_measurements", "Show Measurements", value = custom$show_measurements),
            
            # UPDATED: ggplot2 compatible shapes (Integers)
            selectInput("meas_marker_type", "Measurement Marker Type", 
                       choices = c("Circle" = 16, "Triangle" = 17, "X" = 4, 
                                  "Square" = 15, "Diamond" = 18), 
                       selected = custom$meas_marker_type),
                       
            colourInput("meas_marker_color", "Measurement Marker Color", value = custom$meas_marker_color,allowTransparent = TRUE),
            numericInput("meas_marker_size", "Measurement Marker Size", value = custom$meas_marker_size, min = 1, max = 20, step = 1),
            tags$hr(style = "border-top: 5px solid #ccc; margin-top: 30px; margin-bottom: 30px;"),
            div(style = "margin-top: 0px;",
                selectInput("customize_additional_var", "Customize Additional Variable(s)",
                    choices = c("None" = "", custom$additional_vars),
                    selected = selected_var)
            ),
            uiOutput("additional_var_customizations"),


            actionButton("apply_custom", "Apply to Selected Variable"),
            actionButton("reset_custom", "Reset Current Variable")
        )
    })

    output$additional_var_customizations <- renderUI({
        req(input$customize_additional_var, input$customize_additional_var != "")
        add_var <- input$customize_additional_var
        custom <- plotCustomizations[[input$customize_var]]
        if (is.null(custom$additional_vars_settings[[add_var]])) {
            custom$additional_vars_settings[[add_var]] <- list(
            line_type = "solid",
            line_color = "green",
            line_width = 1,
            meas_marker_type = 16,  # Default ggplot shape (16 = filled circle)
            meas_marker_color = "#047704",
            meas_marker_size = 3,
            show_measurements = FALSE
            )
        }

        add_custom <- custom$additional_vars_settings[[add_var]]
        mapping <- mappingRV()
        mappedCols <- if (!is.null(mapping)) names(mapping)[mapping == add_var] else character(0)
        has_measurements <- length(mappedCols) > 0
        
        tagList(
            selectInput(paste0("line_type_", add_var), "Line Type",
                        choices = c("Solid" = "solid", "Dashed" = "dashed", "Dotted" = "dotted", "Dot-Dash" = "dotdash"),
                        selected = fix_linetype(add_custom$line_type)),
            colourInput(paste0("line_color_", add_var), "Line Color", value = add_custom$line_color,allowTransparent = TRUE),
            numericInput(paste0("line_width_", add_var), "Line Width", value = add_custom$line_width, min = 0.5, max = 10, step = 0.5),
            if (has_measurements) {
                tagList(
                    checkboxInput(paste0("show_measurements_", add_var), "Show Measurements", value = add_custom$show_measurements),
                    # UPDATED: ggplot2 shapes here as well
                    selectInput(paste0("meas_marker_type_", add_var), "Measurement Marker Type",
                                choices = c("Circle" = 16, "Triangle" = 17, "X" = 4, 
                                            "Square" = 15, "Diamond" = 18),
                                selected = add_custom$meas_marker_type),
                    colourInput(paste0("meas_marker_color_", add_var), "Measurement Marker Color", 
                                value = add_custom$meas_marker_color,allowTransparent = TRUE),
                    numericInput(paste0("meas_marker_size_", add_var), "Measurement Marker Size", 
                                value = add_custom$meas_marker_size, min = 1, max = 20, step = 1),

                )
            }
        )
    })

    # Apply button: Move pending changes to main customizations
    observeEvent(input$apply_custom, {
        req(input$customize_var)
        custom <- plotCustomizations[[input$customize_var]]
        plotCustomizations[[input$customize_var]] <- list(
            y_min = input$y_min,
            y_max = input$y_max,
            y_title = input$y_title,
            line_type = input$line_type,
            line_color = input$line_color,
            line_width = input$line_width,
            title_font_size = input$title_size,
            show_legend = input$show_legend,
            meas_marker_type = as.integer(input$meas_marker_type), # Ensure integer
            meas_marker_color = input$meas_marker_color,
            meas_marker_size = input$meas_marker_size,
            additional_vars = input$additional_vars,
            additional_vars_settings = custom$additional_vars_settings,
            selected_additional_var = input$customize_additional_var,
            show_measurements = input$show_measurements

        )

        if (!is.null(input$customize_additional_var) && input$customize_additional_var != "") {
            add_var <- input$customize_additional_var
            plotCustomizations[[input$customize_var]]$additional_vars_settings[[add_var]] <- list(
            line_type = input[[paste0("line_type_", add_var)]],
            line_color = input[[paste0("line_color_", add_var)]],
            line_width = input[[paste0("line_width_", add_var)]],
            meas_marker_type = as.integer(input[[paste0("meas_marker_type_", add_var)]]), # Ensure integer
            meas_marker_color = input[[paste0("meas_marker_color_", add_var)]],
            meas_marker_size = input[[paste0("meas_marker_size_", add_var)]],
            show_measurements = input[[paste0("show_measurements_", add_var)]]
            )
        }
    })

    # Reset button: Reset main customizations and update UI
    observeEvent(input$reset_custom, {
        req(input$customize_var)
        plotCustomizations[[input$customize_var]] <- list(
            y_min = NULL,
            y_max = NULL,
            y_title = input$customize_var,
            line_type = "solid",
            line_color = "red",
            line_width = 1,
            title_font_size = exportSettings$ytitlefont,
            show_legend = TRUE,
            meas_marker_type = 16,
            meas_marker_color = "#047704",
            meas_marker_size = 3,
            additional_vars = NULL,
            additional_vars_settings = list(),
            selected_additional_var = "",
            show_measurements = TRUE

        )
        custom <- plotCustomizations[[input$customize_var]]
        updateTextInput(session, "y_title", value = custom$y_title)
        updateNumericInput(session, "y_min", value = custom$y_min)
        updateNumericInput(session, "y_max", value = custom$y_max)
        updateSelectInput(session, "line_type", selected = custom$line_type)
        updateColourInput(session, "line_color", value = custom$line_color)
        updateNumericInput(session, "line_width", value = custom$line_width)
        updateNumericInput(session, "title_size", value = custom$title_font_size)
        updateCheckboxInput(session, "show_legend", value = custom$show_legend)
        updateSelectInput(session, "meas_marker_type", selected = custom$meas_marker_type)
        updateColourInput(session, "meas_marker_color", value = custom$meas_marker_color)
        updateNumericInput(session, "meas_marker_size", value = custom$meas_marker_size)
        updatePickerInput(session, "additional_vars", selected = NULL)
        updateCheckboxInput(session, "show_measurements", value = TRUE)
    })
        


################ PLOTTING ###############

output$dynamicPlots <- renderUI({
  req(input$selected_vars)
  session$sendCustomMessage("save_scroll", list(id = "plotPanel"))
  
  plot_outputs <- lapply(input$selected_vars, function(var) {
    plotOutput(paste0("plot_", var), height = "400px")
  })
  do.call(tagList, plot_outputs)
})

observe({
  req(input$selected_vars, outputData())

  lapply(input$selected_vars, function(var) {
    
    output[[paste0("plot_", var)]] <- renderPlot({
      
      # --- Data Preparation ---
      
      if (isTRUE(input$singleYear)) {
        validate(need(is.finite(input$yearRange), "Year not available yet"))
        selectedYears <- input$yearRange 
      } else {
        validate(
          need(length(input$yearRange) == 2 &&
                 is.finite(input$yearRange[1]) &&
                 is.finite(input$yearRange[2]),
               "Year range not available yet")
        )
        selectedYears <- seq(input$yearRange[1], input$yearRange[2])
      }
      
      # Filter dates
      filteredDates <- dates[as.numeric(format(dates, "%Y")) %in% selectedYears]
      
      # Get simulation data
      filteredPrev <- if (length(outputList$prev) != 0) {
        outputList$prev[as.numeric(format(dates, "%Y")) %in% selectedYears, ]
      } else NULL
      
      filteredNext <- outputData()[as.numeric(format(dates, "%Y")) %in% selectedYears, ]
      
      # Custom settings
      custom <- plotCustomizations[[var]]
      
      # Bond logic
      previous_bonds_list <- bondsForPrevPlot()
      current_bonds_list <- bondsForCurrentPlot()
      was_prev_bonded <- hasActiveBonds(previous_bonds_list)
      is_current_bonded <- hasActiveBonds(current_bonds_list)
      
      # Measurement Data
      mapping <- mappingRV()
      df <- measurementData()
      df_filtered <- df[format(df$Date, "%Y") %in% selectedYears, ]
      metrics_df <- metricsData()
      mappedCols <- if (!is.null(mapping)) names(mapping)[mapping == var] else character(0)
      
      # Legend Labels
      prev_legend_label <- ifelse(was_prev_bonded, 
                                  paste0("Previous Comparison ", var), 
                                  paste0("Previous ", var))
      current_legend_label <- ifelse(is_current_bonded, 
                                     paste0("Current Comparison ", var), 
                                     paste0("New ", var))
      current_legend_label_single <- ifelse(is_current_bonded, 
                                            paste0("Comparison ", var), 
                                            paste0(var, ""))
      
      # --- Initialize Vectors for Manual Scales ---
      # Using named vectors to ensure strict mapping
      manual_colors <- c()
      manual_linetypes <- c()
      manual_shapes <- c() 
      
      # --- Logic Branching: Scatter vs Line Plot ---
      
      is_scatter_metrics <- (length(mappedCols) > 0 && input$plotType != "line")
      
      # Check Legend Visibility
      show_legend <- if(exists("legendVisible") && is.function(legendVisible)) legendVisible() else TRUE
      
      # Initialize ggplot
      # CHANGE 1: Set base_family to "sans" (Arial/Helvetica) for a cleaner look
      p <- ggplot() + theme_bw(base_family = "sans") + 
        theme(
          text = element_text(size = 12, family = "sans"), # Ensure text elements use the font
          axis.text.x = element_text(size = exportSettings$tickfontx),
          axis.text.y = element_text(size = exportSettings$tickfonty),
          legend.text = element_text(size = exportSettings$legendfont),
          axis.title.y = element_text(margin = margin(t = 0, r = -20, b = 0, l = 0)),
          # Adjusted margin (removed specific axis padding since title is gone)
          plot.margin = margin(10, 10, 35, 10), 
          # Legend Position: Top
          legend.position = if (show_legend) "top" else "none",
          legend.direction = "horizontal",
          legend.box = "vertical",
          # CHANGE: Increased spacing between the plot and the legend to avoid collision
          legend.box.spacing = unit(0.5, "cm")
        )
      
      # Helper to pad y-axis labels to fixed width for alignment
      pad_labels <- function(x) {
        format(x, trim = FALSE, width = 10, justify = "right") 
      }
      
      if (is_scatter_metrics) {
        # --- SCATTER PLOT ---
        
        global_abs_min <- Inf
        global_abs_max <- -Inf
        
        for (col in mappedCols) {
          sim_vals <- filteredNext[, var]
          meas_vals <- df_filtered[[col]]
          sim_data <- data.frame(Date = filteredDates, sim = sim_vals)
          meas_data <- df_filtered[, c("Date", col)]
          common_data <- merge(sim_data, meas_data, by = "Date")
          
          local_min <- min(c(common_data$sim, common_data[[col]]), na.rm = TRUE)
          local_max <- max(c(common_data$sim, common_data[[col]]), na.rm = TRUE)
          global_abs_min <- min(global_abs_min, local_min)
          global_abs_max <- max(global_abs_max, local_max)
        }
        
        breaks <- pretty(c(global_abs_min, global_abs_max), n = 8)
        min_tick <- min(breaks)
        max_tick <- max(breaks)
        
        for (i in seq_along(mappedCols)) {
          col <- mappedCols[i]
          m_row <- metrics_df[metrics_df$Measurement == col, ]
          rmse_str <- if (nrow(m_row) > 0 && !is.na(m_row$RMSE)) sprintf("RMSE: %.2f", m_row$RMSE) else "RMSE: NA"
          bias_str <- if (nrow(m_row) > 0 && !is.na(m_row$BIAS)) sprintf("Bias: %.2f", m_row$BIAS) else "Bias: NA"
          corr_str <- if (nrow(m_row) > 0 && !is.na(m_row$Correlation)) sprintf("R2: %.2f", m_row$Correlation) else "R2: NA"
          metric_label <- paste(rmse_str, bias_str, corr_str, sep = " | ")
          label_name <- paste0(col, " Metrics\n", metric_label)
          
          sim_data <- data.frame(Date = filteredDates, sim = filteredNext[, var])
          meas_data <- df_filtered[, c("Date", col)]
          plot_data <- merge(sim_data, meas_data, by = "Date")
          colnames(plot_data)[3] <- "measured" 
          
          # Map all aesthetics to ensure correct guide generation if needed later
          p <- p + geom_point(data = plot_data, 
                              aes(x = measured, y = sim, color = label_name, shape = label_name),
                              size = 2)
          
          manual_colors[label_name] <- "#d99820"
          manual_linetypes[label_name] <- "blank"
          manual_shapes[label_name] <- 16
        }
        
        p <- p + geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue", linewidth = 0.8) +
          coord_fixed(ratio = 1, xlim = c(min_tick, max_tick), ylim = c(min_tick, max_tick)) +
          labs(x = paste0("Measured ", paste(mappedCols, collapse=", ")), y = paste0("Simulated ", var))
        
      } else {
        # --- TIME SERIES PLOT ---
        
        # 1. Previous Run Line
        if (!is.null(filteredPrev) && input$lastRun) {
          prev_df <- data.frame(Date = filteredDates, Value = filteredPrev[, var])
          p <- p + geom_line(data = prev_df, 
                             aes(x = Date, y = Value, color = prev_legend_label, linetype = prev_legend_label),
                             linewidth = custom$line_width)
          
          manual_colors[prev_legend_label] <- "#2b2bf8ef"
          manual_linetypes[prev_legend_label] <- fix_linetype(custom$line_type)
          manual_shapes[prev_legend_label] <- NA # Lines don't have shapes
        }
        
        # 2. Current Run Line
        curr_df <- data.frame(Date = filteredDates, Value = filteredNext[, var])
        curr_lbl <- if(!is.null(filteredPrev) && input$lastRun) current_legend_label else current_legend_label_single
        
        p <- p + geom_line(data = curr_df, 
                           aes(x = Date, y = Value, color = curr_lbl, linetype = curr_lbl),
                           linewidth = custom$line_width)
        
        manual_colors[curr_lbl] <- custom$line_color
        manual_linetypes[curr_lbl] <- fix_linetype(custom$line_type)
        manual_shapes[curr_lbl] <- NA
        
        # 3. Additional Variables
        if (!is.null(custom$additional_vars)) {
          for (add_var in custom$additional_vars){
            if (add_var != var && add_var %in% colnames(filteredNext)) {
              add_custom <- custom$additional_vars_settings[[add_var]]
              if (is.null(add_custom)) {
                add_custom <- list(line_type = "solid", line_color = "blue", line_width = custom$line_width, show_measurements = FALSE)
              }
              
              add_df <- data.frame(Date = filteredDates, Value = filteredNext[, add_var])
              p <- p + geom_line(data = add_df, 
                                 aes(x = Date, y = Value, color = add_var, linetype = add_var),
                                 linewidth = add_custom$line_width)
              
              manual_colors[add_var] <- add_custom$line_color
              manual_linetypes[add_var] <- fix_linetype(add_custom$line_type)
              manual_shapes[add_var] <- NA
              
              if (!is.null(filteredPrev) && input$lastRun) {
                prev_add_df <- data.frame(Date = filteredDates, Value = filteredPrev[, add_var])
                prev_lbl <- paste0(add_var, " (Prev)")
                p <- p + geom_line(data = prev_add_df, 
                                   aes(x = Date, y = Value, color = prev_lbl, linetype = prev_lbl),
                                   linewidth = add_custom$line_width)
                
                manual_colors[prev_lbl] <- alpha(add_custom$line_color, 0.5)
                manual_linetypes[prev_lbl] <- fix_linetype(add_custom$line_type)
                manual_shapes[prev_lbl] <- NA
              }
              
              if(isTRUE(add_custom$show_measurements)) {
                add_mappedCols <- if (!is.null(mapping)) names(mapping)[mapping == add_var] else character(0)
                if (length(add_mappedCols) > 0) {
                  for (col in add_mappedCols) {
                    yData <- df_filtered[[col]]
                    if(input$avoid_negative){
                      if (var %in% c("GPP", "TR")) yData[yData < 0] <- NA
                    }
                    
                    m_row <- metrics_df[metrics_df$Measurement == col, ]
                    rmse_str <- if (nrow(m_row) > 0 && !is.na(m_row$RMSE)) sprintf("RMSE: %.2f", m_row$RMSE) else "RMSE: NA"
                    bias_str <- if (nrow(m_row) > 0 && !is.na(m_row$BIAS)) sprintf("Bias: %.2f", m_row$BIAS) else "Bias: NA"
                    corr_str <- if (nrow(m_row) > 0 && !is.na(m_row$Correlation)) sprintf("R2: %.2f", m_row$Correlation) else "R2: NA"
                    metric_label <- paste(rmse_str, bias_str, corr_str, sep = " | ")
                    meas_lbl <- paste0(col, " Meas\n", metric_label)
                    
                    meas_df <- data.frame(Date = df_filtered$Date, Value = yData)
                    
                    # Ensure shape is mapped correctly
                    current_shape <- as.integer(add_custom$meas_marker_type)
                    if (is.na(current_shape)) current_shape <- 16
                    
                    # Map both color and shape. DO NOT map linetype for points.
                    p <- p + geom_point(data = meas_df,
                                        aes(x = Date, y = Value, color = meas_lbl, shape = meas_lbl),
                                        size = add_custom$meas_marker_size)
                    
                    manual_colors[meas_lbl] <- add_custom$meas_marker_color
                    manual_linetypes[meas_lbl] <- "blank" # Use blank so line key is invisible for points
                    manual_shapes[meas_lbl] <- current_shape
                  }
                }
              }
            }
          }
        }
        
        # 4. Primary Measurements
        if (length(mappedCols) > 0 && isTRUE(custom$show_measurements)) {
          for (col in mappedCols) {
            yData <- df_filtered[[col]]
            
            if(input$avoid_negative){
              if (var %in% c("GPP", "TR")) yData[yData < 0] <- NA
            }
            
            m_row <- metrics_df[metrics_df$Measurement == col, ]
            rmse_str <- if (nrow(m_row) > 0 && !is.na(m_row$RMSE)) sprintf("RMSE: %.2f", m_row$RMSE) else "RMSE: NA"
            bias_str <- if (nrow(m_row) > 0 && !is.na(m_row$BIAS)) sprintf("Bias: %.2f", m_row$BIAS) else "Bias: NA"
            corr_str <- if (nrow(m_row) > 0 && !is.na(m_row$Correlation)) sprintf("R2: %.2f", m_row$Correlation) else "R2: NA"
            metric_label <- paste(rmse_str, bias_str, corr_str, sep = " | ")
            meas_lbl <- paste0(col, " Meas\n", metric_label)
            
            meas_df <- data.frame(Date = df_filtered$Date, Value = yData)
            
            # Use integer shape
            current_shape <- as.integer(custom$meas_marker_type)
            if (is.na(current_shape)) current_shape <- 16
            
            # Map both color and shape. DO NOT map linetype for points.
            p <- p + geom_point(data = meas_df,
                                aes(x = Date, y = Value, color = meas_lbl, shape = meas_lbl),
                                size = custom$meas_marker_size)
            
            manual_colors[meas_lbl] <- custom$meas_marker_color
            manual_linetypes[meas_lbl] <- "blank" 
            manual_shapes[meas_lbl] <- current_shape
          }
        }
        
        # 5. Planting Dates
        planting_dates <- rv$epc_dates
        if (!is.null(planting_dates) && nrow(planting_dates) > 0) {
          selected_planting <- planting_dates %>%
            dplyr::filter(lubridate::year(DATE) %in% selectedYears)
          
          if (nrow(selected_planting) > 0) {
            labels_df <- data.frame(Date = selected_planting$DATE, Label = "", StringsAsFactors = FALSE)
            for (i in 1:nrow(selected_planting)) {
              current_epcs <- unlist(strsplit(selected_planting$`CROP(file)`[i], " +"))
              if (input$singleYear || length(selectedYears) <= 3) {
                epc_labels <- sapply(current_epcs, function(epc) {
                  idx <- which(rv$epc_files == epc)
                  if (length(idx) > 0) rv$epc_labels[idx] else epc
                })
                labels_df$Label[i] <- paste(unique(epc_labels), collapse = ", ")
              } else {
                epc_numbers <- sapply(current_epcs, function(epc) {
                  idx <- which(rv$epc_files == epc)
                  if (length(idx) > 0) rv$epc_num_labels[idx] else epc
                })
                labels_df$Label[i] <- paste(unique(epc_numbers), collapse = ", ")
              }
            }
            
            # CHANGE 2: Increased size for Planting Labels (was 3, now 5)
            p <- p + geom_point(data = labels_df, aes(x = Date, y = -Inf), 
                                shape = 25, fill = "#047704", color = "black", size = 3, stroke = 0.5) +
              geom_text(data = labels_df, aes(x = Date, y = -Inf, label = Label),
                        vjust = 2.5, color = "#047704", size = 5) 
              #geom_text(data = labels_df, aes(x = Date, y = -Inf, label = "▼"),
              #          vjust = -0.2, color = "#047704", size = 4)
            p <- p + coord_cartesian(clip = "off") 
          }
        }
        
        # 6. Harvest Dates
        if(input$showHarvest) {
          if ("HarvestDates" %in% names(rv$epc_dates)) {
            selected_harvest <- planting_dates %>%
              dplyr::filter(lubridate::year(HarvestDates) %in% selectedYears)
            
            if (nrow(selected_harvest) > 0) {
              h_labels_df <- data.frame(Date = selected_harvest$HarvestDates, Label = "", StringsAsFactors = FALSE)
              for (i in 1:nrow(selected_harvest)) {
                 current_epcs <- unlist(strsplit(selected_harvest$`CROP(file)`[i], " +"))
                 epc_numbers <- sapply(current_epcs, function(epc) {
                   idx <- which(rv$epc_files == epc)
                   if (length(idx) > 0) rv$epc_num_labels[idx] else epc
                 })
                 h_labels_df$Label[i] <- paste(unique(epc_numbers), collapse = ", ")
              }
              
              # CHANGE 2: Increased size for Harvest Labels (was 3, now 5)
              p <- p + geom_point(data = h_labels_df, aes(x = Date, y = -Inf), 
                                  shape = 24, fill = "#6c4a00", color = "black", size = 3, stroke = 0.5) +
                geom_text(data = h_labels_df, aes(x = Date, y = -Inf, label = Label),
                          vjust = 2.5, color = "#6c4a00", size = 5) 
                # geom_text(data = h_labels_df, aes(x = Date, y = -Inf, label = "▲"),
                #          vjust = -0.2, color = "#6c4a00", size = 4)
            }
          }
        }
        
        # 7. Phenophases
        if(input$showPheno) {
          if("n_actphen" %in% colnames(outputData())){
            sim_df <- outputData()
            sim_df <- sim_df %>%
              dplyr::arrange(Date) %>% 
              dplyr::mutate(prev_phase = dplyr::lag(n_actphen, default = dplyr::first(n_actphen)),
                            phase_change = n_actphen != prev_phase) %>%
              dplyr::filter(phase_change) %>%
              dplyr::select(Date, n_actphen)
            
            transition_df <- sim_df %>%
              dplyr::filter(lubridate::year(Date) %in% selectedYears, n_actphen != 0)
            
            if(nrow(transition_df) > 0){
              # CHANGE 3: Changed vjust to -1 (moves text higher up, outside plot) 
              # and increased size to 5
              p <- p + geom_vline(data = transition_df, aes(xintercept = Date),
                                  linetype = "dotted", color = "#047704", linewidth = 0.5) +
                geom_text(data = transition_df, aes(x = Date, y = Inf, label = n_actphen),
                          vjust = -0.2, color = "#047704", size = 5) 
            }
          } else {
            myShowNotification("Variable n_actphen not found", type = "error")
          }
        }

        # Apply Formatting
        date_format_str <- "%Y-%m-%d"
        date_break_str <- "1 year"
        if (input$singleYear || length(selectedYears) == 1) {
          date_format_str <- "%b %Y"; date_break_str <- "1 month"
        } else if (length(selectedYears) <= 3) {
          date_format_str <- "%b %Y"; date_break_str <- "2 months"
        } else {
          date_format_str <- "%Y"
          # 2) Dynamic date breaks for long time series
          n_years <- length(selectedYears)
          if(n_years > 30) {
             date_break_str <- "5 years"
          } else if (n_years > 15) {
             date_break_str <- "2 years"
          } else {
             date_break_str <- "1 year"
          }
        }
        
        p <- p + scale_x_date(date_labels = date_format_str, 
                              date_breaks = date_break_str, 
                              limits = range(filteredDates, na.rm=TRUE),
                              expand = c(0, 0)) +
          # Remove X axis titles
          labs(y = var, x = NULL) +
          # 1) Use padded labels to ensure alignment on left side
          scale_y_continuous(labels = pad_labels)
        
        if (!is.null(custom$y_min) && !is.null(custom$y_max)) {
           p <- p + coord_cartesian(ylim = c(custom$y_min, custom$y_max), clip = "off") 
        } else {
           p <- p + coord_cartesian(clip = "off") 
        }

      } 
      
      # --- CRITICAL FIX FOR MANUAL SCALES ---
      if (length(manual_colors) > 0) {
        # Debugging Output to Console
        # print("--- Debugging Plot Scales ---")
        # print("Manual Colors:")
        # print(manual_colors)
        # print("Manual Linetypes:")
        # print(manual_linetypes)
        # print("Manual Shapes:")
        # print(manual_shapes)
        
        # 1. Define strictly unique ordered labels based on what was collected
        ordered_labels <- names(manual_colors)
        
        # 2. Extract strictly matching vectors for overrides
        #    Using unname() prevents label mismatches in the guide construction
        override_linetypes <- unname(manual_linetypes[ordered_labels])
        override_shapes    <- unname(manual_shapes[ordered_labels])
        
        # 3. Create a single robust guide object attached ONLY to color
        #    We hide the other guides to prevent merging conflicts
        unified_guide <- guide_legend(
          override.aes = list(
            linetype = override_linetypes,
            shape = override_shapes,
            color = unname(manual_colors[ordered_labels]) # Explicitly set color too
          ),
          nrow = 1,
          order = 1
        )
        
        # 4. Apply scales with specific guide settings
        #    We set guide = "none" for linetype and shape so ggplot doesn't create separate legends
        #    or try to merge them incorrectly. We use the color guide as the "Master" legend.
        p <- p + 
          scale_color_manual(values = manual_colors, breaks = ordered_labels, name = "", guide = unified_guide) +
          scale_linetype_manual(values = manual_linetypes, breaks = ordered_labels, name = "", guide = "none") +
          scale_shape_manual(values = manual_shapes, breaks = ordered_labels, name = "", na.value = NA, guide = "none")
      }

      return(p)
      
    }) 
  })
})

    ##### SPECIAL PLOTS ###########
    # Special plots pop up window
    popup_visible <- reactiveVal(FALSE)
    has_checked_vars <- reactiveVal(FALSE)

    # # check to warn the user about variables
    observeEvent(input$show_popup, {
    
        if (popup_visible() == FALSE) {
            
            if (has_checked_vars() == FALSE) {
                
                req(outputData()) 
                
                model_data <- outputData()
                all_req_vars <- c("yieldc", "leafc", "frootc", "softstemc", "STDBc_above", "harvestIndex")
                available_vars <- colnames(model_data)
                missing_vars <- setdiff(all_req_vars, available_vars)
                
                if (length(missing_vars) > 0) {
                    showNotification(
                        paste("Warning: Missing plot variables (plot will be made without them):", paste(missing_vars, collapse = ", ")),
                        type = "warning",
                        duration = 20 
                    )
                }
                
                has_checked_vars(TRUE)
            }
        }
        
        popup_visible(!popup_visible())
    })

    plot_settings <- reactiveValues(
        yieldcColor = "darkgoldenrod2",
        leafcColor = "#6fd76f",
        frootcColor = "brown",
        softstemcColor = "#fa0000",
        stdbmassColor = "black",
        harvestIndexColor = "blue", 
        
        harvestIndexShape = 19,     
        harvestIndexSize = 2.5,     
        
        yieldcWidth = 0.7,
        leafcWidth = 0.7,
        frootcWidth = 0.7,
        softstemcWidth = 0.7,
        stdbmassWidth = 0.7,
        
        yieldcLinetype = "solid",
        leafcLinetype = "solid",
        frootcLinetype = "solid",
        softstemcLinetype = "solid",
        stdbmassLinetype = "solid",
        
        xaxisTextSize = 1.4,
        yaxisTitleSize = 1.2,
        yaxisTextSize = 1.4,
        legendTextSize = 1.2,
        legendTitleSize = 1.2,

        yAxisLimToggle = FALSE,
        yAxisLimMin = -1,
        yAxisLimMax = 1
    )

    # observeEvent(input$show_popup, {
    #     # Toggles the reactive value between TRUE and FALSE
    #     popup_visible(!popup_visible())
    # })

    observeEvent(input$close_popup, {
        # Sets the reactive value to FALSE
        popup_visible(FALSE)
    })

    observe({
        if (popup_visible()) {
            shinyjs::show("popup_wrapper")
        } else {
            shinyjs::hide("popup_wrapper")
        }
    })


    observeEvent(input$toggle_custom_plot, {
        shinyjs::toggle(id = "custom_plot_panel", anim = TRUE)
    })
    
    linetype_choices <- c(
        "Solid" = "solid",
        "Dashed" = "dashed",
        "Dotted" = "dotted",
        "Dot-Dash" = "dotdash",
        "Long Dash" = "longdash",
        "Two Dash" = "twodash"
    )


    output$dynamic_content_area <- renderUI({
        req(popup_visible())
        
        tagList(
            # Customize button 
            div(style = "display: flex; justify-content: flex-end; padding-bottom: 5px;",
                actionLink("toggle_custom_plot", "Customize Plot", icon = icon("cog"))
            ),
            
            # customisation panel
            shinyjs::hidden(
                div(
                    id = "custom_plot_panel",
                    style = "background: #fdfdfd; border: 1px solid #eee; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
                    
                    h4(style="margin-top:0;", "Plot Customization"),
                    
                    #  Colors 
                    h5("Colors"),
                    fluidRow(
                        # Use the `plot_settings` for default values 
                        column(4, colourpicker::colourInput("yieldcColor", "YieldC", value = plot_settings$yieldcColor)),
                        column(4, colourpicker::colourInput("leafcColor", "LeafC", value = plot_settings$leafcColor)),
                        column(4, colourpicker::colourInput("frootcColor", "FRootC", value = plot_settings$frootcColor)),
                        column(4, colourpicker::colourInput("softstemcColor", "SoftStemC", value = plot_settings$softstemcColor)),
                        column(4, colourpicker::colourInput("stdbmassColor", "StDbMass", value = plot_settings$stdbmassColor)),
                        column(4, colourpicker::colourInput("harvestIndexColor", "Harvest Index", value = plot_settings$harvestIndexColor))
                    ),
                    hr(),
                    
                    # Line Widths & Point Styles 
                    h5("Line & Point Styles"),
                    fluidRow(
                        # Line Widths
                        column(6,
                            numericInput("yieldcWidth", "YieldC Width", value = plot_settings$yieldcWidth, min=0.1, max=5, step=0.1),
                            numericInput("leafcWidth", "LeafC Width", value = plot_settings$leafcWidth, min=0.1, max=5, step=0.1),
                            numericInput("frootcWidth", "FRootC Width", value = plot_settings$frootcWidth, min=0.1, max=5, step=0.1),
                            numericInput("softstemcWidth", "SoftStemC Width", value = plot_settings$softstemcWidth, min=0.1, max=5, step=0.1),
                            numericInput("stdbmassWidth", "StDbMass Width", value = plot_settings$stdbmassWidth, min=0.1, max=5, step=0.1)
                        ),
                        # Line Types
                        column(6,
                            selectInput("yieldcLinetype", "YieldC Type", linetype_choices, selected = plot_settings$yieldcLinetype),
                            selectInput("leafcLinetype", "LeafC Type", linetype_choices, selected = plot_settings$leafcLinetype),
                            selectInput("frootcLinetype", "FRootC Type", linetype_choices, selected = plot_settings$frootcLinetype),
                            selectInput("softstemcLinetype", "SoftStemC Type", linetype_choices, selected = plot_settings$softstemcLinetype),
                            selectInput("stdbmassLinetype", "StDbMass Type", linetype_choices, selected = plot_settings$stdbmassLinetype)
                        )
                    ),
                    fluidRow(
                        # Point Styles
                        column(6, numericInput("harvestIndexShape", "HI Shape", value = plot_settings$harvestIndexShape, min=0, max=25, step=1)),
                        column(6, numericInput("harvestIndexSize", "HI Size", value = plot_settings$harvestIndexSize, min=0, max=10, step=0.1))
                    ),
                    hr(),
                    
                    # Text Sizes
                    h5("Text Sizes"),
                    fluidRow(
                        column(4, numericInput("xaxisTextSize", "X-Axis", value = plot_settings$xaxisTextSize, min=0.1, max=3, step=0.1)),
                        column(4, numericInput("yaxisTitleSize", "Y-Title", value = plot_settings$yaxisTitleSize, min=0.1, max=3, step=0.1)),
                        column(4, numericInput("yaxisTextSize", "Y-Axis", value = plot_settings$yaxisTextSize, min=0.1, max=3, step=0.1)),
                        column(6, numericInput("legendTextSize", "Legend Text", value = plot_settings$legendTextSize, min=0.1, max=3, step=0.1)),
                        column(6, numericInput("legendTitleSize", "Legend Title", value = plot_settings$legendTitleSize, min=0.1, max=3, step=0.1))
                    ),
                    hr(),
                    h5("Y-Axis Range"),
                    fluidRow(
                        column(12, checkboxInput("yAxisLimToggle", "Set Custom Y-Axis Range", value = plot_settings$yAxisLimToggle))
                    ),
                    # This conditional UI will only show if the box is checked.
                    conditionalPanel(
                        condition = "input.yAxisLimToggle == true",
                        fluidRow(
                            column(6, numericInput("yAxisLimMin", "Y-Min", value = plot_settings$yAxisLimMin)),
                            column(6, numericInput("yAxisLimMax", "Y-Max", value = plot_settings$yAxisLimMax))
                        )
                    ),
                    hr(),
                    
                    # Apply & Save Buttons
                    h5("Actions"),
                    fluidRow(
                        #Apply Button
                        column(6,
                            actionButton("apply_custom_plot", "Apply Changes", icon = icon("check"), width="100%", class = "btn-primary")
                        ),
                        column(6,
                            actionButton("save_popup_plot", "Save as PNG", icon = icon("download"), width="100%")
                        )
                    ),
                    
                    h5("Save Dimensions (inches)"),
                    fluidRow(
                        column(6, numericInput("save_width", "Width", 10, min=1, max=30, step=0.5)),
                        column(6, numericInput("save_height", "Height", 8, min=1, max=30, step=0.5))
                    )
                )
            ), # end shinyjs::hidden
        
            # This is the original plot output
            plotOutput("popup_plot", height = "380px")
        )
    })

    plot_years <- reactive({
      req(!is.null(input$singleYear), input$yearRange) # Wait for year inputs
      
      local_years_to_plot <- NULL # Initialize
      
      if (isTRUE(input$singleYear)) {
          req(is.finite(input$yearRange))
          local_years_to_plot <- input$yearRange
      } else {
          req(length(input$yearRange) == 2 &&
              is.finite(input$yearRange[1]) &&
              is.finite(input$yearRange[2]))
          local_years_to_plot <- list(c(input$yearRange[1], input$yearRange[2]))
      }
      return(local_years_to_plot)
    })

       observeEvent(input$apply_custom_plot, {
        
        # Show a quick notification
        #showNotification("Applying plot settings...", type = "message", duration = 2)
        
        # Update all values in the reactiveValues snapshot
        plot_settings$yieldcColor <- input$yieldcColor
        plot_settings$leafcColor <- input$leafcColor
        plot_settings$frootcColor <- input$frootcColor
        plot_settings$softstemcColor <- input$softstemcColor
        plot_settings$stdbmassColor <- input$stdbmassColor
        plot_settings$harvestIndexColor <- input$harvestIndexColor
        
        plot_settings$harvestIndexShape <- input$harvestIndexShape
        plot_settings$harvestIndexSize <- input$harvestIndexSize
        
        plot_settings$yieldcWidth <- input$yieldcWidth
        plot_settings$leafcWidth <- input$leafcWidth
        plot_settings$frootcWidth <- input$frootcWidth
        plot_settings$softstemcWidth <- input$softstemcWidth
        plot_settings$stdbmassWidth <- input$stdbmassWidth
        
        plot_settings$yieldcLinetype <- input$yieldcLinetype
        plot_settings$leafcLinetype <- input$leafcLinetype
        plot_settings$frootcLinetype <- input$frootcLinetype
        plot_settings$softstemcLinetype <- input$softstemcLinetype
        plot_settings$stdbmassLinetype <- input$stdbmassLinetype
        
        plot_settings$xaxisTextSize <- input$xaxisTextSize
        plot_settings$yaxisTitleSize <- input$yaxisTitleSize
        plot_settings$yaxisTextSize <- input$yaxisTextSize
        plot_settings$legendTextSize <- input$legendTextSize
        plot_settings$legendTitleSize <- input$legendTitleSize

        plot_settings$yAxisLimToggle <- input$yAxisLimToggle
        plot_settings$yAxisLimMin <- input$yAxisLimMin
        plot_settings$yAxisLimMax <- input$yAxisLimMax
    })

   output$popup_plot <- renderPlot({
      
      req(outputData(), plot_years())
      
      model_data <- outputData()
      local_years_to_plot <- plot_years()

    local_yAxisLim <- NULL
      if (isTRUE(plot_settings$yAxisLimToggle) &&
          is.numeric(plot_settings$yAxisLimMin) &&
          is.numeric(plot_settings$yAxisLimMax)) {
        local_yAxisLim <- c(plot_settings$yAxisLimMin, plot_settings$yAxisLimMax)
      }

      musoPlotHarvestIndexBiomass(
        modelResult = model_data,
        yearsToPlot = local_years_to_plot,
        yAxisLim = local_yAxisLim,
        silent = TRUE,
        
        # Pass all the *applied* values
        yieldcColor = plot_settings$yieldcColor,
        leafcColor = plot_settings$leafcColor,
        frootcColor = plot_settings$frootcColor,
        softstemcColor = plot_settings$softstemcColor,
        stdbmassColor = plot_settings$stdbmassColor,
        harvestIndexColor = plot_settings$harvestIndexColor,
        
        harvestIndexShape = plot_settings$harvestIndexShape,
        harvestIndexSize = plot_settings$harvestIndexSize,
        
        yieldcWidth = plot_settings$yieldcWidth,
        leafcWidth = plot_settings$leafcWidth,
        frootcWidth = plot_settings$frootcWidth,
        softstemcWidth = plot_settings$softstemcWidth,
        stdbmassWidth = plot_settings$stdbmassWidth,
        
        yieldcLinetype = plot_settings$yieldcLinetype,
        leafcLinetype = plot_settings$leafcLinetype,
        frootcLinetype = plot_settings$frootcLinetype,
        softstemcLinetype = plot_settings$softstemcLinetype,
        stdbmassLinetype = plot_settings$stdbmassLinetype,
        
        xaxisTextSize = plot_settings$xaxisTextSize,
        yaxisTitleSize = plot_settings$yaxisTitleSize,
        yaxisTextSize = plot_settings$yaxisTextSize,
        legendTextSize = plot_settings$legendTextSize,
        legendTitleSize = plot_settings$legendTitleSize
      )
     
    })
    
    # plot saving
    observeEvent(input$save_popup_plot, {
      req(outputData(), plot_years(), input$save_width, input$save_height)
      
      showNotification("Saving plot...", type = "message", duration = 3)
      
      model_data <- outputData()
      local_years_to_plot <- plot_years()

        local_yAxisLim <- NULL
      if (isTRUE(plot_settings$yAxisLimToggle) &&
          is.numeric(plot_settings$yAxisLimMin) &&
          is.numeric(plot_settings$yAxisLimMax)) {
        local_yAxisLim <- c(plot_settings$yAxisLimMin, plot_settings$yAxisLimMax)
      }

      musoPlotHarvestIndexBiomass(
        modelResult = model_data,
        yearsToPlot = local_years_to_plot,

        yAxisLim = local_yAxisLim,
        
        saveOutput = TRUE,
        width = input$save_width,
        height = input$save_height,
        silent = FALSE, 
        
        yieldcColor = plot_settings$yieldcColor,
        leafcColor = plot_settings$leafcColor,
        frootcColor = plot_settings$frootcColor,
        softstemcColor = plot_settings$softstemcColor,
        stdbmassColor = plot_settings$stdbmassColor,
        harvestIndexColor = plot_settings$harvestIndexColor,
        
        harvestIndexShape = plot_settings$harvestIndexShape,
        harvestIndexSize = plot_settings$harvestIndexSize,
        
        yieldcWidth = plot_settings$yieldcWidth,
        leafcWidth = plot_settings$leafcWidth,
        frootcWidth = plot_settings$frootcWidth,
        softstemcWidth = plot_settings$softstemcWidth,
        stdbmassWidth = plot_settings$stdbmassWidth,
        
        yieldcLinetype = plot_settings$yieldcLinetype,
        leafcLinetype = plot_settings$leafcLinetype,
        frootcLinetype = plot_settings$frootcLinetype,
        softstemcLinetype = plot_settings$softstemcLinetype,
        stdbmassLinetype = plot_settings$stdbmassLinetype,
        
        xaxisTextSize = plot_settings$xaxisTextSize,
        yaxisTitleSize = plot_settings$yaxisTitleSize,
        yaxisTextSize = plot_settings$yaxisTextSize,
        legendTextSize = plot_settings$legendTextSize,
        legendTitleSize = plot_settings$legendTitleSize
      )
      
      showNotification("Plot saved!", type = "message", duration = 5)
    })


    observeEvent(input$getOriginalIni,{
                     updateTextAreaInput(session, "inifile", value=paste(readLines("bck/n.ini"),
                                                                                              collapse="\n") )
    })


        # messaging
        get_remote_message <- function() {
  
            base_url <- "https://raw.githubusercontent.com/Cyb3rNani/tuneMessage/refs/heads/main/message.json"
            
            url <- paste0(base_url, "?t=", as.numeric(Sys.time()))
            tryCatch({
                # Fetch the JSON file
                response <- httr::GET(url, httr::user_agent("Shiny App"))
                
                # Check if the request was successful
                if (httr::http_status(response)$category != "Success") {
                return(paste("Error: Failed to fetch JSON. HTTP Status:", httr::http_status(response)$message))
                }
                
                # Parse the JSON content
                json_data <- jsonlite::fromJSON(httr::content(response, "text"))
                
                # Check if the "message" key exists
                if (!is.null(json_data$message)) {
                return(json_data$message)
                } else {
                return("Error: 'message' key not found in JSON.")
                }
            }, error = function(e) {
                return(paste("Error retrieving notification:", e$message))
            })
        }

        # Function to send user feedback to google sheets
        send_feedback <- function(message) {
        url <- "https://script.google.com/macros/s/AKfycbyeApSnxnbQXB-m0Ziw24cdhZIK0pm8FBJFLCHC-OfO_ABOJwWlHy788FuKYDKYi_U7Ng/exec"
            tryCatch({
                response <- httr::POST(
                url,
                body = list(message = message),
                encode = "json",
                #httr::verbose() # Enable to debug request/response
                )
                # Check if the request was successful
                if (httr::status_code(response) == 200) {
                    
                    showNotification("Feedback successfully sent", type = "message")
                    success <- "Feedback submitted successfully!"

                } else {
                    showNotification("Failed to send feedback", type = "error")
                    success <- paste("Error: HTTP", httr::status_code(response))
                }

                return(success)
            }, error = function(e) {
                showNotification("Error in sending the feedback", type = "error")
                return(paste("Error submitting feedback:", e$message))
                
            })
        }

      notification_message <- get_remote_message()
    session$sendCustomMessage("checkNotification", notification_message)
        
        
       observeEvent(input$show_notification, {
            showModal(modalDialog(
            title = "Notification",
            notification_message,
            easyClose = TRUE,
            footer = modalButton("Close")
            ))
            # Mark the notification as read by updating localStorage and removing the badge (it actually works let's go!)
            safe_message <- gsub("'", "\\\\'", notification_message)  # Escape single quotes
            runjs(sprintf("localStorage.setItem('last_notification', '%s'); $('#show_notification').removeClass('new-notification'); $('#badge').remove();", safe_message))
        })
        
        
        observeEvent(input$submit_feedback, {
            if (input$feedback_message != "") {
            result <- send_feedback(input$feedback_message)
            output$feedback_status <- renderText({ result })
            } else {
            output$feedback_status <- renderText("Please enter a message before submitting.")
            }
        })


         observeEvent(input$exit, {
            confirmSweetAlert(
            session = session,
            inputId = "confirm_exit",
            title = "Are you sure?",
            text = "Do you really want to exit tuneMuso? Your plants will miss you...",
            type = "warning",
            btn_labels = c("No", "Yes"),
            danger_mode = TRUE
            )
        })
        
        observeEvent(input$confirm_exit, {
            if (input$confirm_exit) {
                stopApp()
                runjs("window.close();")  
            }
        })
    
}




#' tuneMusoTest
#'
#' launchApp launch the shiny app
#' @param ... Other parameters for shinyApp function
#' @importFrom shiny shinyApp shinyOptions
#' @export
tuneMuso2 <- function(directory = NULL, ...){ 
    shinyOptions(workdir = getwd())
    if(is.null(directory)){
        shinyOptions(musoRoot = ".")
    } else {
        shinyOptions(musoRoot = normalizePath(directory))
    }
    shinyApp(ui = tuneMusoUI2(), server = tuneMusoServer2, options = c(list(launch.browser = TRUE), list(...)))
    #shinyApp(ui = tuneMusoUI(), server = tuneMusoServer, options = list(...))
}

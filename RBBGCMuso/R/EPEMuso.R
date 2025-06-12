#' EPEMuso
#' 
#' With this you can view the profiles of the variables in the endpoint file and optionally edit them 
#'
#' @importFrom shiny tags actionButton numericInput HTML checkboxInput titlePanel radioButtons textAreaInput fluidPage sidebarLayout sidebarPanel mainPanel getShinyOption tabsetPanel tabPanel tagList selectInput sliderInput renderUI div fileInput uiOutput updateSliderInput observe observeEvent validate need showNotification icon textInput isRunning reactiveVal reactiveValues isolate debounce bindEvent fluidRow column checkboxGroupInput showModal modalDialog modalButton removeModal h4 downloadButton downloadHandler verbatimTextOutput onFlushed stopApp conditionalPanel eventReactive hr p plotOutput reactive renderPlot renderText req updateNumericInput
#' @importFrom shinyjs useShinyjs disable enable runjs
#' @usage ...
#' @export 

EPEui <- function() { 
  setwd(getShinyOption("musoRoot"))
  workdir <- getwd()
  
  fluidPage(
    tags$head(
      tags$title("BBGCMuso Endpoint Profile Editor")
    ),
    useShinyjs(),
    titlePanel(
      div(
        style = "display: flex; align-items: center; margin-left: 0px;",
        tags$span("Endpoint Profile Editor (BBGCMuso Alpha)", style = "font-size: 24px; font-weight: bold; margin-right: 20px;"),
        tags$span(paste0(workdir), style = "font-size: 14px; color: #666;")
      )
    ),
    tags$head(tags$style(HTML("
      .btn-xs { padding: 1px 5px; font-size: 12px; line-height: 1.5; border-radius: 3px; } 
      .numeric-input-xs .form-group { margin-bottom: 2px !important; } 
      .numeric-input-xs label {font-size: 11px; margin-bottom: 1px;} 
      .numeric-input-xs input {height: 28px; font-size:12px; padding: 2px 4px;}
      .switch-input-xs .form-group { margin-bottom: 2px !important; }
      .switch-input-xs .shiny-input-container { margin-bottom: 2px !important; }
      .plot-control-row { margin-top: 8px; margin-bottom: 5px; }
      /* Style for DT tables to make them compact */
      .compact-dt table.dataTable th, .compact-dt table.dataTable td { padding: 2px 8px !important; }
      .compact-dt .dataTables_scrollBody { max-height: 320px !important; }
      .conservation-panel { border: 1px solid #007bff; padding: 15px; margin-bottom: 15px; border-radius: 8px; background-color: #f8f9fa; }
      .conservation-header { font-weight: bold; font-size: 16px; margin-bottom: 10px; color: #0056b3; }
      .layer-row { display: flex; align-items: center; margin-bottom: 4px; padding: 2px; border-radius: 4px; }
      .layer-label { font-size: 11px; font-weight: bold; width: 130px; }
      .sum-display { font-size: 12px; width: 240px; }
      .deficit-val { font-weight: bold; font-size: 12px; }
      .deficit-green { color: #28a745; }
      .deficit-red { color: #dc3545; }
      .compensate-buttons { margin-left: 20px; }
      .compensate-buttons .btn-xs { margin-left: 5px; }
    "))),
    
    div(
      style = "position: absolute; top: 10px; right: 10px; z-index: 1000;",
            actionButton("exit", "Exit", icon = icon("sign-out"), 
                   style = "color: #fff; background-color: #dc3545; border-color: #dc3545; margin-top: 10px;")
    ),
    
    fluidRow(
      column(4,
             fileInput("file_input", "Upload Binary Endpoint File (.rst)", accept = ".rst", width = "100%")
      ),
      column(8,
             uiOutput("variable_selector_ui") 
      )
    ),
    hr(),
    
    h4("Edit Selected Variable Data:"),
    uiOutput("dynamic_tables_ui"),
    hr(),
    
    h4("Profile Plots:"),
    uiOutput("dynamic_plots_ui"),
    hr(),
    
    fluidRow(
      column(3, 
             downloadButton("download_data_button", "Save Modified to New .rst", class = "btn-primary btn-block")
      ),
      column(3, 
             actionButton("overwrite_wd_button", "Overwrite Original File", class = "btn-danger btn-block", icon = icon("exclamation-triangle")) # Changed label and style
      ),
      column(6, 
             h4("Status / Messages:"),
             verbatimTextOutput("status_output")
      )
    )
  )

}


#' EPEMuso server
#' 
#' Server program for EPEMuso
#'
#' @param input shiny input
#' @param output shiny output
#' @param session dinamic session management for shiny
#' @importFrom shiny reactiveValues isolate observeEvent
#' @importFrom shinyjs useShinyjs disable enable runjs
#' @importFrom shinyWidgets pickerInput pickerOptions switchInput updateSwitchInput confirmSweetAlert
#' @importFrom DT DTOutput renderDT datatable
#' @importFrom data.table as.data.table
#' @importFrom ggplot2 ggplot aes geom_line labs scale_x_continuous scale_y_continuous theme_minimal theme element_blank element_text geom_hline geom_path geom_point scale_y_reverse xlab ggtitle element_rect coord_cartesian ggsave
#' @importFrom grid unit
#' @usage ...
#' @export 
EPEserver <- function(input, output, session) {
  
  setwd(getShinyOption("musoRoot")) 
  workdir <- getwd()
  # Variable definitions for the Muso endpoint binary
  variable_defs <- list(
    soilw = list(name = "soilw [kgH2O/m2]", indices = 0:9, type = "double"),
    snoww = list(name = "snoww", indices = 10, type = "double"),
    canopyw = list(name = "canopyw", indices = 11, type = "double"),
    leafc = list(name = "leafc", indices = 12, type = "double"),
    leafc_storage = list(name = "leafc_storage", indices = 13, type = "double"),
    leafc_transfer = list(name = "leafc_transfer", indices = 14, type = "double"),
    frootc = list(name = "frootc", indices = 15, type = "double"),
    frootc_storage = list(name = "frootc_storage", indices = 16, type = "double"),
    frootc_transfer = list(name = "frootc_transfer", indices = 17, type = "double"),
    yield = list(name = "yield", indices = 18, type = "double"),
    yieldc_storage = list(name = "yieldc_storage", indices = 19, type = "double"),
    yieldc_transfer = list(name = "yieldc_transfer", indices = 20, type = "double"),
    softstemc = list(name = "softstemc", indices = 21, type = "double"),
    softstemc_storage = list(name = "softstemc_storage", indices = 22, type = "double"),
    softstemc_transfer = list(name = "softstemc_transfer", indices = 23, type = "double"),
    livestemc = list(name = "livestemc", indices = 24, type = "double"),
    livestemc_storage = list(name = "livestemc_storage", indices = 25, type = "double"),
    livestemc_transfer = list(name = "livestemc_transfer", indices = 26, type = "double"),
    deadstemc = list(name = "deadstemc", indices = 27, type = "double"),
    deadstemc_storage = list(name = "deadstemc_storage", indices = 28, type = "double"),
    deadstemc_transfer = list(name = "deadstemc_transfer", indices = 29, type = "double"),
    livecrootc = list(name = "livecrootc", indices = 30, type = "double"),
    livecrootc_storage = list(name = "livecrootc_storage", indices = 31, type = "double"),
    livecrootc_transfer = list(name = "livecrootc_transfer", indices = 32, type = "double"),
    deadcrootc = list(name = "deadcrootc", indices = 33, type = "double"),
    deadcrootc_storage = list(name = "deadcrootc_storage", indices = 34, type = "double"),
    deadcrootc_transfer = list(name = "deadcrootc_transfer", indices = 35, type = "double"),
    gresp_storage = list(name = "gresp_storage", indices = 36, type = "double"),
    gresp_transfer = list(name = "gresp_transfer", indices = 37, type = "double"),
    cwdc = list(name = "cwdc [kgC/m2]", indices = 38:47, type = "double"),
    litr1c = list(name = "litr1c [kgC/m2]", indices = 48:57, type = "double"),
    litr2c = list(name = "litr2c [kgC/m2]", indices = 58:67, type = "double"),
    litr3c = list(name = "litr3c [kgC/m2]", indices = 68:77, type = "double"),
    litr4c = list(name = "litr4c [kgC/m2]", indices = 78:87, type = "double"),
    STDBc_leaf = list(name = "STDBc_leaf", indices = 88, type = "double"),
    STDBc_froot = list(name = "STDBc_froot", indices = 89, type = "double"),
    STDBc_yield = list(name = "STDBc_yield", indices = 90, type = "double"),
    STDBc_softstem = list(name = "STDBc_softstem", indices = 91, type = "double"),
    CTDBc_leaf = list(name = "CTDBc_leaf", indices = 92, type = "double"),
    CTDBc_froot = list(name = "CTDBc_froot", indices = 93, type = "double"),
    CTDBc_yield = list(name = "CTDBc_yield", indices = 94, type = "double"),
    CTDBc_softstem = list(name = "CTDBc_softstem", indices = 95, type = "double"),
    CTDBc_cstem = list(name = "CTDBc_cstem", indices = 96, type = "double"),
    CTDBc_croot = list(name = "CTDBc_croot", indices = 97, type = "double"),
    soil1c = list(name = "soil1c [kgC/m2]", indices = 98:107, type = "double"),
    soil2c = list(name = "soil2c [kgC/m2]", indices = 108:117, type = "double"),
    soil3c = list(name = "soil3c [kgC/m2]", indices = 118:127, type = "double"),
    soil4c = list(name = "soil4c [kgC/m2]", indices = 128:137, type = "double"),
    cpool = list(name = "cpool", indices = 138, type = "double"),
    leafn = list(name = "leafn", indices = 139, type = "double"),
    leafn_storage = list(name = "leafn_storage", indices = 140, type = "double"),
    leafn_transfer = list(name = "leafn_transfer", indices = 141, type = "double"),
    frootn = list(name = "frootn", indices = 142, type = "double"),
    frootn_storage = list(name = "frootn_storage", indices = 143, type = "double"),
    frootn_transfer = list(name = "frootn_transfer", indices = 144, type = "double"),
    yieldn = list(name = "yieldn", indices = 145, type = "double"),
    yieldn_storage = list(name = "yieldn_storage", indices = 146, type = "double"),
    yieldn_transfer = list(name = "yieldn_transfer", indices = 147, type = "double"),
    softstemn = list(name = "softstemn", indices = 148, type = "double"),
    softstemn_storage = list(name = "softstemn_storage", indices = 149, type = "double"),
    softstemn_transfer = list(name = "softstemn_transfer", indices = 150, type = "double"),
    livestemn = list(name = "livestemn", indices = 151, type = "double"),
    livestemn_storage = list(name = "livestemn_storage", indices = 152, type = "double"),
    livestemn_transfer = list(name = "livestemn_transfer", indices = 153, type = "double"),
    deadstemn = list(name = "deadstemn", indices = 154, type = "double"),
    deadstemn_storage = list(name = "deadstemn_storage", indices = 155, type = "double"),
    deadstemn_transfer = list(name = "deadstemn_transfer", indices = 156, type = "double"),
    livecrootn = list(name = "livecrootn", indices = 157, type = "double"),
    livecrootn_storage = list(name = "livecrootn_storage", indices = 158, type = "double"),
    livecrootn_transfer = list(name = "livecrootn_transfer", indices = 159, type = "double"),
    deadcrootn = list(name = "deadcrootn", indices = 160, type = "double"),
    deadcrootn_storage = list(name = "deadcrootn_storage", indices = 161, type = "double"),
    deadcrootn_transfer = list(name = "deadcrootn_transfer", indices = 162, type = "double"),
    cwdn = list(name = "cwdn [kgN/m2]", indices = 163:172, type = "double"),
    litr1n = list(name = "litr1n [kgN/m2]", indices = 173:182, type = "double"),
    litr2n = list(name = "litr2n [kgN/m2]", indices = 183:192, type = "double"),
    litr3n = list(name = "litr3n [kgN/m2]", indices = 193:202, type = "double"),
    litr4n = list(name = "litr4n [kgN/m2]", indices = 203:212, type = "double"),
    litrCabove = list(name = "litrCabove [kgC/m2]", indices = 213:222, type = "double"),
    litrCbelow = list(name = "litrCbelow [kgC/m2]", indices = 223:232, type = "double"),
    cwdCabove = list(name = "cwdCabove [kgC/m2]", indices = 233:242, type = "double"),
    cwdCbelow = list(name = "cwdCbelow [kgC/m2]", indices = 243:252, type = "double"),
    STDBn_leaf = list(name = "STDBn_leaf", indices = 253, type = "double"),
    STDBn_froot = list(name = "STDBn_froot", indices = 254, type = "double"),
    STDBn_yield = list(name = "STDBn_yield", indices = 255, type = "double"),
    STDBn_softstem = list(name = "STDBn_softstem", indices = 256, type = "double"),
    CTDBn_leaf = list(name = "CTDBn_leaf", indices = 257, type = "double"),
    CTDBn_froot = list(name = "CTDBn_froot", indices = 258, type = "double"),
    CTDBn_yield = list(name = "CTDBn_yield", indices = 259, type = "double"),
    CTDBn_softstem = list(name = "CTDBn_softstem", indices = 260, type = "double"),
    CTDBn_cstem = list(name = "CTDBn_cstem", indices = 261, type = "double"),
    CTDBn_croot = list(name = "CTDBn_croot", indices = 262, type = "double"),
    soil1n = list(name = "soil1n [kgN/m2]", indices = 263:272, type = "double"),
    soil2n = list(name = "soil2n [kgN/m2]", indices = 273:282, type = "double"),
    soil3n = list(name = "soil3n [kgN/m2]", indices = 283:292, type = "double"),
    soil4n = list(name = "soil4n [kgN/m2]", indices = 293:302, type = "double"),
    retransn = list(name = "retransn", indices = 303, type = "double"),
    npool = list(name = "npool", indices = 304, type = "double"),
    NH4 = list(name = "NH4 [kgN/m2]", indices = 305:314, type = "double"),
    NO3 = list(name = "NO3 [kgN/m2]", indices = 315:324, type = "double"),
    annmax_leafc = list(name = "annmax_leafc", indices = 325, type = "double"),
    annmax_frootc = list(name = "annmax_frootc", indices = 326, type = "double"),
    annmax_yieldc = list(name = "annmax_yieldc", indices = 327, type = "double"),
    annmax_softstemc = list(name = "annmax_softstemc", indices = 328, type = "double"),
    annmax_livestemc = list(name = "annmax_livestemc", indices = 329, type = "double"),
    annmax_livecrootc = list(name = "annmax_livecrootc", indices = 330, type = "double")
  )
  
  # Define groups of related variables for conservation sum
  variable_groups <- list(
    soilc = list(name = "Soil Carbon Pools", members = c("soil1c", "soil2c", "soil3c", "soil4c")),
    soiln = list(name = "Soil Nitrogen Pools", members = c("soil1n", "soil2n", "soil3n", "soil4n")),
    litrc = list(name = "Litter Carbon Pools", members = c("litr1c", "litr2c", "litr3c", "litr4c")),
    litrn = list(name = "Litter Nitrogen Pools", members = c("litr1n", "litr2n", "litr3n", "litr4n"))
  )
  
  # Helper to find which group a variable belongs to
  find_variable_group <- function(var_name) {
    for (group_name in names(variable_groups)) {
      if (var_name %in% variable_groups[[group_name]]$members) {
        return(group_name)
      }
    }
    return(NULL)
  }
  
  # Plotting constants and Table Row Names
  depth_layers_top_orig <- c(0, 3, 10, 30, 60, 90, 120, 150, 200, 400)
  depth_layers_bottom_orig <- c(3, 10, 30, 60, 90, 120, 150, 200, 400, 1000)
  y_axis_breaks_orig <- c(0, 3, 10, 30, 60, 90, 120, 150, 200, 400, 1000)
  y_axis_breaks_transformed <- sqrt(y_axis_breaks_orig)
  new_depth_midpoints_transformed <- numeric(length(depth_layers_top_orig))
  table_row_names_with_depths <- character(length(depth_layers_top_orig))
  
  for (i in 1:length(depth_layers_top_orig)) {
    top_transformed <- sqrt(depth_layers_top_orig[i])
    bottom_transformed <- sqrt(depth_layers_bottom_orig[i])
    new_depth_midpoints_transformed[i] <- (top_transformed + bottom_transformed) / 2
    table_row_names_with_depths[i] <- paste0("Layer ", i-1, " [", depth_layers_top_orig[i], "-", depth_layers_bottom_orig[i], " cm]")
  }
  
  
  "%||%" <- function(x, y) if (is.null(x)) y else x
  
  
  rv <- reactiveValues(
    full_data_vector = NULL,
    edited_data_vector = NULL, 
    num_values_expected = max(unlist(lapply(variable_defs, function(x) x$indices))) + 1,
    depth_vars_choices = character(0),
    selected_var_names = character(0),
    plot_axis_limits = list(), 
    initialized_vars = character(0), 
    table_update_triggers = reactiveValues(), 
    status_message = "Please upload a binary file to begin.",
    conservation_sums = reactiveValues() # To store initial sums and deficits
  )
  
  plot_data_slicers <- new.env(parent = emptyenv())
  
  
  observeEvent(input$file_input, {
    req(input$file_input)
    file_path <- input$file_input$datapath
    rv$status_message <- paste("Processing file:", input$file_input$name)
    rv$full_data_vector <- NULL
    rv$edited_data_vector <- NULL
    rv$depth_vars_choices <- character(0)
    rv$selected_var_names <- character(0) 
    rv$plot_axis_limits <- list()
    rv$initialized_vars <- character(0) 
    
    # Reset conservation sums
    for(group_name in names(rv$conservation_sums)) {
      rv$conservation_sums[[group_name]] <- NULL
    }
    
    # Reset table update triggers
    for(name in names(rv$table_update_triggers)) {
      rv$table_update_triggers[[name]] <- NULL
    }
    rm(list = ls(envir = plot_data_slicers), envir = plot_data_slicers)
    
    tryCatch({
      con <- file(file_path, "rb")
      data_read <- readBin(con, what = "double", n = rv$num_values_expected, size = 8, endian = "little")
      close(con)
      if (length(data_read) < rv$num_values_expected) {
        rv$status_message <- paste("Error: File is smaller than expected. Read", length(data_read), "values, expected", rv$num_values_expected)
        shinyjs::disable("overwrite_wd_button") 
        return()
      }
      rv$full_data_vector <- data_read
      rv$edited_data_vector <- data_read 
      
      # Pre-calculate initial sums for all groups
      for (group_name in names(variable_groups)) {
        group_info <- variable_groups[[group_name]]
        initial_sum_layers <- rep(0, 10)
        for (member_name in group_info$members) {
          member_indices <- variable_defs[[member_name]]$indices
          initial_sum_layers <- initial_sum_layers + data_read[member_indices + 1]
        }
        rv$conservation_sums[[group_name]] <- list(
          initial = initial_sum_layers,
          deficit = rep(0, 10) # Initially, no deficit
        )
      }
      
      vars_with_10_layers <- sapply(variable_defs, function(var_def) { length(var_def$indices) == 10 })
      rv$depth_vars_choices <- names(variable_defs)[vars_with_10_layers] # Keep original order from variable_defs
      if (length(rv$depth_vars_choices) > 0) {
        rv$status_message <- "File processed. Select variable(s) to plot and edit."
        shinyjs::enable("overwrite_wd_button") 
      } else {
        rv$status_message <- "File processed, but no variables with 10 depth layers found."
        shinyjs::disable("overwrite_wd_button")
      }
    }, error = function(e) {
      rv$status_message <- paste("Error reading binary file:", e$message)
      rv$full_data_vector <- NULL; rv$edited_data_vector <- NULL
      rv$depth_vars_choices <- character(0); rv$selected_var_names <- character(0)
      rv$plot_axis_limits <- list(); rv$initialized_vars <- character(0)
      rm(list = ls(envir = plot_data_slicers), envir = plot_data_slicers)
      shinyjs::disable("overwrite_wd_button")
    })
  })
  
  shinyjs::disable("overwrite_wd_button")
  
  
  output$variable_selector_ui <- renderUI({
    choices <- rv$depth_vars_choices 
    if (length(choices) > 0) {
      pickerInput(
        inputId = "variable_select_picker",
        label = "Select Variable(s) (with 10 depth layers):",
        choices = choices, 
        selected = isolate(rv$selected_var_names), 
        multiple = TRUE,
        options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, selectAllText = "Select All Variables",
                                deselectAllText = "Deselect All Variables", selectedTextFormat = "count > 3",
                                countSelectedText = "{0} Variables Selected", width = "100%")
      )
    } else { p("Upload a valid binary file to see variable options.") }
  })
  
  observeEvent(input$variable_select_picker, {
    rv$selected_var_names <- input$variable_select_picker %||% character(0)
  }, ignoreNULL = FALSE, ignoreInit = TRUE) 
  
  
  output$dynamic_tables_ui <- renderUI({
    req(input$file_input, rv$full_data_vector)
    selected_vars <- rv$selected_var_names
    if (length(selected_vars) == 0) {
      return(p("Select one or more variables to edit their data."))
    }
    
    # Processed vars keeps track of variables already displayed in a group
    processed_vars <- c()
    ui_elements <- list()
    
    for (var_name in selected_vars) {
      if (var_name %in% processed_vars) next
      
      group_name <- find_variable_group(var_name)
      
      if (!is.null(group_name)) {
        # It's a grouped variable
        group_info <- variable_groups[[group_name]]
        group_members <- group_info$members
        
        # UI for conservation sums and buttons
        conservation_ui <- div(class = "conservation-panel",
          div(class = "conservation-header", paste("Conservation Sums for:", group_info$name)),
          # This div will be updated reactively
          uiOutput(paste0("conservation_status_", group_name))
        )
        
        # UI for the tables of all group members
        table_uis <- lapply(group_members, function(member_name) {
          column(3, key = paste0("table_col_", member_name),
                 div(style="border: 1px solid #ddd; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
                     tags$h5(strong(variable_defs[[member_name]]$name %||% member_name)),
                     div(class = "compact-dt", DTOutput(outputId = paste0("table_", member_name))),
                     actionButton(inputId = paste0("reset_", member_name), label = "Reset", icon = icon("undo"), class = "btn-warning btn-xs", style="margin-top: 10px;")
                 )
          )
        })
        
        # Add the entire group UI as one block
        ui_elements[[length(ui_elements) + 1]] <- tagList(
          conservation_ui,
          fluidRow(table_uis)
        )
        
        processed_vars <- c(processed_vars, group_members)
      } else {
        # It's a standalone variable
        ui_elements[[length(ui_elements) + 1]] <- column(4, key = paste0("table_col_", var_name), 
                 div(style="border: 1px solid #ddd; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
                     tags$h5(strong(variable_defs[[var_name]]$name %||% var_name)),
                     div(class = "compact-dt", DTOutput(outputId = paste0("table_", var_name))),
                     actionButton(inputId = paste0("reset_", var_name), label = "Reset Values", icon = icon("undo"), class = "btn-warning btn-xs", style="margin-top: 10px;")
                 )
          )
      }
    }
    
    # Arrange standalone variables in rows
    final_ui_list <- list()
    current_row <- list()
    for (el in ui_elements) {
        # Check if element is a tagList (a group) or a column (standalone)
        if (inherits(el, "shiny.tag.list")) {
            if (length(current_row) > 0) {
                final_ui_list[[length(final_ui_list) + 1]] <- fluidRow(current_row)
                current_row <- list()
            }
            final_ui_list[[length(final_ui_list) + 1]] <- el
        } else {
            current_row[[length(current_row) + 1]] <- el
            if (length(current_row) == 3) {
                final_ui_list[[length(final_ui_list) + 1]] <- fluidRow(current_row)
                current_row <- list()
            }
        }
    }
    if (length(current_row) > 0) {
        final_ui_list[[length(final_ui_list) + 1]] <- fluidRow(current_row)
    }

    tagList(final_ui_list)
  })

  
  output$dynamic_plots_ui <- renderUI({
    req(input$file_input, rv$full_data_vector) 
    if (length(rv$selected_var_names) == 0) {
      return(p("Select one or more variables to display their profile plots."))
    }
    tagList(
      lapply(rv$selected_var_names, function(var_name) {
        is_custom_active_default <- isolate(rv$plot_axis_limits[[var_name]]$is_custom_active %||% FALSE)
        xmin_default <- isolate(rv$plot_axis_limits[[var_name]]$min %||% NA_real_)
        xmax_default <- isolate(rv$plot_axis_limits[[var_name]]$max %||% NA_real_)
        
        column(4, key = paste0("plot_col_", var_name),
               div(style="border: 1px solid #ddd; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
                   plotOutput(outputId = paste0("plot_", var_name), height = "320px"), 
                   div(class="plot-control-row switch-input-xs",
                       switchInput(
                         inputId = paste0("toggle_custom_", var_name), 
                         label = "Custom X-Axis:", value = is_custom_active_default, 
                         onStatus = "primary", offStatus = "default", size = "mini",
                         labelWidth = "100px", handleWidth = "50px", inline = TRUE
                       )
                   ),
                   conditionalPanel(
                     condition = paste0("input['", paste0("toggle_custom_", var_name),"'] == true"), 
                     div(class="plot-control-row numeric-input-xs",
                         fluidRow(
                           column(5, style="padding-right:3px;", numericInput(inputId = paste0("xmin_ui_", var_name), label = "Min", value = xmin_default, width = "100%", step=0.01)),
                           column(5, style="padding-left:3px; padding-right:3px;", numericInput(inputId = paste0("xmax_ui_", var_name), label = "Max", value = xmax_default, width = "100%", step=0.01)),
                           column(2, style="padding-left:3px; display:flex; align-items:flex-end; height: 47px;", 
                                  actionButton(inputId = paste0("apply_limits_", var_name), label = icon("check"), class = "btn-primary btn-xs", style="height:28px; width:100%;"))
                         )
                     )
                   ),
                   downloadButton(outputId = paste0("download_plot_", var_name), label = "Save This Plot", class = "btn-info btn-xs", style="margin-top: 10px; width: 100%;")
               )
        )
      })
    ) %>% fluidRow()
  })
  
  observe({
    current_selection <- rv$selected_var_names
    vars_to_setup <- setdiff(current_selection, rv$initialized_vars)
    
    lapply(vars_to_setup, function(var_name_to_setup) {
      local({ 
        local_var_name <- var_name_to_setup 
        if (is.null(rv$plot_axis_limits[[local_var_name]])) {
          rv$plot_axis_limits[[local_var_name]] <- list(min = NULL, max = NULL, is_custom_active = FALSE)
        }
        # Initialize trigger for this table
        rv$table_update_triggers[[local_var_name]] <- Sys.time() 
        
        plot_data_slicers[[local_var_name]] <- reactive({
          req(rv$edited_data_vector) 
          var_info_slice <- variable_defs[[local_var_name]]
          if (is.null(var_info_slice) || is.null(var_info_slice$indices) ||
              max(var_info_slice$indices + 1) > length(rv$edited_data_vector) ||
              min(var_info_slice$indices + 1) <= 0) { return(rep(NA_real_, 10)) }
          return(as.numeric(rv$edited_data_vector[var_info_slice$indices + 1]))
        })
        
        # Render DT Table
        output[[paste0("table_", local_var_name)]] <- renderDT({
          req(rv$table_update_triggers[[local_var_name]]) 
          
          req(rv$edited_data_vector) 
          var_info_table <- variable_defs[[local_var_name]]
          if (is.null(var_info_table) || length(var_info_table$indices) != 10) return(NULL)
          
          current_values_for_table <- plot_data_slicers[[local_var_name]]()
          req(current_values_for_table)
          
          df_for_table <- data.frame(
            Value = formatC(current_values_for_table, format = "e", digits = 3)
          )
          
          DT::datatable(df_for_table, 
                        rownames = table_row_names_with_depths,
                        editable = list(target = "cell", disable = list(columns = c(0))), 
                        options = list(
                          dom = 't', 
                          pageLength = 10, 
                          scrollY = "290px", 
                          scrollCollapse = TRUE,
                          ordering = FALSE 
                        ),
                        selection = 'none',
                        class = 'cell-border stripe compact' 
          )
        })
        
        # DT Table Edits Observer
        observeEvent(input[[paste0("table_", local_var_name, "_cell_edit")]], {
          edit_info <- input[[paste0("table_", local_var_name, "_cell_edit")]]
          req(edit_info, rv$edited_data_vector) 
          
          if(is.null(edit_info$col) || edit_info$col != 1) { 
            return() 
          }
          
          row_idx <- edit_info$row 
          new_value_char <- edit_info$value
          
          new_value_num <- tryCatch({
            as.numeric(new_value_char)
          }, warning = function(w) NA, error = function(e) NA)
          
          if(is.na(new_value_num)){
            msg <- paste("Invalid numeric input ('", new_value_char ,"') for ", (variable_defs[[local_var_name]]$name %||% local_var_name), " at layer ", row_idx -1, sep="")
            rv$status_message <- msg; showNotification(msg, type = "error", duration = 7)
            rv$table_update_triggers[[local_var_name]] <- Sys.time() 
            return()
          }
          
          var_info_edit <- variable_defs[[local_var_name]]
          if (!is.null(var_info_edit) && row_idx <= length(var_info_edit$indices)) { 
            actual_index_in_variable <- var_info_edit$indices[row_idx] 
            index_to_update_in_vector <- actual_index_in_variable + 1
            
            if (index_to_update_in_vector > 0 && index_to_update_in_vector <= length(rv$edited_data_vector)) {
              temp_vector <- rv$edited_data_vector 
              old_value <- temp_vector[index_to_update_in_vector]
              
              if (!isTRUE(all.equal(old_value, new_value_num, tolerance = .Machine$double.eps^0.5))) { 
                temp_vector[index_to_update_in_vector] <- new_value_num
                rv$edited_data_vector <- temp_vector 
                
                group_name <- find_variable_group(local_var_name)
                if (!is.null(group_name)) {
                    value_change <- new_value_num - old_value
                    current_deficits <- isolate(rv$conservation_sums[[group_name]]$deficit)
                    current_deficits[row_idx] <- current_deficits[row_idx] - value_change
                    rv$conservation_sums[[group_name]]$deficit <- current_deficits

                    rv$status_message <- sprintf(
                        "Updated %s L%d from %.2e to %.2e (change: %.2e). New Deficit: %.2e.",
                        local_var_name, row_idx - 1, old_value, new_value_num, value_change, current_deficits[row_idx]
                    )
                } else {
                    value_change <- new_value_num - old_value
                    rv$status_message <- sprintf(
                        "Updated %s L%d from %.2e to %.2e (change: %.2e).",
                        (var_info_edit$name %||% local_var_name), row_idx - 1, old_value, new_value_num, value_change
                    )
                }

              } else {
                rv$table_update_triggers[[local_var_name]] <- Sys.time()
                rv$status_message <- paste("Value for", (var_info_edit$name %||% local_var_name), "at layer", row_idx-1, "re-formatted (no numeric change).")
              }
            }
          }
        })
        
        
        # Reset Button Logic 
        reset_trigger <- eventReactive(input[[paste0("reset_", local_var_name)]], {
          list(var_to_reset = local_var_name, timestamp = Sys.time())
        }, ignoreInit = TRUE)
        
        observeEvent(reset_trigger(), {
          reset_action_info <- reset_trigger() 
          var_to_reset_name <- reset_action_info$var_to_reset
          
          var_info_reset <- isolate(variable_defs[[var_to_reset_name]])
          full_data_vec <- isolate(rv$full_data_vector)
          
          req(var_info_reset, full_data_vec, rv$edited_data_vector)
          
          original_values_slice <- full_data_vec[var_info_reset$indices + 1]
          
          # Update the data vector
          indices_to_update <- var_info_reset$indices + 1
          temp_vector <- rv$edited_data_vector 
          temp_vector[indices_to_update] <- original_values_slice
          rv$edited_data_vector <- temp_vector

          # Trigger table re-render
          rv$table_update_triggers[[var_to_reset_name]] <- Sys.time()
          
          # Recalculate deficit for the group if applicable
          group_name <- find_variable_group(var_to_reset_name)
          if(!is.null(group_name)){
            group_info <- variable_groups[[group_name]]
            current_sum_layers <- rep(0,10)
            for(member_name in group_info$members){
              member_indices <- variable_defs[[member_name]]$indices
              current_sum_layers <- current_sum_layers + rv$edited_data_vector[member_indices+1]
            }
            initial_sums <- isolate(rv$conservation_sums[[group_name]]$initial)
            rv$conservation_sums[[group_name]]$deficit <- initial_sums - current_sum_layers
          }
          
          msg <- paste((var_info_reset$name %||% var_to_reset_name), "has been reset.")
          rv$status_message <- msg
          showNotification(msg, type = "message", duration = 5, id = paste0("notify_reset_", var_to_reset_name)) 
        })
        
        #Custom X-Axis Toggle Switch Observer 
        observeEvent(debounce(reactive(input[[paste0("toggle_custom_", local_var_name)]]), millis = 500), {
          if (is.null(rv$plot_axis_limits[[local_var_name]])) {
            rv$plot_axis_limits[[local_var_name]] <- list(min = NULL, max = NULL, is_custom_active = FALSE)
          }
          is_active <- input[[paste0("toggle_custom_", local_var_name)]] %||% FALSE
          if (rv$plot_axis_limits[[local_var_name]]$is_custom_active != is_active) {
            rv$plot_axis_limits[[local_var_name]]$is_custom_active <- is_active
            updateSwitchInput(session, paste0("toggle_custom_", local_var_name), value = is_active)
            rv$status_message <- paste("Custom X-axis toggle for", variable_defs[[local_var_name]]$name %||% local_var_name, "set to", if (is_active) "ON" else "OFF")
            showNotification(rv$status_message, type = "message", duration = 3)
          }
        }, ignoreNULL = FALSE, ignoreInit = TRUE)
        
        # Apply Limits Button Logic
        apply_limits_trigger <- eventReactive(input[[paste0("apply_limits_", local_var_name)]], {
          xmin_val_ui <- input[[paste0("xmin_ui_", local_var_name)]]
          xmax_val_ui <- input[[paste0("xmax_ui_", local_var_name)]]
          list(var_to_limit = local_var_name, xmin = xmin_val_ui, xmax = xmax_val_ui, timestamp = Sys.time())
        }, ignoreInit = TRUE)
        
        observeEvent(apply_limits_trigger(), {
          limit_action_info <- apply_limits_trigger()
          var_to_limit_name <- limit_action_info$var_to_limit
          xmin_val <- limit_action_info$xmin; xmax_val <- limit_action_info$xmax
          notification_id <- paste0("notify_limits_", var_to_limit_name)
          valid_input <- TRUE; error_msg <- ""
          processed_xmin <- if (is.na(xmin_val) || !is.numeric(xmin_val)) NULL else xmin_val
          processed_xmax <- if (is.na(xmax_val) || !is.numeric(xmax_val)) NULL else xmax_val
          if (!is.null(processed_xmin) && !is.null(processed_xmax) && processed_xmin >= processed_xmax) {
            valid_input <- FALSE; error_msg <- "Min must be less than Max."
          }
          if (valid_input) {
            if(is.null(rv$plot_axis_limits[[var_to_limit_name]])) {
              rv$plot_axis_limits[[var_to_limit_name]] <- list(min = NULL, max = NULL, is_custom_active = FALSE)
            }
            rv$plot_axis_limits[[var_to_limit_name]]$min <- processed_xmin
            rv$plot_axis_limits[[var_to_limit_name]]$max <- processed_xmax
            rv$plot_axis_limits[[var_to_limit_name]]$is_custom_active <- TRUE
            updateSwitchInput(session, paste0("toggle_custom_", var_to_limit_name), value = TRUE)
            showNotification(paste("Custom limits applied for", variable_defs[[var_to_limit_name]]$name %||% var_to_limit_name), type = "message", duration = 3, id = notification_id)
          } else {
            showNotification(paste("Invalid limits for", variable_defs[[var_to_limit_name]]$name %||% var_to_limit_name, ":", error_msg), type = "error", duration = 5, id = notification_id)
          }
        })
        
        # Plot Rendering Output 
        output[[paste0("plot_", local_var_name)]] <- renderPlot({
          data_values_for_plot <- plot_data_slicers[[local_var_name]]() 
          current_axis_settings <- rv$plot_axis_limits[[local_var_name]] 
          req(data_values_for_plot, current_axis_settings) 
          var_info_plot <- variable_defs[[local_var_name]] 
          req(var_info_plot)
          
          if (any(is.na(data_values_for_plot)) || length(data_values_for_plot) != 10) {
            return(ggplot() + annotate("text", x=0.5, y=0.5, label=paste("Data error for",local_var_name), size=5) + theme_void())
          }
          plot_df <- data.frame(Value = data_values_for_plot, Y_Transformed = new_depth_midpoints_transformed, Depth_Index = 1:10)
          p <- ggplot(plot_df, aes(x = Value, y = Y_Transformed)) +
            geom_hline(yintercept = y_axis_breaks_transformed, linetype = "dashed", color = "gray") +
            geom_path(color = "steelblue", linewidth = 1) + geom_point(color = "steelblue", size = 3) +
            scale_y_reverse(name = "Depth (cm)", breaks = y_axis_breaks_transformed, labels = y_axis_breaks_orig, limits = c(max(y_axis_breaks_transformed), min(y_axis_breaks_transformed))) +
            xlab(var_info_plot$name %||% local_var_name) + ggtitle(paste("Profile for", var_info_plot$name %||% local_var_name)) +
            theme_minimal(base_size = 11) +
            theme(
              plot.background = element_rect(fill = "white", color = NA), 
              panel.background = element_rect(fill = "white", color = NA),
              plot.title = element_text(hjust = 0.5, size = 13, face = "bold"), 
              axis.title = element_text(size = 11),
              axis.text.x = element_text(size = 12), 
              axis.text.y = element_text(size = 11),
              panel.grid.major.y = element_blank(), 
              panel.grid.minor.y = element_blank(),
              plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")
            )
          if (isTRUE(current_axis_settings$is_custom_active)) {
            xlim_to_apply <- c(current_axis_settings$min, current_axis_settings$max)
            if (!is.null(xlim_to_apply[1]) || !is.null(xlim_to_apply[2])) {
              p <- p + coord_cartesian(xlim = xlim_to_apply)
            }
          }
          return(p)
        })
        
        # Download Plot Handler 
        output[[paste0("download_plot_", local_var_name)]] <- downloadHandler(
          filename = function() { paste0("plot_", gsub("[^a-zA-Z0-9_.-]", "_", local_var_name), "_", Sys.Date(), ".png") },
          content = function(file) {
            data_values_dl <- plot_data_slicers[[local_var_name]]()
            axis_settings_dl <- rv$plot_axis_limits[[local_var_name]]
            var_info_dl <- variable_defs[[local_var_name]]
            req(data_values_dl, axis_settings_dl, var_info_dl)
            if (any(is.na(data_values_dl)) || length(data_values_dl) != 10) { showNotification("Data error for download plot.", type="error"); return(NULL) }
            plot_df_dl <- data.frame(Value = data_values_dl, Y_Transformed = new_depth_midpoints_transformed, Depth_Index = 1:10)
            plot_to_save <- ggplot(plot_df_dl, aes(x = Value, y = Y_Transformed)) +
              geom_hline(yintercept = y_axis_breaks_transformed, linetype = "dashed", color = "gray") +
              geom_path(color = "steelblue", linewidth = 1) + geom_point(color = "steelblue", size = 3) +
              scale_y_reverse(name = "Depth (cm)", breaks = y_axis_breaks_transformed, labels = y_axis_breaks_orig, limits = c(max(y_axis_breaks_transformed), min(y_axis_breaks_transformed))) +
              xlab(var_info_dl$name %||% local_var_name) + ggtitle(paste("Profile for", var_info_dl$name %||% local_var_name)) +
              theme_minimal(base_size = 11) + 
              theme(
                plot.background = element_rect(fill = "white", color = NA), 
                panel.background = element_rect(fill = "white", color = NA),
                plot.title = element_text(hjust = 0.5, size = 13, face = "bold"), 
                axis.title = element_text(size = 11),
                axis.text.x = element_text(size = 10), 
                axis.text.y = element_text(size = 9),
                panel.grid.major.y = element_blank(), 
                panel.grid.minor.y = element_blank(),
                plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")
              )
            if (isTRUE(axis_settings_dl$is_custom_active)) {
              xlim_dl <- c(axis_settings_dl$min, axis_settings_dl$max)
              if (!is.null(xlim_dl[1]) || !is.null(xlim_dl[2])) {
                plot_to_save <- plot_to_save + coord_cartesian(xlim = xlim_dl)
              }
            }
            ggsave(filename = file, plot = plot_to_save, width = 6, height = 7, dpi = 300)
            showNotification(paste("Plot for", (var_info_dl$name %||% local_var_name), "saved."), type = "message")
          }
        )
        rv$initialized_vars <- union(rv$initialized_vars, local_var_name)
      }) 
    }) 
    
    lapply(current_selection, function(var_name_to_update_ui) {
      local({
        local_var_name_ui <- var_name_to_update_ui
        if (!is.null(rv$plot_axis_limits[[local_var_name_ui]])) { 
          updateSwitchInput(session, paste0("toggle_custom_", local_var_name_ui),
                            value = rv$plot_axis_limits[[local_var_name_ui]]$is_custom_active %||% FALSE)
          updateNumericInput(session, paste0("xmin_ui_", local_var_name_ui),
                             value = rv$plot_axis_limits[[local_var_name_ui]]$min %||% NA_real_)
          updateNumericInput(session, paste0("xmax_ui_", local_var_name_ui),
                             value = rv$plot_axis_limits[[local_var_name_ui]]$max %||% NA_real_)
        }
      })
    })
    
  }) 
  
  # Conservation Sums Logic
  
  # Dynamically render the UI for each conservation group
  lapply(names(variable_groups), function(group_name) {
    output[[paste0("conservation_status_", group_name)]] <- renderUI({
      req(rv$conservation_sums[[group_name]])
      
      initial_sums <- rv$conservation_sums[[group_name]]$initial
      deficits <- rv$conservation_sums[[group_name]]$deficit
      group_info <- variable_groups[[group_name]]
      
      tagList(
        lapply(1:10, function(i) {
          deficit_val <- deficits[i]
          deficit_class <- if (abs(deficit_val) < 1e-9) "" else if (deficit_val < 0) "deficit-red" else "deficit-green"
          
          # Create compensation buttons for this layer
          compensate_btns <- lapply(group_info$members, function(member_name) {
            actionButton(
              inputId = paste("compensate", group_name, i, member_name, sep = "_"),
              label = member_name,
              class = "btn-primary btn-xs"
            )
          })
          
          div(class = "layer-row",
              div(class = "layer-label", table_row_names_with_depths[i]),
              div(class = "sum-display", HTML(sprintf(
                  "Initial Sum: %.3e | Deficit: <span class='deficit-val %s'>%.3e</span>",
                  initial_sums[i], deficit_class, deficit_val
              ))),
              div(class = "compensate-buttons", compensate_btns)
          )
        })
      )
    })
  })
  
  # Dynamically create observers for all compensation buttons
  lapply(names(variable_groups), function(group_name) {
    group_info <- variable_groups[[group_name]]
    lapply(group_info$members, function(member_name) {
      lapply(1:10, function(layer_idx) {
        button_id <- paste("compensate", group_name, layer_idx, member_name, sep = "_")
        observeEvent(input[[button_id]], {
          
          deficit_to_apply <- isolate(rv$conservation_sums[[group_name]]$deficit[layer_idx])
          
          if (abs(deficit_to_apply) < 1e-9) {
            showNotification(paste("No deficit to compensate for Layer", layer_idx - 1), type="default")
            return()
          }
          
          # Find the index in the main data vector for the target variable and layer
          var_info <- variable_defs[[member_name]]
          target_global_idx <- var_info$indices[layer_idx] + 1
          
          current_val <- isolate(rv$edited_data_vector[target_global_idx])
          
          # The amount to add to the current value is the deficit
          new_val <- current_val + deficit_to_apply
          
          # Safety check: value cannot be negative
          actual_change <- deficit_to_apply
          if (new_val < 0) {
            actual_change <- -current_val # Change is limited to making the value 0
            new_val <- 0
          }
          
          # Update the data vector
          temp_vector <- rv$edited_data_vector
          temp_vector[target_global_idx] <- new_val
          rv$edited_data_vector <- temp_vector
          
          # Update the deficit
          remaining_deficit <- deficit_to_apply - actual_change
          current_deficits <- isolate(rv$conservation_sums[[group_name]]$deficit)
          current_deficits[layer_idx] <- remaining_deficit
          rv$conservation_sums[[group_name]]$deficit <- current_deficits
          
          # Trigger table re-render for the modified variable
          rv$table_update_triggers[[member_name]] <- Sys.time()
          
          # User feedback
          if (abs(remaining_deficit) > 1e-9) {
            msg <- sprintf(
              "Partially compensated Layer %d with %s. Remaining deficit: %.2e",
              layer_idx - 1, member_name, remaining_deficit
            )
            showNotification(msg, type = "warning", duration = 8)
            rv$status_message <- msg
          } else {
            msg <- sprintf(
              "Compensated Layer %d deficit of %.2e using %s.",
              layer_idx - 1, deficit_to_apply, member_name
            )
            showNotification(msg, type = "message", duration = 5)
            rv$status_message <- msg
          }
        })
      })
    })
  })

  # Overwrite File in Working Directory Button
  overwrite_wd_trigger <- eventReactive(input$overwrite_wd_button, {
    list(timestamp = Sys.time()) 
  }, ignoreInit = TRUE)
  
  observeEvent(overwrite_wd_trigger(), {
    req(rv$edited_data_vector, input$file_input$name) 
    
    target_file_path <- file.path(getwd(), input$file_input$name)
    
    confirmSweetAlert(
      session = session, inputId = "confirm_overwrite_wd",
      title = "Overwrite Original File?",
      text = paste0("This will attempt to overwrite the file named '", input$file_input$name, 
                    "' in your current R working directory: \n", getwd()),
      type = "warning", 
      btn_labels = c("Cancel", "Yes, Overwrite File"), 
      danger_mode = TRUE,
      html = TRUE 
    )
  })
  
  observeEvent(input$confirm_overwrite_wd, {
    if (isTRUE(input$confirm_overwrite_wd)) {
      req(rv$edited_data_vector, input$file_input$name)
      target_file_path <- file.path(getwd(), input$file_input$name)
      
      tryCatch({
        writeBin(rv$edited_data_vector, target_file_path, size = 8, endian = "little")
        rv$status_message <- paste("File '", input$file_input$name, "' overwritten in working directory", sep="")
        showNotification(rv$status_message, type = "message", duration = 7, id = "notify_overwrite_wd_success") 
      }, error = function(e) {
        rv$status_message <- paste("Error overwriting file '", input$file_input$name, "': ", e$message, sep="")
        showNotification(rv$status_message, type = "error", duration = NULL, id = "notify_overwrite_wd_error") 
      })
    }
  })
  
  
  # Global Actions (Save All Data, Status, Exit) 
  output$download_data_button <- downloadHandler(
    filename = function() { paste0(tools::file_path_sans_ext(input$file_input$name %||% "data"), "_modified_", Sys.Date(), ".rst") },
    content = function(file) {
      req(rv$edited_data_vector)
      tryCatch({
        out_con <- file(file, "wb"); writeBin(rv$edited_data_vector, out_con, size = 8, endian = "little"); close(out_con)
        rv$status_message <- paste("Modified data saved as a new file"); showNotification(rv$status_message, type = "message", duration=7)
      }, error = function(e) {
        rv$status_message <- paste("Error saving file:", e$message); showNotification(rv$status_message, type = "error")
      })
    }, contentType = "application/octet-stream"
  )
  
  observe({ if (is.null(rv$edited_data_vector)) shinyjs::disable("download_data_button") else shinyjs::enable("download_data_button") })
  output$status_output <- renderText({ rv$status_message })
  
  observeEvent(input$exit, {
    confirmSweetAlert(session = session, inputId = "confirm_exit", title = "Is your profile good enough?",
                      text = "(The depth profile)", type = "warning",
                      btn_labels = c("Cancel", "Yes, Exit"), danger_mode = TRUE)
  })
  
  observeEvent(input$confirm_exit, {
    if (isTRUE(input$confirm_exit)) {
      stopApp()
      runjs("window.close();") 
    }
  })
}



#' EPEMuso
#'
#' launchApp launch the shiny app
#' @param ... Other parameters for shinyApp function
#' @importFrom shiny shinyApp shinyOptions
#' @export
#' 
EPEMuso <- function(directory = NULL, ...){ 
  shinyOptions(workdir = getwd())
  if(is.null(directory)){
    shinyOptions(musoRoot = ".")
  } else {
    shinyOptions(musoRoot = normalizePath(directory))
  }
  shinyApp(ui = EPEui(), server = EPEserver, options = c(list(launch.browser = TRUE), list(...)))
}

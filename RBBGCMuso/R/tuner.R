#' tuneMusoUI 
#' 
#' This is a simple parameter tuner function which works great in a flat directory system
#'
#' @param parameterFile optional, the parameter csv file
#' @importFrom plotly plotlyOutput
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
    defaultValues <- musoGetValues(settings$epcInput[2],parameters[,2])            
    fluidPage(
        # tags$head(tags$style(HTML("#iniContainer {width: 80vw;}"))),
        tags$head(tags$style(HTML("#contolp {height: 80vh;overflow-y:scroll;}"))),
        titlePanel("Biome-BGCMuSo parameter tuner"),
        sidebarLayout(
            sidebarPanel(tabsetPanel(type="tabs",
                tabPanel("params",
                fileInput("measurementFile", "Upload Measurement File", 
                        accept = c(".txt")),
                   checkboxInput("autoupdate","Automatic update"),
                     checkboxInput("singleYear", "Single year mode", value = FALSE),
                    uiOutput("yearRangeUI"),
                   tags$div(id="controlp",selectInput("ovar",
                       label="Select output Variable",
                       choices=settings$dailyOutputTable$name,
                       width="40%"
                       ), ## slider for parameters
                        do.call(tagList, lapply(1:nrow(parameters), function(x) {
                            sliderInput(paste0("param_", x),
                            label = parameters[x, 1],  
                            min = parameters[x, 3],  
                            max = parameters[x, 4],  
                            value = defaultValues[x],  
                            step = (parameters[x, 4] - parameters[x, 3]) / 100)  # step size
            }))
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
                )),
            mainPanel(plotlyOutput(outputId="Result"))
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

    parameters <- read.csv("parameters.csv", stringsAsFactors=FALSE)

    #outputList <- vector(mode = "list", length = 2)
    #outputList <- reactiveValues()
    #outputList[['prev']] <- character(0)
    #outputList[['next']] <- character(0)
    outputList <- reactiveValues(prev = character(0), nextVal = character(0))

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

    measurements <- reactive({
    req(input$measurementFile)  ##Ensure a file is uploaded
    df <- read.table(input$measurementFile$datapath, header = TRUE, sep = "", stringsAsFactors = FALSE)
    
    
    df[df == -9999] <- NA
    
    # Convert year, month, day into a Date object
    df$Date <- as.Date(with(df, paste(yyyy, mm, dd, sep = "-")), "%Y-%m-%d")
    
    return(df)
    })


    observeEvent(input$runModel,{

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


observe({
    if (length(outputList$nextVal) != 0) {
        output$Result <- renderPlotly({
            req(input$yearRange)
            selectedYears <- if (input$singleYear) {
                input$yearRange
            } else {
                seq(input$yearRange[1], input$yearRange[2])
            }

            filteredDates <- dates[as.numeric(format(dates, "%Y")) %in% selectedYears]
            filteredPrev <- if (length(outputList$prev) != 0) {
                outputList$prev[as.numeric(format(dates, "%Y")) %in% selectedYears, ]
            } else {
                NULL
            }
            filteredNext <- outputList$nextVal[as.numeric(format(dates, "%Y")) %in% selectedYears, ]
            
            if (input$ovar %in% c("GPP", "TR")) {
                if (!is.null(filteredPrev)) {
                    filteredPrev[, input$ovar] <- filteredPrev[, input$ovar] * 1000
                }
                filteredNext[, input$ovar] <- filteredNext[, input$ovar] * 1000
            }

            
            p <- plot_ly()

            # Add simulation traces
            if (!is.null(filteredPrev)) {
                p <- add_trace(p, x = filteredDates, y = filteredPrev[, input$ovar], 
                               type = 'scatter', mode = 'lines', name = "Previous Simulation") 
            }
            p <- add_trace(p, x = filteredDates, y = filteredNext[, input$ovar], 
                           type = 'scatter', mode = 'lines', name = "New Simulation", line = list(color = "red"))

            # Overlay measurements ONLY if a file is uploaded
            if (input$ovar %in% c("GPP", "TR", "ET") && !is.null(input$measurementFile)) {
                df <- measurements()

                measurement_col <- switch(input$ovar,
                                          "GPP" = 5,
                                          "TR" = 6,
                                          "ET" = 7)
                # Filter data for selected years
                df_filtered <- df[df$yyyy %in% selectedYears, ]

                p <- add_trace(p, x = df_filtered$Date, y = df_filtered[, measurement_col],
                               type = 'scatter', mode = 'markers', marker = list(symbol = "circle", size = 7),
                               name = paste(input$ovar, "Measurement"))
            }

            p  
        })
    }
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

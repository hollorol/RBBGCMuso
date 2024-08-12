#' tuneMusoUI 
#' 
#' This is a simple parameter tuner function which works great in a flat directory system
#'
#' @param parameterFile optional, the parameter csv file
#' @importFrom plotly plotlyOutput
#' @importFrom shiny tags actionButton numericInput HTML checkboxInput titlePanel radioButtons textAreaInput fluidPage sidebarLayout sidebarPanel mainPanel getShinyOption tabsetPanel tabPanel tagList selectInput
#' @usage ...
#' @export 

restartChangeMusoUI <- function(parameterFile = NULL, ...){
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
                   checkboxInput("autoupdate","Automatic update"),
                   tags$div(id="controlp",selectInput("ovar",
                       label="Select output Variable",
                       choices=settings$dailyOutputTable$name,
                       width="40%"
                       ),
                    do.call(tagList,lapply(1:nrow(parameters),function(x){
                                               numericInput(paste0("param_",x),
                                                            parameters[x,1],
                                                            defaultValues[x],
                                                            step=defaultValues[x]/10,
                                                            width="40%"
                                               )
}))),
              tags$div(actionButton(inputId="runModel","Run MuSo"),
                       radioButtons(inputId="destination",
                                    label="reference or modified",
                                    choiceValues=c("auto","prev","next"),
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

restartChangeMusoServer <- function(input, output, session){

    settings <- setupMuso()
    dates <- as.Date(musoDate(settings$startYear, numYears=settings$numYears),"%d.%m.%Y") 

    parameters <- read.csv("parameters.csv", stringsAsFactors=FALSE)

    outputList <- vector(mode = "list", length = 2)
    outputList <- reactiveValues()
    outputList[['prev']] <- character(0)
    outputList[['next']] <- character(0)


    observeEvent(input$runModel,{

                     paramVal <- sapply(1:nrow(parameters),function(x){
                                            input[[paste0("param_", x)]]
              })


                     if(isolate(input$destination) == "auto"){
                         outputList[['prev']] <- isolate(outputList[['next']]) 
                         outputList[['next']] <- calibMuso(settings = settings,
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
                         outputList[['prev']] <- isolate(outputList[['next']]) 
                         outputList[['next']] <- calibMuso(settings = settings,
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
        if(length(outputList[['next']])!=0){
            output$Result <- renderPlotly(
                                          {
                                              p <- plot_ly()
                                              if(length(outputList[['prev']])!=0){
                                                  p <- add_trace(p, x=dates, y=outputList[['prev']][,input$ovar], type='scatter',
                                                                    mode='lines')
                                              }
                                              add_trace(p, x=dates, y=outputList[['next']][,input$ovar], color="red", type='scatter',
                                                                    mode='lines')
                                          }
            )


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


#' mgmTimePlot
#'
#' This function shows the timeline of the plant management types.
#'
#' @author Erzsebet Kristof
#' @param mgmDir This folder contains the management files.
#' @importFrom data.table fread
#' @importFrom magrittr '%>%'
# @importFrom ggplot2 ggplot geom_bar scale_fill_manual theme
#' @importFrom plotly plot_ly
#' @return An interactive plot about time and management.
#' @export

mgmTimePlot <- function(mgmDir = "./",plotName=".*"){
plotName <- ".*hhs.*"
  mgm <- data.frame("type"=c("planting", "thinning",
                             "mowing", "grazing",
                             "harvesting", "ploughing",
                             "fertilizing", "irrigating"),
                    "ext"=c("plt", "thn", "mow", "grz",
                            "hrv", "plg", "frz" ,"irg"),
                    "num"=1:8, stringsAsFactors = FALSE)

  # Reading files:
 mgmDir <- tcltk::tkchooseDirectory()
    mgmNames <- (function(fileNames){
    fileNames[(fileNames %>% tools::file_ext()) %in% mgm$ext]
  })(list.files(mgmDir,pattern = plotName, recursive = TRUE))
  
  mgmDates <- lapply(mgmNames, function(mgmFiles){
    fread(file.path(mgmDir, mgmFiles), skip=1, select=1, fill=TRUE, header=FALSE, stringsAsFactors = FALSE)
  })

  mgmTimeTable <- lapply(seq_along(mgmNames),function(fileNameIndex){
    extension  <- gsub(".*\\.","",mgmNames[fileNameIndex])
    cbind.data.frame(mgmDates[[fileNameIndex]],mgm$type[grep(extension, mgm$ext)])
  })

  mgmTimeTable <- do.call(rbind,mgmTimeTable)
  colnames(mgmTimeTable) <- c("date","type")

  managementPlot <- mgmTimeTable %>%
    # Visualisation with ggplot:
    # ggplot(aes(x = date, fill = type)) +
    # geom_bar() +
    # scale_fill_manual("Management types", values=c("planting"="blue", "thinning"="deepskyblue", "mowing"="chartreuse", "grazing"="dark orange",
    #                                               "harvesting"="red", "ploughing"="purple", "fertilizing"="dark green", "irrigating"="grey50")) +
    # xlab("Date") +
    # theme(axis.title.y=element_blank(),
    #      axis.text.y=element_blank(),
    #      axis.ticks.y=element_blank(),
    #      legend.direction="horizontal", legend.position="bottom",
    #      axis.text.x=element_text(color="black", size=10, angle=90))
    # 
   
    # Visualisation with plotly:
    colorbar <- c("blue", "deepskyblue", "chartreuse", "dark orange", "red", "purple", "dark green", "grey50")
  
    plotly::plot_ly() %>%
     add_trace(x = mgmTimeTable$date, y = mgmTimeTable$type, type = "bar",
          transforms = list(
            list(
              type = 'groupby',
              groups = mgmTimeTable$type,
              text = mgm$type,
              hoverinfo = 'text',
              styles = list(
              for (i in 1:dim(mgm)[1]) {
                list(target = mgm[i,1], value = list(marker = list(color = colorbar[i]))) 
              }
              )
            )
          )
       ) %>%
    layout(xaxis = list(title = ""),
           yaxis = list(title = "", range = c(0,length(unique(mgmTimeTable$type)))),
           barmode = "stack")
  
  return(mgmTimePlot)
}

    
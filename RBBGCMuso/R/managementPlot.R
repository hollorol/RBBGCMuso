#' mgmTimePlot

#'

#' This function shows the timeline of the plant management types.

#'

#' @author Erzsebet Kristof

#' @param mgmDir This folder contains the management files.

#' @importFrom data.table fread

#' @importFrom ggplot2 ggplot geom_bar scale_fill_manual theme

#' @importFrom magrittr '%>%'

#' @return An interactive plot about time and management.

#' @export



mgmTimePlot <- function(mgmDir = "./"){
  
  
  
  mgm <- data.frame("type"=c("planting", "thinning",
                             
                             "mowing", "grazing",
                             
                             "harvesting", "ploughing",
                             
                             "fertilizing", "irrigating"),
                    
                    "ext"=c("plt", "thi", "mow", "gra",
                            
                            "hrv", "plg", "frz" ,"irr"),
                    
                    "num"=1:8, stringsAsFactors = FALSE)
  
  
  
  # Reading files:
  
  
  
  mgmNames <- list.files(mgmDir)
  
  
  
  mgmDates <- lapply(mgmNames, function(mgmFiles){
    
    fread(paste(mgmDir, mgmFiles, sep = "/"), skip=1, select=1, fill=TRUE, header=F, stringsAsFactors = FALSE)
    
  })
  
  
  
  mgmTimeTable<- lapply(seq_along(mgmNames),function(fileNameIndex){
    
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
  
  plot_ly() %>%
    
    add_trace(x = mgmTimeTable$date, y = mgmTimeTable$type, type = "bar",
              
              transforms = list(
                
                list(
                  
                  type = 'groupby',
                  
                  groups = mgmTimeTable$type,
                  
                  text = mgm$type,
                  
                  hoverinfo = 'text',
                  
                  styles = list(
                    
                    list(target = "planting", value = list(marker = list(color = "blue"))),
                    
                    list(target = "thinning", value = list(marker = list(color = "deepskyblue"))),
                    
                    list(target = "mowing", value = list(marker = list(color = "chartreuse"))),
                    
                    list(target = "grazing", value = list(marker = list(color = "dark orange"))),
                    
                    list(target = "harvesting", value = list(marker = list(color = "red"))),
                    
                    list(target = "ploughing", value = list(marker = list(color = "purple"))),
                    
                    list(target = "fertilizing", value = list(marker = list(color = "dark green"))),
                    
                    list(target = "irrigating", value = list(marker = list(color = "grey50")))
                    
                  )
                  
                )
                
              )
              
    ) %>%
    
    layout(xaxis = list(title = ""),
           
           yaxis = list(title = "", range = c(0,length(unique(mgmTimeTable$type)))),
           
           barmode = "stack")
  
  
  
  return(managementPlot)
  
}
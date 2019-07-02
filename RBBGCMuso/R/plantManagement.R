#' mgmTimePlot
#'
#' Overal description  
#'
#' @author Erzsebet Kristof
#' @param mgmDir This folder contains the management files.
#' @importFrom data.table fread
#' @importFrom ggplot2 ggplot geom_bar scale_fill_manual theme
#' @importFrom magrittr '%>%'
#' @return An interactive plot about time and management
#' @export

mgmTimePlot <- function(mgmDir = "./"){

  mgm <- data.frame("type"=c("planting", "thinning",
                             "mowing", "grazing",
                             "harvesting", "ploughing",
                             "fertilizing", "irrigating"),
                    "ext"=c("plt", "thi", "mow", "gra",
                            "hrv", "plg", "frz" ,"irr"),
                    "nr"=1:8, stringsAsFactors = FALSE)

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
    ggplot(aes(x = date, fill = type)) +
    geom_bar() +
    scale_fill_manual("Management types", values=c("planting"="blue", "thinning"="deepskyblue", "mowing"="chartreuse", "grazing"="dark orange",
                                                   "harvesting"="red", "ploughing"="purple", "fertilizing"="dark green", "irrigating"="grey50")) +
    xlab("Date") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.direction="horizontal", legend.position="bottom",
          axis.text.x=element_text(color="black", size=10, angle=90))
  
  
  return(managementPlot)
}

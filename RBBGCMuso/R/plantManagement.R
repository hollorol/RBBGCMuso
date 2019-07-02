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

  mgmNames <- list.files()

  mgmDates <- lapply(mgmNames, function(manFiles){
    fread(manFiles, skip=1, select=1, fill=TRUE, header=F, stringsAsFactors = FALSE)
  })

  mgmTimeTable<- lapply(seq_along(mgmNames),function(fileNameIndex){
    extension  <- gsub(".*\\.","",mgmNames[fileNameIndex])
    cbind.data.frame(mgmDates[[fileNameIndex]],mgm$type[grep(extension, mgm$ext)])
  })

   mgmTimeTable <- do.call(rbind,mgmTimeTable)
  colnames(mgmTimeTable) <- c("date","type")

  managementPlot <- mgmTimeTable %>%
    ggplot(aes(x = date, fill = type)) + geom_bar()
  return(managementPlot)
}

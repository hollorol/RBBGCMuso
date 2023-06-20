## #' setupMuso6
## #'
## #' This is the setup function for MuSo version: 6
## #'
## #' @author Roland HOLLOS
## #' @param setupFile
## #' @export

## setupMuso6<- function(setupFile){
    
## }

## ini <- readLines("./hhs_apriori_MuSo6_normal.ini")
## flags <- c("MET_INPUT",
##            "RESTART",
##            "TIME_DEFINE",
##            "CO2_CONTROL",
##            "NDEP_CONTROL",
##            "SITE",
##            "SOILPROP_FILE",
##            "EPC_FILE",
##            "MANAGEMENT_FILE",
##            "SIMULATION_CONTROL",
##            "W_STATE",
##            "CN_STATE",
##            "CLIM_CHANGE",
##            "CONDITIONAL_MANAGEMENT_STRATEGIES",
##            "OUTPUT_CONTROL",
##            "DAILY_OUTPUT",
##            "ANNUAL_OUTPUT",
##            "END_INIT")
## getSegments <- function(ini, flags){
##     output <- list()
##     flagIterator <- 1:(length(flags)-1)
##     for(i in flagIterator){
##         output[[flags[i]]] <- lapply(ini[(grep(flags[i],ini)+1):(grep(flags[i+1],ini)-2)], function(x){
##             unlist(strsplit(x,split = "\\["))[1]
##         })
##     }
##     output
## }
## getSegments(ini,flags)

## gsub("(.*\\[\\|)([a-zA-Z1-9_]*)","",ini)
## stringi::stri_trim_right("rexamine.com/", "\\[r\\]")
## stri_extract("asdfasdf [|Ezat|]",regex = "\\[\\|*\\]")
## lapply(ini,function(x) gsub("\\s","",(strsplit(x,split= "T]"))[[1]][2]))

setwd("~/hhs")
inputLoc <- "./"
version <- "latest"
setupMuso()
options("RMuso_versionVars")
?options
iniName <- 
execPath <- 
depTable <- data.frame(
           child = c("weather", "endpointIn","endPointOut", "CO2",
                     "N", "soil", "startEpc", "management",
                    "planting", "thinning", "mowing", "grazing",
                    "harvest", "cultivation", "fertilization","irrigation", "plantEpc"),
           parent = c("iniFile", "iniFile", "iniFile",
                      "iniFile", "iniFile", "iniFile","iniFile",
                      "iniFile", "management",  "management",
                      "management",  "management",  "management",
                      "management",  "management",  "management", "planting"),
           block = c("MET_INPUT", "RESTART",
                     "RESTART", "CO2_CONTROL", "NDEP_CONTROL", "SOIL_FILE",
                     "EPC_FILE", "MANAGEMENT_FILE",
           "PLANTING", "THINNING", "MOWING", "GRAZING", "HARVESTING",
           "PLOUGHING", "FERTILIZING", "IRRIGATING", "#.*\\s+(.*\\.epc)") ,
           position = c(1,3,4,3,3,1,1,1,2,2,2,2,2,2,2,2,1),stringsAsFactors=FALSE
)
depTable

qu <- function(name, depTable){
    if (name == "iniFile") {
        return(NULL)
    } else {
        parent <- depTable$parent[match(name,depTable$child)]
        c(qu(parent,depTable),parent)
    }
}

filePathFromIni <- function (iniName, fName, execPath = "./", depTable,
                             noneWord = "none"){

    if(fName == "iniFile"){
        return(iniName)
    }

    depRow <-  depTable[match(fName,depTable$child),]
    block <- depRow$block
    position <- depRow$position 
    parent <- depRow$parent
    parentPath <- filePathFromIni(iniName,parent, execPath, depTable) 
    parentFile <- readLines(parentPath) 

    if(substring(block,1,1) != "#"){ # when not regex block
        blockRow <- grep(block, parentFile, perl = TRUE)
        innerPath <- gsub("(.*?)\\s+.*","\\1",parentFile[blockRow + position],
                          perl = TRUE)
        ifelse(innerPath == noneWord, NA, file.path(execPath,innerPath))
    } else {
        block <- substring(block,2)
        elines <- grep(block,parentFile)
        file.path(execPath, unique(gsub(block, paste0("\\",position),
                                        parentFile[elines],  perl = TRUE)))
    }
}

gsub("(.*\\s+)(.*\\.epc)", "\\2", parentFile[1],  perl = TRUE)
iniName <- "/home/fodor.nandor/AgroMo/data/input/initialization/grid/cropland/1003_1.ini"
fName <- "planting" 
execPath <- "/home/fodor.nandor/AgroMo/data/"
filePathFromIni(iniName, fName, execPath, depTable)

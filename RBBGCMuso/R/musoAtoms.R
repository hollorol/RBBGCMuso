getMusoAtom <- function(atomName,
                        iniName,
                        execPath,
                        atomList = jsonlite::read_json(system.file("RBBGCMuso","data"))){

    if(!is.element(atomName, names(atomList))){
        stop("MuSo atom not found, please check the available atoms with the musoAtomList function")
    }

    fileList <- getFilesFromIni(iniName, execPath)
    fileList[["iniFile"]] <- iniName # Have to be fixex in getFilesFromIni
    atom <- atomList[[atomName]]
    reFile <- readLines(fileList[[atom$source]])
    ln <- grep(atom$block,reFile,perl=TRUE)
    do.call(sprintf("as.%s",atom$type),list(strsplit(reFile[ln + as.integer(atom$index)], split = "\\s+")[[1]][1:atom$length]))
}

musoAtomList <- function(atomList=jsonlite::read_json(system.file("RBBGCMuso", "data"))){
    names(atomList)
}


getVars <- function(iniFile, annual=FALSE, atomList){
    iniLines <- readLines(iniFile)
    blockName <- ifelse(annual,"ANNUAL_OUTPUT","DAILY_OUTPUT")
    startLine <- grep(blockName,iniLines) 
    numVars <- as.integer(gsub("(.*?)\\s+.*","\\1",iniLines[startLine + 1],perl = TRUE))
    as.integer(gsub("(.*?)\\s+.*","\\1",iniLines[(startLine + 2):(startLine + 1+numVars)])) 
}

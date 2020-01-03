#' postProcMuso
#' 
#' This is a function wich provides some minimal post processing capabilities
#' @keywords internal
postProcMuso  <- function(modelData, procString){
    modelDat <- modelData[,-(1:4)]
    cNames <- colnames(modelData)
    tocalc <- gsub("(@)(\\d)","modelDat[,\\2]",procString)
    newVarName <- gsub("\\s","",unlist(strsplit(procString,"<-"))[1])
    assign(newVarName,eval(parse(text = unlist(strsplit(tocalc,"<-"))[2])))
    modelData <- cbind.data.frame(modelData,eval(parse(text = newVarName)))
    colnames(modelData) <- c(cNames,newVarName)
    modelData
}


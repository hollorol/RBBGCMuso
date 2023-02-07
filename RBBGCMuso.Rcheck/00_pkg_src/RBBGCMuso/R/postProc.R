postProcMuso  <- function(modelData, procString){
    cNames <- colnames(modelData)
    tocalc <- gsub("(@)(\\d)","modelData[,\\2]",procString)
    newVarName <- gsub("\\s","",unlist(strsplit(procString,"<-"))[1])
    assign(newVarName,eval(parse(text = unlist(strsplit(tocalc,"<-"))[2])))
    modelData <- cbind.data.frame(modelData,eval(parse(text = newVarName)))
    colnames(modelData) <- c(cNames,newVarName)
    modelData
}


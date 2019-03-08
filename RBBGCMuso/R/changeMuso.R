#' This is the function which is capable to change multiple specific lines to others using their row numbers.
#'
#' The function uses the previous changspecline function to operate.
         ##From now changespecline is in the forarcheologist file, because itis  no longer needed
#' 
#' @author Roland Hollos
#' @keywords internal
#' 

changemulline <- function(filePaths, calibrationPar, contents, fileOut, fileToChange, modifyOriginal=FALSE){
    selectFileToWrite <- function(filePaths, fileTochange){
        if(fileToChange == "epc"){
            return(1)
        } else{
            return(2)            
        }

    }
    
    if(xor(is.list(calibrationPar), is.list(contents)) ){
        stop("If you change epc and ini files also, you have to use list for calibrationPar, and paramateters.")
    }
     
    if(!is.element(fileToChange,c("ini","epc","both"))){
        stop("RBBGCMuso can only change ini or epc file, so fileToChange can be 'epc/ini/both'")
    }

    if(fileToChange == "epc" | fileToChange == "ini"){
        parMat<-cbind(calibrationPar, contents)
        parMat<- parMat[order(parMat[,1]),]
        changeMusoC(inFile = filePaths[selectFileToWrite(filePaths, fileToChange)],
                    outFile = fileOut[selectFileToWrite(filePaths, fileToChange)],
                    parMat)
    }
    
    if(fileToChange == "both"){
        parMat<-list()
        parMat[[1]]<-cbind(calibrationPar[[1]], contents[[1]])
        parMat[[1]][order(parMat[[1]][,1]),]
        parMat[[2]]<-cbind(calibrationPar[[2]], contents[[2]])
        parMat[[2]][order(parMat[[2]][,1]),]
        
        changeMusoC(filePaths[1],fileOut[1],parMat[[1]] )
        changeMusoC(filePaths[2],fileOut[2],parMat[[2]] )
    }

}



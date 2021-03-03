options(AgroMo_depTree=readRDS("~/projects/AgroMo/inst/depTree.RDS"))
filename_sans_ext <- function(filename){
    gsub("\\.(?:\\w*)$", "", filename, perl=TRUE)
}


recoverAfterEval <- function(expr){
    wd <- getwd()
    tryCatch({
        eval(expr)
        setwd(wd) 
    }, error=function(e){
            setwd(wd)
            # stop(e)
            NULL
    })
}

getQueue <- function(depTree=options("AgroMo_depTree")[[1]], startPoint){
    
    if(length(startPoint) == 0){
        return(c())
    }
    parent <- depTree[depTree[,"name"] == startPoint,"parent"]
    c(getQueue(depTree, depTree[depTree[,"child"] == depTree[depTree[,"name"] == startPoint,"parent"],"name"]),parent)
}

#' getFilePath
#'
#' This function reads the ini file and for a chosen fileType it gives you the filePath
#' @param iniName The name of the ini file
#' @param filetype The type of the choosen file. For options see options("AgroMo_depTree")[[1]]$name
#' @param depTree The file dependency defining dataframe. At default it is:  options("AgroMo_depTree")[[1]]
#' @export

getFilePath <- function(iniName, fileType, execPath = "./", depTree=options("AgroMo_depTree")[[1]]){
    if(!file.exists(iniName) || dir.exists(iniName)){
        stop(sprintf("Cannot find iniFile: %s", iniName))
    }

    startPoint <- fileType
    startRow <- depTree[depTree[,"name"] == startPoint,]
    startExt <- startRow$child

    parentFile <- Reduce(function(x,y){
                             tryCatch(file.path(execPath,gsub(sprintf("\\.%s.*",y),
                                            sprintf("\\.%s",y),
                                            grep(sprintf("\\.%s",y),readLines(x),value=TRUE,perl=TRUE))), error = function(e){
                                        stop(sprintf("Cannot find %s",x))
                             })
                        },
                        getQueue(depTree,startPoint)[-1],
                        init=iniName)
    if(startRow$mod > 0){
        tryCatch(
         gsub(sprintf("\\.%s.*", startExt),
              sprintf("\\.%s", startExt),
              grep(sprintf("\\.%s",startExt),readLines(parentFile),value=TRUE,perl=TRUE))[startRow$mod]
        ,error = function(e){stop(sprintf("Cannot read %s",parentFile))})
    } else {
        res <- tryCatch(
         gsub(sprintf("\\.%s.*", startExt),
                     sprintf("\\.%s",startExt),
                     grep(sprintf("\\.%s",startExt),readLines(parentFile),value=TRUE, perl=TRUE))
        ,error = function(e){stop(sprintf("Cannot read %s", parentFile))})
        unique(gsub(".*\\t","",res))
    }
}



#' getFilesFromIni
#'
#' This function reads the ini file and gives yout back the path of all file involved in model run
#' @param iniName The name of the ini file
#' @param depTree The file dependency defining dataframe. At default it is:  options("AgroMo_depTree")[[1]]
#' @export

getFilesFromIni <- function(iniName, execPath = "./", depTree=options("AgroMo_depTree")[[1]]){
    res <- lapply(depTree$name,function(x){
                      tryCatch(getFilePath(iniName,x,execPath,depTree), error = function(e){
                            return(NA);
                     })
        })
    names(res) <- depTree$name
    res
}

flatMuso <- function(iniName, execPath="./",
                     depTree=options("AgroMo_depTree")[[1]],
                     directory="flatdir",
                     runType=1, timeDefine=NULL){

    dir.create(directory, showWarnings=FALSE, recursive=TRUE)
    files <- getFilesFromIni(iniName,execPath,depTree)
    files <- sapply(unlist(files)[!is.na(files)], function(x){ifelse(isRelative(x),file.path(execPath,x),x)})
    file.copy(unlist(files), directory, overwrite=TRUE)
    file.copy(iniName, directory, overwrite=TRUE)

    filesByName <- getFilesFromIni(iniName, execPath, depTree)
    filesWithoutIni <- seq_along(filesByName)
    filesByName["iniFile"] <- iniName
    for(i in filesWithoutIni){
        parentFile<- file.path(directory,basename(unlist(filesByName[getParent(names(filesByName)[i],depTree)])))
        fileLines <- readLines(parentFile)

        sapply(filesByName[[i]],function(origname){
            if(!is.na(origname)){
                fileLines <<- gsub(origname, basename(origname), fileLines, fixed=TRUE)
            }
        })

        if(!is.na(filesByName[[i]][1])){
            writeLines(fileLines, parentFile)
        }

    }

    iniLines <- readLines(file.path(directory, basename(iniName)))

    outPlace <- grep("OUTPUT_CONTROL", iniLines, perl=TRUE)+1
    iniLines[outPlace] <-  paste0(filename_sans_ext(basename(iniName)),"_",basename(strsplit(iniLines[outPlace], split = "\\s+")[[1]][1]))
    iniLines[outPlace + 1] <- runType

    if(!is.null(timeDefine)){
        timePlace <- grep("TIME_DEFINE", iniLines, perl=TRUE) + 1
        iniLines[timePlace] <- timeDefine[2] 
        iniLines[timePlace + 1] <- timeDefine[1]
    }

    writeLines(iniLines, file.path(directory, basename(iniName)))    
}


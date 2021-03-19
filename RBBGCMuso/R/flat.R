getQueue <- function(depTree=options("RMuso_depTree")[[1]], startPoint){
    
    if(length(startPoint) == 0){
        return(c())
    }
    parent <- depTree[depTree[,"name"] == startPoint,"parent"]
    c(getQueue(depTree, depTree[depTree[,"child"] == depTree[depTree[,"name"] == startPoint,"parent"],"name"]),parent)
}

isRelative <- function(path){
   substr(path,1,1) != '/'
}

#' getFilePath
#'
#' This function reads the ini file and for a chosen fileType it gives you the filePath
#' @param iniName The name of the ini file
#' @param filetype The type of the choosen file. For options see options("RMuso_depTree")[[1]]$name
#' @param depTree The file dependency defining dataframe. At default it is:  options("RMuso_depTree")[[1]]
#' @export

getFilePath <- function(iniName, fileType, execPath = "./", depTree=options("RMuso_depTree")[[1]]){
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
#' @param depTree The file dependency defining dataframe. At default it is:  options("RMuso_depTree")[[1]]
#' @export

getFilesFromIni <- function(iniName, execPath = "./", depTree=options("RMuso_depTree")[[1]]){
    res <- lapply(depTree$name,function(x){
                      tryCatch(getFilePath(iniName,x,execPath,depTree), error = function(e){
                            return(NA);
                     })
        })
    names(res) <- depTree$name
    res
}

#' flatMuso
#'
#' This function reads the ini file and creates a directory (named after the directory argument) with all the files the modell uses with this file. the directory will be flat.
#' @param iniName The name of the ini file
#' @param depTree The file dependency defining dataframe. At default it is:  options("RMuso_depTree")[[1]]
#' @param directory The destination directory for flattening. At default it will be flatdir 
#' @export

flatMuso <- function(iniName, execPath="./", depTree=options("RMuso_depTree")[[1]], directory="flatdir", d=TRUE,outE=TRUE){
    dir.create(directory, showWarnings=FALSE, recursive = TRUE)
    files <- getFilesFromIni(iniName,execPath,depTree)
    files <- sapply(unlist(files)[!is.na(files)], function(x){ifelse(isRelative(x),file.path(execPath,x),x)})
    file.copy(unlist(files), directory, overwrite=TRUE)
    file.copy(iniName, directory, overwrite=TRUE)

    filesByName <- getFilesFromIni(iniName, execPath, depTree)
    for(i in seq_along(filesByName)){
        fileLines <- readLines(file.path(directory,list.files(directory, pattern = sprintf("*\\.%s", depTree$parent[i])))[1])

        sapply(filesByName[[i]],function(origname){
            if(!is.na(origname)){
                fileLines <<- gsub(origname, basename(origname), fileLines, fixed=TRUE)
            }
        })

        if(!is.na(filesByName[[i]][1])){
            writeLines(fileLines, file.path(directory,list.files(directory, pattern = sprintf("*\\.%s", depTree$parent[i])))[1])
        }

    }

    iniLines <- readLines(file.path(directory, basename(iniName)))
    outPlace <- grep("OUTPUT_CONTROL", iniLines, perl=TRUE)+1
    if(outE){
        iniLines[outPlace] <- tools::file_path_sans_ext(basename(iniName))
    } else {
        iniLines[outPlace] <-  basename(strsplit(iniLines[outPlace], split = "\\s+")[[1]][1])
    }
    if(d){
        iniLines[outPlace + 1] <- 1
    }
    writeLines(iniLines, file.path(directory, basename(iniName)))    
}

#' checkFileSystem
#'
#' This function checks the MuSo file system, if it is correct
#' @param iniName The name of the ini file
#' @param depTree The file dependency defining dataframe. At default it is:  options("RMuso_depTree")[[1]]
#' @export

checkFileSystem <- function(iniName,root = ".", depTree = options("RMuso_depTree")[[1]]){
    recoverAfterEval({
        setwd(root)
        fileNames <- getFilesFromIni(iniName, depTree)
        if(is.na(fileNames$management)){
            fileNames[getLeafs("management")] <- NA
        }
        fileNames <- fileNames[!is.na(fileNames)]
        errorFiles <- fileNames[!file.exists(unlist(fileNames))]
    })
    return(errorFiles)
}

recoverAfterEval <- function(expr){
    wd <- getwd()
    tryCatch({
        eval(expr)
        setwd(wd) 
    }, error=function(e){
            setwd(wd)
            stop(e)
    })
}

getLeafs <- function(name, depTree=options("RMuso_depTree")[[1]]){

    if(length(name) == 0){
        return(NULL)
    }

    if(name[1] == "ini"){
        return(getLeafs(depTree$name))
    }

    pname <- depTree[ depTree[,"name"] == name[1] , "child"]
    children <- depTree[depTree[,"parent"] == pname,"child"]  

    if(length(children)==0){
        if(length(name) == 1){
            return(NULL)
        } else{
            apname <- depTree[ depTree[,"name"] == name[2] , "child"]
            achildren <- depTree[depTree[,"parent"] == apname,"child"]  
            if(length(achildren)!=0){
                return(c(name[1],name[2],getLeafs(name[-1])))
            } else{
                return(c(name[1], getLeafs(name[-1])))
            }

        }
    }
    
    childrenLogic <-depTree[,"child"] %in% children 
    parentLogic <- depTree[,"parent"] ==pname
    res <- depTree[childrenLogic & parentLogic, "name"]
    getChildelem <- depTree[depTree[,"child"] == intersect(depTree[,"parent"], children), "name"]
    unique(c(res,getLeafs(getChildelem)))
}

getParent <- function (name, depTree=options("RMuso_depTree")[[1]]) {
    parentExt <- depTree[depTree$name == name,"parent"]
    # if(length(parentExt) == 0){
    #     browser()
    # }
    if(parentExt == "ini"){
        return("iniFile")
    }

    depTree[depTree[,"child"] == parentExt,"name"]
}



getFilePath2 <- function(iniName, fileType, depTree=options("RMuso_depTree")[[1]]){
    if(!file.exists(iniName) || dir.exists(iniName)){
        stop(sprintf("Cannot find iniFile: %s", iniName))
    }

    startPoint <- fileType
    startRow <- depTree[depTree[,"name"] == startPoint,]
    startExt <- startRow$child

    parentFile <- Reduce(function(x,y){
                             tryCatch(gsub(sprintf("\\.%s.*",y),
                                   sprintf("\\.%s",y),
                                   grep(sprintf("\\.%s",y),readLines(x),value=TRUE,perl=TRUE)), error = function(e){
                                        stop(sprintf("Cannot find %s",x))
                             })
                        },
                        getQueue(depTree,startPoint)[-1],
                        init=iniName)
    res <- list()
    res["parent"] <- parentFile
    if(startRow$mod > 0){
    res["children"] <-    tryCatch(
         gsub(sprintf("\\.%s.*", startExt),
              sprintf("\\.%s", startExt),
              grep(sprintf("\\.%s",startExt),readLines(parentFile),value=TRUE,perl=TRUE))[startRow$mod]
        ,error = function(e){stop(sprintf("Cannot read %s",parentFile))})

    } else {
         rows <- tryCatch(
        gsub(sprintf("\\.%s.*", startExt),
                     sprintf("\\.%s",startExt),
                     grep(sprintf("\\.%s",startExt),readLines(parentFile),value=TRUE, perl=TRUE))
         
        ,error = function(e){stop(sprintf("Cannot read %s", parentFile))})
        unique(gsub(".*\\t","",res))
        res["children"] <- unique(gsub(".*\\s+(.*\\.epc)","\\1",rows))
    }
    res
}

getFilesFromIni2 <- function(iniName, depTree=options("RMuso_depTree")[[1]]){
    res <- lapply(depTree$name,function(x){
                      tryCatch(getFilePath2(iniName,x,depTree), error = function(e){
                            return(NA);
                     })
        })
    names(res) <- depTree$name
    res
}

checkFileSystemForNotif <- function(iniName,root = ".", depTree = options("RMuso_depTree")[[1]]){
    recoverAfterEval({
        setwd(root)
        fileNames <- suppressWarnings(getFilesFromIni2(iniName, depTree))
        if(is.atomic(fileNames$management)){
            fileNames[getLeafs("management")] <- NA
        }

        hasparent   <- sapply(fileNames, function(x){
                                  !is.atomic(x)
                     })
        notNA <- ! sapply(fileNames[hasparent], function(x) {is.na(x$children)})
        errorIndex <- ! sapply(fileNames[hasparent & notNA], function(x) file.exists(x$children))

    })
    return(fileNames[hasparent & notNA][errorIndex])
}


